{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
module MonopolyDeal.VersionMgmt where

import Prelude()
import ClassyPrelude.Yesod
import Data.Aeson
import Data.Char            (isDigit)
import Data.FileEmbed       (embedFile)
import Text.Read
import System.Directory
import Language.Haskell.TH.Syntax

-- TODO: update the major/minor versions whenever the Swagger API has
-- significantly changed. 
curMajor :: Int
curMajor = 0

curMinor :: Int
curMinor = 1

verParam :: Text
verParam = "ver"

readInt :: String -> [(Int, String)]
readInt s = case reads $ takeWhile isDigit s of
              [(num, "")] -> [(num, dropWhile isDigit s)]
              _ -> []

data Version = Version {major :: Int, minor :: Int, incremental :: Int}
instance Read Version where
  readsPrec _ inp = maybeToList $ parseVersion "v" "." inp
instance Show Version where
  show = showVersion "v" "."

parseVersion :: String -> String -> String -> Maybe (Version, String)
parseVersion pref sep inp = do 
  inp0 <- stripPrefix pref inp
  (_major, inp1) <- listToMaybe $ readInt inp0
  inp2 <- stripPrefix sep inp1
  (_minor, inp3) <- listToMaybe $ readInt inp2
  inp4 <- stripPrefix sep inp3
  (incr, inp5) <- listToMaybe $ readInt inp4
  pure (Version _major _minor incr, inp5)

parseVersionSfx :: String -> String -> String -> String -> Maybe (Version)
parseVersionSfx pref sep sfx inp = 
  case parseVersion pref sep inp of
    Just (v, s2) -> if sfx == s2 then Just v
                                 else Nothing
    _ -> Nothing

showVersion :: String -> String -> Version -> String
showVersion pref sep vers =
  pref <> (show $ major vers) <> sep <> (show $ minor vers) 
    <> sep <> (show $ incremental vers)

versName :: Version -> Name
versName = mkName . showVersion "V" "_"

swaggerDir :: FilePath
swaggerDir = "config/swagger/"

swaggerPath :: Version -> FilePath
swaggerPath ver = swaggerDir ++ show ver ++ ".json"

-- Queries all the available versions
queryVersions :: IO [Version]
queryVersions = do
  lst <- listDirectory swaggerDir
  pure $ concat $ map (maybeToList . parseVersionSfx "v" "." ".json") lst

-- Obtains the next available incremental version number
nextIncrement :: IO Int
nextIncrement = map length queryVersions

-- Obtains the next available version number
nextVersion :: IO Version
nextVersion = map (Version curMajor curMinor) nextIncrement

-- Obtains all the swagger files loaded as a list of content data. This is then
-- stored at compile-time!
mkSwagger :: Q [Dec]
mkSwagger = do
  vers <- runIO queryVersions 
  files <- mapM go vers
  pure [ SigD filesName (ListT `AppT` tupleT [ConT eVersName, ConT ''Content])
       , assignD filesName $ ListE files
       , enumD eVersName (map versName vers)
          [''Read, ''Eq, ''Show, ''Generic, ''ToJSON, ''FromJSON
          , ''ToJSONKey, ''FromJSONKey, ''Hashable]
       , SigD versName2 (ListT `AppT` ConT eVersName)
       , assignD versName2 $ ListE $ map (ConE . versName) vers]
 where go ver = do
         f <- embedFile (swaggerPath ver)
         pure $ TupE [ConE $ versName ver, VarE 'toContent `AppE` f]
       filesName = mkName "swaggerFiles"
       versName2 = mkName "versions"
       eVersName = mkName "SwaggerVersion"

assignD :: Name -> Exp -> Dec
assignD var expr = ValD (VarP var) (NormalB expr) []

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

tupleT :: [Type] -> Type
tupleT typs = foldl' AppT (TupleT $ length typs) typs

typeInstanceD :: Name -> Name -> [Dec] -> Dec
typeInstanceD clazz inst = instanceD [] (ConT clazz `AppT` ConT inst)

enumD :: Name -> [Name] -> [Name] -> Dec
enumD name csts dv = DataD [] name [] Nothing (map ((flip NormalC) []) csts)
  [DerivClause Nothing $ map ConT dv]
