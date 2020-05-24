{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
module MonopolyDeal.VersionMgmt where

import Prelude()
import ClassyPrelude.Yesod
import Data.Aeson
import Data.Char            (isDigit)
import Data.FileEmbed       (embedFile)
import Text.Read
import System.Directory
import Language.Haskell.TH.Syntax
import qualified Data.ByteString.Char8 as BS8

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
  deriving (Eq, Ord, Hashable, Generic, ToJSON, FromJSON, ToJSONKey, FromJSONKey)
  
instance Read Version where
  readsPrec _ inp = maybeToList $ parseVersion "v" "." inp
instance Show Version where
  show = showVersion "v" "."
instance PathPiece Version where
  toPathPiece = pack . show
  fromPathPiece = parseVersionSfx "v" "." "" . unpack

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

swaggerDir :: FilePath
swaggerDir = "config/swagger/"

swaggerPath :: Version -> FilePath
swaggerPath ver = swaggerDir ++ show ver ++ ".json"

swaggerDep :: FilePath
swaggerDep = swaggerDir ++ ".update"

-- Queries all the available versions
queryVersions :: IO [Version]
queryVersions = do
  lst <- listDirectory swaggerDir
  pure $ reverse $ sort $ concat $
    map (maybeToList . parseVersionSfx "v" "." ".json") lst

-- This forces a dependency nudge on all files that depend on the versioning.
-- Call this function whenever we create a new file to the config/swagger 
-- directory.
updateDeps :: IO ()
updateDeps = do
  ver <- nextIncrement
  writeFile swaggerDep $ BS8.pack $ show $ ver

-- Obtains the next available incremental version number
nextIncrement :: IO Int
nextIncrement = ((+)1 . length) <$> queryVersions

-- Obtains the next available version number
nextVersion :: IO Version
nextVersion = Version curMajor curMinor <$> nextIncrement

-- Obtains all the swagger files loaded as a list of content data. This is then
-- stored at compile-time! This will also automatically add a dependency to any
-- changes made to that swagger directory
mkSwagger :: Q [Dec]
mkSwagger = do
  vers <- runIO queryVersions 
  files <- mapM go vers

  -- Add swagger dependency on the number of files in there.
  runIO updateDeps
  qAddDependentFile swaggerDep

  pure [ SigD filesName (ListT `AppT` tupleT [ConT ''Version, ConT ''Content])
       , assignD filesName $ ListE files
--       , enumD eVersName (map versName vers)
--          [''Read, ''Eq, ''Show, ''Generic, ''ToJSON, ''FromJSON
--          , ''ToJSONKey, ''FromJSONKey, ''Hashable]
       , SigD versName (ListT `AppT` ConT ''Version)
       , assignD versName $ ListE $ map versE vers]
 where go ver = do
         f <- embedFile (swaggerPath ver)
         pure $ TupE [versE ver, VarE 'toContent `AppE` f]
       filesName = mkName "swaggerFiles"
       versName = mkName "versions"
       versE v = foldl' AppE (ConE 'Version) $ map 
         (LitE . IntegerL . fromIntegral . app v) [major, minor, incremental]
       app e fn = fn e

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
