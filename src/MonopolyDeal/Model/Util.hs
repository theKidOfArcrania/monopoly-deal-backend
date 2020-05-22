{-# LANGUAGE TemplateHaskellQuotes #-}
module MonopolyDeal.Model.Util 
  ( entDSOptions, entDAOptions, deriveAll
  , declareEndpoint, declareSubOperationsFor) where

import Prelude ()
import Control.Lens
import ClassyPrelude.Yesod
import Language.Haskell.TH.Syntax
import qualified Data.Proxy       as P
import qualified Data.Char        as DC
import qualified Data.Aeson       as DA
import qualified Data.Aeson.TH    as DA
import qualified Data.Swagger     as DS
import qualified Servant.Swagger  as DS

maybeStripPrefix :: String -> String -> String
maybeStripPrefix pref sym = case stripPrefix pref sym of
                              Just s -> s
                              Nothing -> sym

lcaseFirst :: String -> String
lcaseFirst str = case str of
               x : xs -> (DC.toLower x) : xs
               []     -> []

recordStrip :: String -> String -> String
recordStrip pref = lcaseFirst . maybeStripPrefix pref 

entDSOptions :: String -> DS.SchemaOptions
entDSOptions pref =
  DS.defaultSchemaOptions 
    { DS.fieldLabelModifier = recordStrip pref
    , DS.unwrapUnaryRecords = True }

entDAOptions :: String -> DA.Options
entDAOptions pref =
  DA.defaultOptions 
    { DA.fieldLabelModifier = recordStrip pref
    , DA.unwrapUnaryRecords = True }

deriveAll :: Name -> Q [Dec]
deriveAll cls = do
  nm1 <- newName "x"
  jsons <- DA.deriveJSON (entDAOptions pref) cls
  pure $ schemaInst nm1 : jsons
 where pref = lcaseFirst $ nameBase cls
       schOptsE = VarE 'entDSOptions `AppE` LitE (StringL pref)
       schemaInst nm1 = typeInstanceD ''DS.ToSchema cls 
        [ assignD 'DS.declareNamedSchema $ LamE [VarP nm1]
          (VarE 'DS.genericDeclareNamedSchema `AppE` schOptsE `AppE` VarE nm1)
        ]
       
-- Declares an endpoint (the actual API type and a proxy object pointing to this
-- API endpoint). The API type is prefixed with "API" and the proxy object is
-- prefixed with "p", both followed by a user provided string. You also must
-- pass the API type as a TH-quote
declareEndpoint :: String -> Q Type -> Q [Dec]
declareEndpoint name qapi = do
  api <- qapi
  pure 
    [ TySynD apiName [] api
    , SigD proxName (ConT ''P.Proxy `AppT` ConT apiName)
    , assignD proxName (ConE 'P.Proxy)
    ]
 where apiName = mkName ("API" ++ name)
       proxName = mkName ("p" ++ name)

declareSubOperationsFor :: Name -> [String] -> Q [Dec]
declareSubOperationsFor pAll subs = do
  let prod sub =
                [ SigD soName (ConT ''Traversal' `AppT` ConT ''DS.Swagger 
                  `AppT` ConT ''DS.Operation)
                , assignD soName (VarE 'DS.subOperations `AppE` VarE proxName 
                  `AppE` VarE pAll)
                ]
                 where soName = mkName ("so" ++ sub)
                       proxName = mkName ("p" ++ sub)
  pure $ concat $ map prod subs

assignD :: Name -> Exp -> Dec
assignD var expr = ValD (VarP var) (NormalB expr) []

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

typeInstanceD :: Name -> Name -> [Dec] -> Dec
typeInstanceD clazz inst = instanceD [] (ConT clazz `AppT` ConT inst)

