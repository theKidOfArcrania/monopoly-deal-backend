{-# LANGUAGE TemplateHaskellQuotes #-}
module MonopolyDeal.Model.Util 
  (entDSOptions, entDAOptions, deriveAll) where

import Prelude ()
import ClassyPrelude.Yesod
import Language.Haskell.TH.Syntax
import qualified Data.Char        as DC
import qualified Data.Aeson       as DA
import qualified Data.Aeson.TH    as DA
import qualified Data.Swagger     as DS

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
  DS.defaultSchemaOptions {DS.fieldLabelModifier = recordStrip pref}

entDAOptions :: String -> DA.Options
entDAOptions pref =
  DA.defaultOptions {DA.fieldLabelModifier = recordStrip pref}

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
       
assignD :: Name -> Exp -> Dec
assignD var expr = ValD (VarP var) (NormalB expr) []

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

typeInstanceD :: Name -> Name -> [Dec] -> Dec
typeInstanceD clazz inst = instanceD [] (ConT clazz `AppT` ConT inst)

