{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
module MonopolyDeal.Model.Util 
  ( entDSOptions, entDAOptions, deriveAll, deriveAllPref, fixComplexUnion
  , deriveAllPrefUnion) where

import Prelude ()
import Control.Lens
import ClassyPrelude.Yesod
import Language.Haskell.TH.Syntax
import Data.Aeson       as DA
import Data.Aeson.TH    as DA
import Data.Swagger     as DS
import qualified Data.Char        as DC
import qualified Data.HashMap.Strict.InsOrd as InsOrd

maybeStripPrefix :: String -> String -> String
maybeStripPrefix pref sym = case stripPrefix pref sym of
                              Just s -> s
                              Nothing -> sym

lcaseFirst :: String -> String
lcaseFirst str = case str of
               x : xs -> (DC.toLower x) : xs
               []     -> []
ucaseFirst :: String -> String
ucaseFirst str = case str of
               x : xs -> (DC.toUpper x) : xs
               []     -> []

recordStrip :: String -> String -> String
recordStrip pref = lcaseFirst . maybeStripPrefix pref 

entDSOptions :: String -> DS.SchemaOptions
entDSOptions pref = DS.fromAesonOptions $ entDAOptions pref 

entDAOptions :: String -> DA.Options
entDAOptions pref =
  DA.defaultOptions 
    { DA.fieldLabelModifier = recordStrip $ lcaseFirst pref
    , DA.constructorTagModifier = recordStrip $ ucaseFirst pref
    , DA.unwrapUnaryRecords = True }

deriveAll :: Name -> Q [Dec]
deriveAll cls = deriveAllPref cls $ nameBase cls

deriveAllPref :: Name -> String -> Q [Dec]
deriveAllPref cls pref = do
  nm1 <- newName "x"
  jsons <- DA.deriveJSON (entDAOptions pref) cls
  pure $ schemaInst nm1 : jsons
 where schOptsE = VarE 'entDSOptions `AppE` LitE (StringL pref)
       schemaInst nm1 = typeInstanceD ''DS.ToSchema cls 
        [ assignD 'DS.declareNamedSchema $ LamE [VarP nm1]
          (VarE 'DS.genericDeclareNamedSchema `AppE` schOptsE `AppE` VarE nm1)
        ]
       
deriveAllPrefUnion :: Name -> String -> Q [Dec]
deriveAllPrefUnion cls pref = do
  nm1 <- newName "x"
  jsons <- DA.deriveJSON (entDAOptions pref) cls
  pure $ schemaInst nm1 : jsons
 where schOptsE = VarE 'entDSOptions `AppE` LitE (StringL pref)
       schemaInst nm1 = typeInstanceD ''DS.ToSchema cls 
        [ assignD 'DS.declareNamedSchema $ LamE [VarP nm1]
          ((VarE 'fmap `AppE` VarE 'fixComplexUnion) 
            `AppE` (VarE 'DS.genericDeclareNamedSchema 
              `AppE` schOptsE `AppE` VarE nm1))
        ]

fixComplexUnion :: NamedSchema -> NamedSchema
fixComplexUnion nsch = 
  nsch & over (schema.properties) (InsOrd.insert "tag" $ DS.Inline (mempty
    & type_       ?~ SwaggerString
    & description ?~ "Denotes the union type of this object"
    & format      ?~ "enum"
    & enum_       ?~ map String (InsOrd.keys $ nsch ^. schema.properties)))

assignD :: Name -> Exp -> Dec
assignD var expr = ValD (VarP var) (NormalB expr) []

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

typeInstanceD :: Name -> Name -> [Dec] -> Dec
typeInstanceD clazz inst = instanceD [] (ConT clazz `AppT` ConT inst)

