{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Data.Docs.DocGen where

import ClassyPrelude.Yesod 
  ( BackendKey, MkPersistSettings, EntityDef, FieldType(..)
  , entityAttrs, entityHaskell, entityFields, fieldType, fieldHaskell
  , mpsBackend, unHaskellName
  , String, Text, Int64, Maybe(..)
  , concat, find, filter, id, isJust, map, mapM, return, unpack
  , (.), ($), (==), (<>)
  ) 
import Control.Lens ((&), (.~))
import Data.Aeson
import Data.Swagger
import Data.Swagger.Lens
import GHC.Exts (fromList)

import Data.Proxy
import qualified Data.Text

import Language.Haskell.TH.Syntax

--Text (Referenced Schema)

data DocModel = DocModel 
  { docName :: Text
  }

entName :: EntityDef -> String
entName = unpack . unHaskellName . entityHaskell

mkModels :: MkPersistSettings -> Text -> [EntityDef] -> Q [Dec]
mkModels settings var ents = do
  modelInsts <- mapM (mkSchema settings) models
  return $ docDeclType : docDecl : concat modelInsts
 where models = (filter $ isJust . find ((==) "doc") . entityAttrs) ents
       modelExprs = map (\ent ->
         (ConE 'DocModel `AppE` (LitE $ StringL $ entName $ ent))) models
       -- The declaration for the list of DocModels
       varN = mkName $ unpack var
       docDeclType = SigD varN $ AppT ListT $ ConT ''DocModel
       docDecl = assignD varN $ ListE modelExprs

ftToType :: FieldType -> Type
ftToType (FTTypeCon Nothing t) = ConT $ mkName $ unpack t
-- This type is generated from the Quasi-Quoter.
-- Adding this special case avoids users needing to import Data.Int
ftToType (FTTypeCon (Just "Data.Int") "Int64") = ConT ''Int64
ftToType (FTTypeCon (Just m) t) = ConT $ mkName $ unpack $ concat [m, ".", t]
ftToType (FTApp x y) = ftToType x `AppT` ftToType y
ftToType (FTList x) = ListT `AppT` ftToType x

mkSchema :: MkPersistSettings -> EntityDef -> Q [Dec]
mkSchema settings ent = do
  body <- [|return $ NamedSchema (Just $ Data.Text.pack __entname) $ mempty
        & Data.Swagger.Lens.type_ .~ (Just Data.Swagger.SwaggerObject)
        & Data.Swagger.Lens.properties .~ (GHC.Exts.fromList $ zip __names __types)
        & Data.Swagger.Lens.required .~ __names |]
  return $ 
    [ typeInstanceD ''ToSchema (mkName $ entName ent) 
      [ assignD 'declareNamedSchema $ LamE [WildP] $ DoE 
        [ LetS 
          [ assignD (mkName "__names") $ (VarE 'map `AppE` VarE 'Data.Text.pack 
            `AppE` (ListE $ map (LitE . StringL . unpack . unHaskellName .
              fieldHaskell) ents))
          , assignD (mkName "__entname") $ LitE $ StringL $ unpack $ entName ent
          ]
        , BindS (VarP $ mkName "__types") (VarE 'mapM `AppE` VarE 'id 
          `AppE` (ListE $ map (AppE (VarE 'declareSchemaRef) . (SigE proxyE) . proxyOf . ftToType . fieldType) ents))
        , NoBindS body
        ]
      ]
    , typeInstanceD ''ToSchema idName 
      [ assignD 'declareNamedSchema $ LamE [WildP] 
        (VarE 'declareNamedSchema `AppE` (proxyE `SigE` proxyOf backendkeyT))
      ]
    , typeInstanceD ''ToParamSchema idName
      [ assignD 'toParamSchema $ LamE [WildP] 
        (VarE 'toParamSchema `AppE` (proxyE `SigE` proxyOf backendkeyT))
      ]
    ]
 where ents = entityFields ent
       proxyE = ConE 'Proxy
       proxyOf = AppT $ ConT ''Proxy
       backendkeyT = ConT ''BackendKey `AppT` mpsBackend settings
       idName = mkName $ entName ent <> "Id"
       

-- A normal assignment with no where clause
assignD :: Name -> Exp -> Dec
assignD var exp = ValD (VarP var) (NormalB exp) []

instanceD :: Cxt -> Type -> [Dec] -> Dec
instanceD = InstanceD Nothing

typeInstanceD :: Name -> Name -> [Dec] -> Dec
typeInstanceD clazz inst = instanceD [] (ConT clazz `AppT` ConT inst)
