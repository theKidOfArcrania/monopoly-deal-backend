{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Data.Swagger.Ext
  ( ToSchema1, OuterBot, declareOuterNamedSchema, liftInnerSchema
  , DeclS
  , module Data.Swagger.Ext.Desc
  , module Data.Swagger.Ext.Tags
  ) where

import ClassyPrelude.Yesod             (when)
import Data.Swagger
  ( Definitions, Schema, NamedSchema(..), ToSchema, Referenced(..)
  , Reference(..), SwaggerType(..)
  , declareSchemaRef, declareNamedSchema, allOf, type_)
import Data.Swagger.Declare            (Declare, looks, declare)
import Data.Proxy                      (Proxy(..))
import Control.Lens                    ((?~), (&))
import Data.HashMap.Strict.InsOrd      (member)
import Data.Swagger.Ext.Desc
import Data.Swagger.Ext.Tags

type DeclS a = Declare (Definitions Schema) a

class ToSchema1 (outer :: * -> *) where
  -- newtype OuterBot outer = {__container :: outer inner}
  data OuterBot outer

  -- Declares the schema of the outer "wrapper" container
  declareOuterNamedSchema :: Proxy (OuterBot outer) -> DeclS (NamedSchema)
  default declareOuterNamedSchema :: (ToSchema (OuterBot outer)) =>
    Proxy (OuterBot outer) -> DeclS (NamedSchema)
  declareOuterNamedSchema = declareNamedSchema

  -- Lifts the inner schema "elements" such that the type product of this schema
  -- and the outer "wrapper container given by declareOuterNamedSchema will
  -- result in the specialized schema "shape" when given a generic of element
  -- type "a" 
  liftInnerSchema :: Proxy (outer inner) -> Referenced Schema ->
    DeclS [Referenced Schema]

-- Defines a ToSchema instance in terms of ToSchema1. Do not define ToSchema
-- instance for generic types, instead use ToSchema1.
instance (ToSchema1 o, ToSchema i) => ToSchema (o i) where
  declareNamedSchema p = do
    wrapper <- declareOuterNamedSchema (Proxy :: Proxy (OuterBot o))
    wrapperRef <- declareRef wrapper
    inner <- declareSchemaRef (Proxy :: Proxy i)
    lifted <- liftInnerSchema p inner
    return $ NamedSchema Nothing $ mempty
      & type_ ?~ SwaggerObject
      & Data.Swagger.allOf ?~ wrapperRef : lifted

-- TODO: maybe check to make sure this works on recursive types??
-- Declares the schema if it does not exist and returns the reference to 
-- this schema.
declareRef :: NamedSchema -> DeclS (Referenced Schema)
declareRef nschema = do
  case nschema of
    NamedSchema (Just name) schema -> do
      known <- looks (member name)
      when (not known) $ declare [(name, schema)]
      return $ Ref (Reference name)
    NamedSchema Nothing schema -> return $ Inline schema
