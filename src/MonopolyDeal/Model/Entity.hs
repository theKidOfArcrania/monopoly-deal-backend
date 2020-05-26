{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MonopolyDeal.Model.Entity where 

import Prelude (reads)
import ClassyPrelude.Yesod  
  (Entity, Key, Maybe(..), BackendKey, SqlBackend, ToBackendKey(..)
  , fromMaybe, fst, map, mempty, pack, pure, show, uncons, unpack, ($), (.))
import Control.Lens         ((&), (?~), (.~))
import Data.Aeson           
  ( ToJSONKey, ToJSONKeyFunction(..), FromJSONKey, FromJSONKeyFunction(..)
  , toJSONKey, fromJSONKey)
import Data.Aeson.Types (contramapToJSONKeyFunction, mapFromJSONKeyFunction)
import Data.Aeson.Encoding (text)
import Data.Typeable        (Proxy(..), Typeable, showsTypeRep, typeRep)
import Data.Swagger         
  ( NamedSchema(..), SwaggerType(..), ToSchema(..), Referenced(..)
  , ToParamSchema(..), allOf, declareSchemaRef, type_, properties
  , description, paramSchemaToSchema)


instance ToSchema (BackendKey SqlBackend) where
  declareNamedSchema _ = pure $ NamedSchema Nothing $ mempty
    & type_ ?~ SwaggerInteger

instance ToParamSchema (BackendKey SqlBackend) where
  toParamSchema _ = mempty & type_ ?~ SwaggerInteger


-- Technically could be as generic as a generic read, but we might have
-- overlapping instances that way.
instance ToJSONKey (BackendKey SqlBackend) where
  toJSONKey = ToJSONKeyText f g
    where f = pack . show
          g = text . pack . show
instance FromJSONKey (BackendKey SqlBackend) where
  fromJSONKey = FromJSONKeyTextParser f
    where f = fromMaybe mempty . map (pure . fst . fst) . uncons . reads . unpack

instance (k ~ (Key a), ToSchema a, ToSchema k) =>
  ToSchema (Entity a) where
    declareNamedSchema _ = do
      idSchema <- declareSchemaRef (Proxy :: Proxy k)
      entSchema <- declareSchemaRef (Proxy :: Proxy a)
      pure $ NamedSchema Nothing $ mempty
        & type_ ?~ SwaggerObject
        & allOf ?~ 
          [ Inline (mempty
                      & type_ ?~ SwaggerObject
                      & properties .~ [("id", idSchema)])
          , entSchema
          ]

-- Schema and JSON instances for all keys
instance (ToParamSchema (Key record), Typeable record) =>
  ToSchema (Key record) where
    declareNamedSchema p = pure $ NamedSchema Nothing $ paramSchemaToSchema p
      & description ?~ pack (showsTypeRep recType " ID")
     where recType = typeRep (Proxy :: Proxy record) 
instance (ToBackendKey backend record, ToParamSchema (BackendKey backend)) =>
  ToParamSchema (Key record) where
    toParamSchema _ = toParamSchema (Proxy :: Proxy (BackendKey backend)) 
instance (ToBackendKey backend record, ToJSONKey (BackendKey backend)) =>
  ToJSONKey (Key record) where
    toJSONKey = contramapToJSONKeyFunction toBackendKey toJSONKey
instance (ToBackendKey backend record, FromJSONKey (BackendKey backend)) =>
  FromJSONKey (Key record) where
    fromJSONKey = mapFromJSONKeyFunction fromBackendKey fromJSONKey
