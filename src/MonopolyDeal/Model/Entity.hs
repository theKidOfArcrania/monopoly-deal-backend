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

import ClassyPrelude.Yesod  
  (Generic, Entity, Key, Maybe(..), BackendKey, SqlBackend, Int64
  , pure, mempty, ($))
import Control.Lens         ((&), (?~), (.~))
import Data.Proxy           (Proxy(..))
import Data.Swagger         
  (NamedSchema(..), SwaggerType(..), ToSchema(..), Referenced(..)
  , ToParamSchema(..), allOf, declareSchemaRef, type_, properties)

instance ToSchema (BackendKey SqlBackend) where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Int64)

instance ToParamSchema (BackendKey SqlBackend) where
  toParamSchema _ = toParamSchema (Proxy :: Proxy Int64)

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

