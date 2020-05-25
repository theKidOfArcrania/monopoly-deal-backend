{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Swagger.Ext.Tags where

import Control.Lens    ((&))
import Data.Text       (pack)
import Data.Typeable   (Typeable)
import Data.Proxy      (Proxy(..))
import Data.Swagger    (Tag(..), applyTags)
import GHC.TypeLits    (Symbol, KnownSymbol, symbolVal)
import Servant.Swagger (HasSwagger(..))
import Servant.API     ((:>))

data SwaggerTag (name :: Symbol) (description :: Symbol)
  deriving (Typeable)

-- instance HasServer api ctx =>
--          HasServer (SwaggerTag name description
--                     :> api) ctx where
--   type ServerT (SwaggerTag name description
--                 :> api) m = ServerT api m
--   route _ = route (Proxy api)
--   hoistServerWithContext _ = hoistServerWithContext (Proxy api)
-- 
-- instance HasClient m api =>
--          HasClient m (SwaggerTag name description
--                       :> api) where
--   type Client m (SwaggerTag name description
--                  :> api) = Client m api
--   clientWithRoute _ _ = clientWithRoute (Proxy m) (Proxy api)
--   hoistClientMonad pm _ = hoistClientMonad pm (Proxy api)

instance (HasSwagger api, KnownSymbol name, KnownSymbol description) =>
         HasSwagger (SwaggerTag name description :> api) where
  toSwagger _ =
    let tag =
          Tag
            (pack $ symbolVal (Proxy :: Proxy name)) -- tag name
            ((\case
                "" -> Nothing
                t -> Just t) .
             pack $
             symbolVal (Proxy :: Proxy description)) -- tag descrption
            Nothing
     in toSwagger (Proxy :: Proxy api) & applyTags [tag]
