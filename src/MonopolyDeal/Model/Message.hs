{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module MonopolyDeal.Model.Message where

import Data.Text
import GHC.Generics
import Data.Aeson.Types
import Network.HTTP.Types 

import Data.Proxy        (Proxy(..))
import Data.Swagger      
  (ToSchema, Referenced(..), SwaggerType(..), NamedSchema(..)
  , declareNamedSchema, genericDeclareNamedSchema, defaultSchemaOptions
  , type_, properties, minimum_, maximum_
  )
import Data.Swagger.Ext  (ToSchema1, OuterBot, liftInnerSchema)
import Control.Lens ((&), (?~), (.~))

msg200 :: Text -> Message payload
msg200 = _msg status200
msg400 :: Text -> Message payload
msg400 = _msg status400
msg401 :: Text -> Message payload
msg401 = _msg status401
msg403 :: Text -> Message payload
msg403 = _msg status403
msg404 :: Text -> Message payload
msg404 = _msg status404
msg405 :: Text -> Message payload
msg405 = _msg status405
msg409 :: Text -> Message payload
msg409 = _msg status409
msg500 :: Text -> Message payload
msg500 = _msg status500

msg200P :: Text -> payload -> Message payload
msg200P = _msgP status200
msg400P :: Text -> payload -> Message payload
msg400P = _msgP status400
msg401P :: Text -> payload -> Message payload
msg401P = _msgP status401
msg403P :: Text -> payload -> Message payload
msg403P = _msgP status403
msg404P :: Text -> payload -> Message payload
msg404P = _msgP status404
msg405P :: Text -> payload -> Message payload
msg405P = _msgP status405
msg409P :: Text -> payload -> Message payload
msg409P = _msgP status409
msg500P :: Text -> payload -> Message payload
msg500P = _msgP status500

_msg :: Status -> Text -> Message payload
_msg sta msg = Message sta msg Nothing
_msgP :: Status -> Text -> payload -> Message payload
_msgP sta msg pay = Message sta msg (Just pay)

data Message a = Message {status :: Status, message :: Text, payload :: Maybe a}
  deriving (Show, Eq, Generic1, Generic)

instance (ToJSON a) => ToJSON (Message a)
instance (FromJSON a) => FromJSON (Message a)
instance ToJSON1 Message
instance FromJSON1 Message

instance ToSchema1 Message where
  newtype OuterBot Message = MessageBot (Message ())
  liftInnerSchema _ rinner = return [Inline $ mempty
    & type_ ?~ SwaggerObject
    & properties .~ [("payload", rinner)]]
instance ToSchema (OuterBot Message) where 
  declareNamedSchema _ = 
    genericDeclareNamedSchema defaultSchemaOptions (Proxy :: Proxy (Message ()))

instance ToSchema (Status) where
  declareNamedSchema _ = pure $ NamedSchema Nothing mempty
    & type_ ?~ SwaggerInteger
    & minimum_ ?~ 0
    & maximum_ ?~ 599


instance ToJSON Status where 
  toJSON s = toJSON $ statusCode s

instance FromJSON Status where 
  parseJSON val = do
    code <- parseJSON val
    return $ mkStatus code ""


