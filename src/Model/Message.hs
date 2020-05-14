{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Message where

import Data.Text
import GHC.Generics
import Data.Aeson.Types
import Network.HTTP.Types 

msg200 :: ToJSON payload => Text -> payload -> Message
msg200 = msg status200
msg400 :: ToJSON payload => Text -> payload -> Message
msg400 = msg status400
msg401 :: ToJSON payload => Text -> payload -> Message
msg401 = msg status401
msg403 :: ToJSON payload => Text -> payload -> Message
msg403 = msg status403
msg404 :: ToJSON payload => Text -> payload -> Message
msg404 = msg status404
msg405 :: ToJSON payload => Text -> payload -> Message
msg405 = msg status405
msg409 :: ToJSON payload => Text -> payload -> Message
msg409 = msg status409
msg500 :: ToJSON payload => Text -> payload -> Message
msg500 = msg status500

msg :: ToJSON payload => Status -> Text -> payload -> Message
msg sta m = Message sta m . toJSON

data Message = Message {status :: Status, message :: Text, payload :: Value}
  deriving (Show, Eq, Generic)
instance ToJSON Message
instance FromJSON Message

instance ToJSON Status where 
  toJSON s = toJSON $ statusCode s

instance FromJSON Status where 
  parseJSON val = do
    code <- parseJSON val
    return $ mkStatus code ""
