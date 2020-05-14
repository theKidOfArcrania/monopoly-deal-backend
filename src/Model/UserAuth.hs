{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Model.UserAuth where

import GHC.Generics
import Data.Aeson.Types
import Data.Text

data T = T { userName :: Text, password :: Text }
  deriving (Show, Read, Eq, Generic)
instance ToJSON T
instance FromJSON T
