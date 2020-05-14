{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
module Model.NewUser where

import GHC.Generics
import Data.Aeson.Types
import Data.Text
import qualified Model as Model


data T = T { userName :: Text, password :: Text, email :: Text }
  deriving (Show, Read, Eq, Generic)
instance ToJSON T
instance FromJSON T

toUserEntity :: T -> Model.User
toUserEntity ent = Model.User
  { Model.userProfile     = Nothing
  , Model.userUserName    = userName ent
  , Model.userDisplayName = userName ent
  , Model.userEmail       = email ent
  , Model.userPassword    = password ent}
