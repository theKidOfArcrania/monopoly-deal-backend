{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric   #-}
module Model.Cards where

import Database.Persist.TH
import Data.Aeson.Types
import Prelude
import GHC.Generics

data CardType = CAction | CProperty | CCash
  deriving (Show, Read, Eq, Generic)
instance ToJSON CardType
instance FromJSON CardType

data CardLocation = LHand | LCashPile | LDraw | LDiscard | LPropStack
  deriving (Show, Read, Eq, Generic)
instance ToJSON CardLocation
instance FromJSON CardLocation

data ActionType = APayment | ARefutal | AGive | APlayCard
  deriving (Show, Read, Eq, Generic)
instance ToJSON ActionType
instance FromJSON ActionType

data TargetType = TSelf | TResponse | TGlobal | TTargeted
  deriving (Show, Read, Eq, Generic)
instance ToJSON TargetType
instance FromJSON TargetType

derivePersistField "CardType"
derivePersistField "CardLocation"
derivePersistField "ActionType"
derivePersistField "TargetType"
