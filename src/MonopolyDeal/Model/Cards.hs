{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module MonopolyDeal.Model.Cards where

import Control.Lens ((?~), (&))
import Database.Persist.TH
import Data.Aeson.Types
import Data.Swagger 
  ( ToSchema, SwaggerType(..), NamedSchema(..)
  , type_, minimum_, maximum_, description, declareNamedSchema )
import Prelude
import GHC.Generics

data CardType = CAction | CProperty | CCash
  deriving (Show, Read, Eq, Generic)
instance ToJSON CardType
instance FromJSON CardType
instance ToSchema CardType

data CardLocation = LHand | LCashPile | LDraw | LDiscard | LPropStack
  deriving (Show, Read, Eq, Generic)
instance ToJSON CardLocation
instance FromJSON CardLocation
instance ToSchema CardLocation

data ActionType = APayment | ARefutal | AGive | APlayCard
  deriving (Show, Read, Eq, Generic)
instance ToJSON ActionType
instance FromJSON ActionType
instance ToSchema ActionType

data TargetType = TSelf | TResponse | TGlobal | TTargeted
  deriving (Show, Read, Eq, Generic)
instance ToJSON TargetType
instance FromJSON TargetType
instance ToSchema TargetType

data TurnNum = TNotPlaying | T1 | T2 | T3
  deriving (Show, Read, Eq, Generic)
instance ToJSON TurnNum where
  toJSON TNotPlaying = Null
  toJSON T1 = Number 1
  toJSON T2 = Number 2
  toJSON T3 = Number 3
instance FromJSON TurnNum where
  parseJSON (Number n) = case n of
                            1 -> pure T1
                            2 -> pure T2
                            3 -> pure T3
                            _ -> mempty
  parseJSON (Null) = pure TNotPlaying
  parseJSON _ = mempty
instance ToSchema TurnNum where
  declareNamedSchema _ = pure $ NamedSchema Nothing $ mempty
    & type_        ?~ SwaggerInteger
    & minimum_     ?~ 1
    & maximum_     ?~ 3
    & description  ?~ "the turn number, can be null if user is not taking a turn"

nextTurn :: TurnNum -> Maybe TurnNum
nextTurn TNotPlaying = Nothing
nextTurn T1 = Just T2
nextTurn T2 = Just T3
nextTurn T3 = Nothing

derivePersistField "CardType"
derivePersistField "CardLocation"
derivePersistField "ActionType"
derivePersistField "TargetType"
derivePersistField "TurnNum"
