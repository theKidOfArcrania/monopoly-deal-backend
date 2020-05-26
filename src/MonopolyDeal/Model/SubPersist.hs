{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
module MonopolyDeal.Model.SubPersist where

import Control.Lens ((?~), (&))
import Database.Persist.TH
import Data.Aeson.Types
import Data.Swagger 
  ( ToSchema, SwaggerType(..), NamedSchema(..)
  , type_, format, enum_, description, declareNamedSchema )
import Prelude
import GHC.Generics
import MonopolyDeal.Model.Util

data CardType = CAction | CProperty | CCash
  deriving (Show, Read, Eq, Generic)
deriveAllPref ''CardType "C"

data CardLocation = LHand | LCashPile | LDraw | LDiscard | LPropStack
  deriving (Show, Read, Eq, Generic)
deriveAllPref ''CardLocation "L"

data ActionType = APayment | ARefutal | AGive | APlayCard
  deriving (Show, Read, Eq, Generic)
deriveAllPref ''ActionType "A"

data TargetType = TSelf | TResponse | TGlobal | TTargeted
  deriving (Show, Read, Eq, Generic)
deriveAllPref ''TargetType "T"

--data TargetSpec = TSSelf Card | TSResponse
--  deriving (Show, Read, Eq, Generic)
--deriveAllPref ''TargetSpec "TS"

data TNum = T1 | T2 | T3 deriving (Show, Read, Eq, Generic)
fromTNum :: Num n => TNum -> n
fromTNum T1 = 1 
fromTNum T2 = 2 
fromTNum T3 = 3 
toTNum :: (Eq n, Num n) => n -> Maybe TNum
toTNum 1 = Just T1 
toTNum 2 = Just T2 
toTNum 3 = Just T3 
toTNum _ = Nothing

data TurnState = TNotPlaying | TPaying | TDrawing | Turn TNum
  deriving (Show, Read, Eq, Generic)
instance ToJSON TurnState where
  toJSON TNotPlaying = String "none"
  toJSON TPaying     = String "paying"
  toJSON TDrawing    = String "drawing"
  toJSON (Turn n)    = Number $ fromTNum n
instance FromJSON TurnState where
  parseJSON (Number n) = maybe mempty (pure . Turn) $ toTNum n
  parseJSON (String "drawing") = pure TDrawing
  parseJSON (String "paying") = pure TPaying
  parseJSON (String "none") = pure TNotPlaying
  parseJSON _ = mempty
instance ToSchema TurnState where
  declareNamedSchema _ = pure $ NamedSchema Nothing $ mempty
    & type_        ?~ SwaggerString
    & description  ?~ "the turn number/state that the user is in"
    & format       ?~ "enum"
    & enum_        ?~ [ String "drawing", String "paying", String "none"
                      , Number 1, Number 2, Number 3 ]

nextTurn :: TurnState -> TurnState
nextTurn TNotPlaying = TNotPlaying
nextTurn TPaying  = TNotPlaying
nextTurn TDrawing = Turn T1
nextTurn (Turn n) = maybe TNotPlaying Turn $ toTNum $ (+) (1 :: Int) $ fromTNum n

derivePersistField "CardType"
derivePersistField "CardLocation"
derivePersistField "ActionType"
derivePersistField "TargetType"
derivePersistField "TurnState"
