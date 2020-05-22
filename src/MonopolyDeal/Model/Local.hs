{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DeriveGeneric         #-}
module MonopolyDeal.Model.Local where 

import Prelude ()
import ClassyPrelude.Yesod
import MonopolyDeal.Model.Cards
import MonopolyDeal.Model.Entity()
import MonopolyDeal.Model.Persist
import MonopolyDeal.Model.Util

data UserAuth = UserAuth
  { userAuthUserName :: Text
  , userAuthPassword :: Text
  } deriving (Eq, Read, Show, Generic, Typeable)

data NewUser = NewUser
  { newUserUserName :: Text
  , newUserPassword :: Text
  , newUserEmail    :: Text
  } deriving (Eq, Read, Show, Generic, Typeable)

data GameStatus = GameStatus
  { gameStatusGame    :: Entity Game
  , gameStatusMe      :: Maybe PlayerId
  , gameStatusPlayers :: Map PlayerId PlayerInfo
  , gameStatusDiscard :: [CardInfo]
  , gameStatusHand    :: [CardInfo]
  } deriving (Eq, Read, Show, Generic, Typeable)

data NewGame = NewGame
  { newGameName     :: Text, 
    newGameUsesDeck :: CardDeckId
  } deriving (Eq, Read, Show, Generic, Typeable)

data CreatedGame = CreatedGame
  { createdGameId      :: GameId
  , createdGamePlayer  :: PlayerId
  } deriving (Eq, Read, Show, Generic, Typeable)

data PlayerInfo = PlayerInfo
  { playerInfoId        :: PlayerId
  , playerInfoStat      :: Player
  , playerInfoHandSize  :: Int
  , playerInfoField     :: [CardInfo]
  } deriving (Eq, Read, Show, Generic, Typeable)

data CardInfo = CardInfo
  { cardInfoId          :: CardId
  , cardInfoSpec        :: CardSpecsId
  , cardInfoType        :: CardType
  , cardInfoLocation    :: CardLocation
  , cardInfoOfPropColor :: Maybe ColorId
  } deriving (Eq, Read, Show, Generic, Typeable)

newtype History = History { unHistory :: [Action] }
  deriving (Eq, Read, Show, Generic, Typeable)

deriveAll ''UserAuth
deriveAll ''NewUser
deriveAll ''GameStatus
deriveAll ''NewGame
deriveAll ''CreatedGame
deriveAll ''PlayerInfo
deriveAll ''CardInfo
deriveAll ''History
