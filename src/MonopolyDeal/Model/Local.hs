{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DeriveGeneric         #-}
module MonopolyDeal.Model.Local where 

import Prelude ()
import ClassyPrelude.Yesod
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

data InvolveInfo = InvolveInfo
  { involveInfoCard        :: CardInfo
  , involveInfoToPlayer    :: Maybe PlayerId
  , involveInfoToPropColor :: Maybe ColorId
  } deriving (Eq, Read, Show, Generic, Typeable)

data HistoryEntry = HistoryEntry
  { historyEntryId       :: ActionId
  , historyEntryAction   :: Action
  , historyEntryInvolves :: [InvolveInfo]
  } deriving (Eq, Read, Show, Generic, Typeable)

newtype History = History { unHistory :: [HistoryEntry] }
  deriving (Eq, Read, Show, Generic, Typeable)

data Target = Target
  { targetPlayer :: PlayerId
  , targetProp   :: Maybe CardId
  , targetSet    :: Maybe ColorId
  } deriving (Eq, Read, Show, Generic, Typeable)

data ActionSpec = SpecPlays {specPlays :: CardId} 
                | SpecTargets {specTargets :: Target}
                | SpecGives {specGives :: CardId}
  deriving (Eq, Read, Show, Generic, Typeable)

data NewAction = NewAction
  { newActionAction   :: AllowedActionId
  , newActionSpecs    :: ActionSpec
  , newActionResponds :: Maybe ActionId
  } deriving (Eq, Read, Show, Generic, Typeable) 

data GameSpec = GameSpec
  { gameSpecDeckInfo       :: CardDeck
  , gameSpecDeckId         :: CardDeckId
  , gameSpecIdToCards      :: Map CardSpecsId CardSpecs
  , gameSpecIdToActions    :: Map AllowedActionId AllowedAction
  , gameSpecIdToColors     :: Map ColorId Color
  } deriving (Eq, Read, Show, Generic, Typeable) 

deriveAll ''UserAuth
deriveAll ''NewUser
deriveAll ''HistoryEntry
deriveAll ''GameStatus
deriveAll ''NewGame
deriveAll ''CreatedGame
deriveAll ''PlayerInfo
deriveAll ''CardInfo
deriveAll ''InvolveInfo
deriveAll ''History
deriveAll ''NewAction
deriveAll ''Target
deriveAll ''GameSpec
deriveAllPrefUnion ''ActionSpec "Spec"

