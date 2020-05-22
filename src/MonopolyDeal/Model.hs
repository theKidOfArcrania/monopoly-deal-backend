{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE KindSignatures #-}
module MonopolyDeal.Model 
  ( module MonopolyDeal.Model ) where

import Prelude()
import ClassyPrelude.Yesod
import MonopolyDeal.Model.Cards   as MonopolyDeal.Model
import MonopolyDeal.Model.Persist as MonopolyDeal.Model
import MonopolyDeal.Model.Local   as MonopolyDeal.Model
import MonopolyDeal.Model.Message as MonopolyDeal.Model

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
  (MonadUnliftIO m) => ReaderT SqlBackend m a

toUser :: NewUser -> User
toUser ent = User
  { userProfile     = Nothing
  , userUserName    = newUserUserName ent
  , userDisplayName = newUserUserName ent
  , userEmail       = newUserEmail ent
  , userPassword    = newUserPassword ent}

toGame :: NewGame -> UTCTime -> Game
toGame ent time = Game 
  { gameCreator     = Nothing
  , gameCreated     = time
  , gameUsesDeck    = newGameUsesDeck ent
  , gameFinished    = False
  , gameDiscardSize = 0
  , gameDrawSize    = 0
  }

toCardInfo :: Entity Card -> CardInfo
toCardInfo ent = CardInfo
  { cardInfoId   = entityKey ent 
  , cardInfoSpec = cardSpec crd
  , cardInfoType = cardType crd
  , cardInfoLocation    = cardLocation crd
  , cardInfoOfPropColor = cardOfPropColor crd
  }
 where crd = entityVal ent

filtHand :: PlayerId -> [Filter Card]
filtHand pid = [CardLocation ==. LHand, CardOfPlayer ==. Just pid]

queryHand :: PlayerId -> DB [CardInfo]
queryHand pid = do
  hand <- selectList (filtHand pid) []
  pure $ map toCardInfo hand

queryPlayerInfo :: Entity Player -> DB PlayerInfo
queryPlayerInfo ent = do
  handSize <- count $ filtHand pid
  field <- fmap (map toCardInfo) $ selectList 
    [CardLocation <-. [LCashPile, LPropStack], CardOfPlayer ==. Just pid] []
  pure PlayerInfo 
    { playerInfoId       = pid
    , playerInfoStat     = entityVal ent
    , playerInfoHandSize = handSize
    , playerInfoField    = field
    }
 where pid = entityKey ent
