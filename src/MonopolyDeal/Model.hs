{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts  #-}
module MonopolyDeal.Model 
  ( module MonopolyDeal.Model ) where

import Prelude()
import ClassyPrelude.Yesod
import Control.Exception          (throw)
import Data.Map                   (fromList)
import MonopolyDeal.Model.Persist as MonopolyDeal.Model
import MonopolyDeal.Model.Local   as MonopolyDeal.Model
import MonopolyDeal.Model.Message as MonopolyDeal.Model

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
toCardInfo ent = toCardInfo2 (entityKey ent) (entityVal ent)

toCardInfo2 :: CardId -> Card -> CardInfo
toCardInfo2 cid crd = CardInfo
  { cardInfoId   = cid
  , cardInfoSpec = cardSpec crd
  , cardInfoType = cardType crd
  , cardInfoLocation    = cardLocation crd
  , cardInfoOfPropColor = cardOfPropColor crd
  }

filtHand :: PlayerId -> [Filter Card]
filtHand pid = [CardLocation ==. LHand, CardOfPlayer ==. Just pid]

queryHand :: PlayerId -> DB [CardInfo]
queryHand pid = do
  hand <- selectList (filtHand pid) []
  pure $ toCardInfo <$> hand

queryPlayerInfo :: Entity Player -> DB PlayerInfo
queryPlayerInfo ent = do
  handSize <- count $ filtHand pid
  field <- map toCardInfo <$> selectList 
    [CardLocation <-. [LCashPile, LPropStack], CardOfPlayer ==. Just pid] []
  pure PlayerInfo 
    { playerInfoId       = pid
    , playerInfoStat     = entityVal ent
    , playerInfoHandSize = handSize
    , playerInfoField    = field
    }
 where pid = entityKey ent

queryHistoryEnt :: Entity Action -> DB HistoryEntry
queryHistoryEnt action = do
  ivls <- selectList [InvolvesAction ==. entityKey action] []
    >>= mapM (infoOf . entityVal)
  pure HistoryEntry 
    { historyEntryAction   = entityVal action
    , historyEntryId       = entityKey action
    , historyEntryInvolves = ivls
    }
 where infoOf ent = do
         let cid = involvesCard ent
             eBadId = PersistError "Orphaned card ID"
         card <- toCardInfo2 cid <$> (get cid >>= maybe (throw eBadId) pure)
         pure InvolveInfo 
           { involveInfoCard        = card
           , involveInfoToPlayer    = involvesToPlayer ent
           , involveInfoToPropColor = involvesToPropColor ent } 
             :: DB InvolveInfo -- Need this binding at this point...

fromEntList :: (Ord (Key record)) => [Entity record] -> Map (Key record) record
fromEntList = Data.Map.fromList . map (entityKey &&& entityVal)

-- TODO: flip the arguments
queryGameSpec :: CardDeck -> CardDeckId -> DB GameSpec
queryGameSpec deck deckId = do
  cards <- selectList [CardSpecsPartOf ==. deckId] []
  actions <- selectList [AllowedActionOfDeck ==. deckId] []
  colors <- selectList [ColorOfDeck ==. deckId] []
  pure $ GameSpec
    { gameSpecDeckInfo    = deck
    , gameSpecDeckId      = deckId
    , gameSpecIdToCards   = fromEntList cards
    , gameSpecIdToActions = fromEntList actions
    , gameSpecIdToColors  = fromEntList colors
    }

queryPaginate :: Monad m => (Text -> m (Maybe Int)) -> m [SelectOpt record]
queryPaginate maybeParam = do
  let maxLimit = 100
  mlimit <- maybeParam "limit"
  mpg <- maybeParam "page"
  let limit = maybe maxLimit (min maxLimit) mlimit
  pure [OffsetBy (limit * maybe 0 (max 0) mpg), LimitTo limit]

