{-# LANGUAGE OverloadedStrings #-}

module MonopolyDeal.Handler.Games where

import Prelude              ()
import Data.Map             (fromList)
import MonopolyDeal.Import

-- Provides game information of a specific ID
getGameR :: GameId -> Handler Value
getGameR gid = do
  game <- runDB $ requireId "game" gid
  pure $ toJSON game
  
-- Provides a list of all currently public games (since there are no permissions
-- yet implemented, this returns every game).
getGamesR :: Handler Value
getGamesR = do
  games <- runDB $ selectList [] [] :: Handler [Entity Game]
  pure $ toJSON games

postGamesR :: Handler Value
postGamesR = do
  time <- liftIO getCurrentTime
  new <- requireCheckJsonBody :: Handler NewGame
  uid <- requireAuthId
  runDB $ do
    -- Check that the card deck is a valid deck ID
    _ <- requireId "card deck" $ newGameUsesDeck new

    -- Create the game, with the one player added as the first player (and also
    -- the owner of the player)
    gid <- insert $ toGame new time
    pid <- insert $ Player 
      { playerPlaying = gid
      , playerUser = uid
      , playerIsPaying = True
      , playerTurnNum = Nothing 
      }
    update gid [GameCreator =. Just pid]

    -- Return the IDs created
    let ret = CreatedGame 
              { createdGameId     = gid
              , createdGamePlayer = pid
              }
    pure $ toJSON $ msg200P "Successfully created game!" ret

getGameStatusR :: GameId -> Handler Value
getGameStatusR gid = do
  uid <- requireAuthId 
  runDB $ do
    game <- requireId "game" gid
    mplayer <- getBy $ UniquePlayer gid uid
    players <- selectList [PlayerPlaying ==. gid] []
    pure $ toJSON $ GameStatus
      { gameStatusGame    = Entity gid game
      , gameStatusMe      = map entityKey mplayer
      , gameStatusPlayers = Data.Map.fromList $ map (\e ->
          (entityKey e, entityVal e)) players
      }
