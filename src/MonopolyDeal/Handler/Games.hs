{-# LANGUAGE OverloadedStrings #-}

module MonopolyDeal.Handler.Games where

import Prelude              ()
import MonopolyDeal.Import

-- Provides game information of a specific ID
getGameR :: GameId -> Handler Value
getGameR gid = do
  game <- runDB $ requireId "game" gid
  pure $ toJSON $ msg200P "Success!" $ game
  
-- Provides a list of all currently public games (paginated). Since there are no
-- permissions yet implemented, this returns every game.
getGamesR :: Handler Value
getGamesR = do
  page <- queryPaginate maybeParam
  games <- runDB $ selectList [] page :: Handler [Entity Game]
  pure $ toJSON $ msg200P "Success!" $ games

-- Create a new game with a few configuration settings
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
      , playerTurnNum = TNotPlaying
      }
    update gid [GameCreator =. Just pid]

    -- TODO: initial game state
    pure $ toJSON $ msg200P "Successfully created game!" $ CreatedGame 
      { createdGameId     = gid
      , createdGamePlayer = pid
      }

getGameSpecR :: CardDeckId -> Handler Value
getGameSpecR deckId = runDB $ do
  deck <- requireId "card deck" deckId
  spec <- queryGameSpec deck deckId 
  pure $ toJSON $ msg200P "Success!" spec


