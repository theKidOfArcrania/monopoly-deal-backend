{-# LANGUAGE OverloadedStrings #-}

module MonopolyDeal.Handler.Games where

import Prelude              ()
import Data.Map             (fromList)
import MonopolyDeal.Import

(<$$>) :: (Functor f, Functor g) => (a -> b) -> g (f a) -> g (f b)
(<$$>) fn = fmap (fmap fn)
infix 0 <$$>

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

    pure $ toJSON $ msg200P "Successfully created game!" $ CreatedGame 
      { createdGameId     = gid
      , createdGamePlayer = pid
      }

-- Obtain more specific game status information
getGameStatusR :: GameId -> Handler Value
getGameStatusR gid = do
  uid <- requireAuthId 
  runDB $ do
    game <- requireId "game" gid
    mplayer <- entityKey <$$> getBy (UniquePlayer gid uid)
    -- Get the player's hand (a deck of cards) if player is in game
    hand <- maybe (pure []) queryHand mplayer
    -- Get extended player info
    players <- selectList [PlayerPlaying ==. gid] [] 
    playerInfos <- mapM queryPlayerInfo players
    -- Card infos on all dicarded (played) cards, sorted in played order, i.e.
    -- descending order of discard index
    discardInfos <- toCardInfo <$$> selectList [CardLocation ==. LDiscard] 
      [Desc CardOfDiscard]
    pure $ toJSON $ msg200P "Success!" $ GameStatus
      { gameStatusGame    = Entity gid game
      , gameStatusMe      = mplayer
      , gameStatusPlayers = Data.Map.fromList $ map 
          (playerInfoId &&& id) playerInfos
      , gameStatusDiscard = discardInfos
      , gameStatusHand    = hand
      }

-- Get a game's history log (paginated)
getGamePlayR :: GameId -> Handler Value
getGamePlayR gid = do
  page <- queryPaginate maybeParam
  runDB $ do
    _ <- requireId "game" gid
    actions <- selectList [ActionOfGame ==. gid] (Desc ActionMadeAt : page)
    history <- mapM queryHistoryEnt actions
    pure $ toJSON $ msg200P "Success!" $ History history
 
-- Make an action play for a player
postGamePlayR :: GameId -> Handler Value
postGamePlayR gid = do
  action <- requireCheckJsonBody :: Handler NewAction
  uid <- requireAuthId 
  runDB $ do
    -- Require that the user is actually a player of this game
    _ <- requireId "game" gid
    ent <- getBy (UniquePlayer gid uid) >>= maybe eNotPlayer pure
    let pid = entityKey ent
        player = entityVal ent
        turn = playerTurnNum player
        next = nextTurn turn

    -- Process the player's action based on his/her turn number
    resp <- case turn of
      TDrawing -> eNotTurn
      TNotPlaying -> eNotTurn
      TPaying -> do
        -- TODO: 
        error "TODO: implement paying logic"
      Turn _ -> do
        -- TODO: 
        error "TODO: implement playing a regular card"

    -- Advance player's turn
    update pid [PlayerTurnNum =. next]
    when (next == TNotPlaying) $ do
      -- TODO: get next player's turn
      error "TODO: change to new player's turn"
      
    -- TODO: change that type of response!
    pure $ toJSON $ msg200P "Success!" (resp :: Value) 
 where eNotPlayer = invalidArgs ["You are not a player of this game"]
       eNotTurn   = invalidArgs ["Not your turn to play"]

-- TODO: notify original player when they get a response to a card they played

