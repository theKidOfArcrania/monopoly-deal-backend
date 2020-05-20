module MonopolyDeal.Model 
  ( module MonopolyDeal.Model ) where

import ClassyPrelude.Yesod
import MonopolyDeal.Model.Persist as MonopolyDeal.Model
import MonopolyDeal.Model.Local   as MonopolyDeal.Model

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
