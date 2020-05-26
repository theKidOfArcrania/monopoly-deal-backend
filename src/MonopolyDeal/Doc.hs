{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module MonopolyDeal.Doc (API, swaggerDoc, writeDocs, writeDocsAt) where

import ClassyPrelude.Yesod hiding (Proxy)
import Control.Lens             ((&), (.~), (?~))
import Data.Aeson               (encode)
import Data.Proxy
import Data.Swagger
import Data.Swagger.Ext.Tags

import MonopolyDeal.Model
import MonopolyDeal.VersionMgmt (swaggerPath, nextVersion, updateDeps, Version)
import Servant.API
import Servant.Swagger

import qualified Data.ByteString.Lazy as B

-- Descriptions for all captures/queries
type CGid = Capture' '[Description "The game ID"] ":gid" GameId
type CDid = Capture' '[Description "The card deck ID"] ":did" CardDeckId
type QLimit = QueryParam' '[Optional, Description 
  "Number of entries to limit to (default 100)"] "limit" Int
type QPage = QueryParam' '[Optional, Description 
  "Page offset of entries relative to limit size"] "page" Int

-- API descriptors
type API = "api" :> "v1" :> (APIAuth :<|> APIGames :<|> APIPlaying)

type APIAuth = SwaggerTag "auth" "Authentication of services" :>
  (    "login" :> Summary "Authenticate an existing user" :>
       ReqBody '[JSON] UserAuth :> Post '[JSON] (Message Nil)
  :<|> "signup" :> Summary "Create a new user" :>
       ReqBody '[JSON] NewUser  :> Post '[JSON] (Message Nil)
  )

type APIGames = SwaggerTag "game" "Accessing and creating games" :> 
  (    "game" :> 
    (    Summary "Obtain all open public games" :>
         Description "this provides a list of game information similar to the \
           \/api/v1/game/{gid} endpoint. It currently gives all available \
           \games (paginated). Private games will be implemented later." :>
         QPage :> QLimit :>
         Get '[JSON] (Message Game)
    :<|> Summary "Create a new game" :>
         Description "Creates a game, making the host as the creator of this \
           \game. The host may also choose the card deck (game mode) to play \
           \from, along with a few other options (not yet fully determined)." :>
         ReqBody' '[Required, Strict, Description "The configurations for the \
           \new game. Currently there are only two settings: the name of the \
           \game and the card deck (as an ID)."] '[JSON] NewGame :>
         Post '[JSON] (Message CreatedGame)
    :<|> CGid :> 
      (    Summary "Obtain information of a specific game" :>
           Description "Similar to /api/v1/game, but only queries information \
             \about one specific game. To get more indepth information, use \
             \the status endpoint instead" :>
           Get '[JSON] (Message Game)))
  :<|> "gspec" :> CDid :> 
       Summary "Obtains the specifications of a card deck" :>
       Description "TODO" :>
       Get '[JSON] (Message GameSpec))


type APIPlaying = "game" :> CGid :> 
  SwaggerTag "playing" "Playing the game and viewing how the game is" :>
  (   "status" :> 
       Summary "Obtain more indepth information of a game" :>
       Description "This obtains some basic game information such as \
         \who's turn it is and which players are in the game." :>
       Get '[JSON] (Message GameStatus)
  :<|> "play" :>
    (    Summary "Obtain the play history of a game" :>
         Description "This contains a list of actions played out by you and \
           \the other players of this game, where the latest event occurs \
           \earlier in the list, i.e. sorted in date-descending order. Each \
           \entry contains cards involved in the action, and some specific \
           \metadata associated with the action. This endpoint has a \
           \pagination feature that limits the total number of history \
           \entries seen at any time." :>
         QPage :> QLimit :>
         Get '[JSON] (Message History)
    :<|> Summary "Plays out an action" :>
         Description "Use this endpoint to play out a card, whether if it is \
           \the player's turn, or to pay another player. Note that this does \
           \not include some other assorted instances such as: ending the \
           \player's turn, moving a wildcard around, or drawing a card. Those \
           \have separate endpoints." :>
         ReqBody' '[Required, Strict, Description "Pass in the cards to play, \
           \as IDs (whether if they are in hand or on field entirely depends \
           \on the specifc nature of the action), the specific player target, \
           \if any, as an ID, and the action played, passed as an ID. The list \
           \of allowed actions per card and in what contexts they are allowed \
           \are queried when requesting the game specs."] '[JSON] NewAction :>
         Post '[JSON] (Message Nil)))

mpdSwagger :: Swagger 
mpdSwagger = toSwagger (Proxy :: Proxy API)
  & info.title       .~ "Monopoly Deal API"
  & info.description ?~ "This is an API for the Monopoly Deal Game"
  & info.license     ?~ ("GNUv3" & url ?~ URL "https://www.gnu.org/licenses/")
  -- & info.version     .~ pack (show nextVersion)

swaggerDoc :: Version -> B.ByteString
swaggerDoc vers = encode $ mpdSwagger
  & info.version .~ pack (show vers)

writeDocsAt :: FilePath -> IO ()
writeDocsAt docPath = do
  nextVer <- nextVersion
  B.writeFile docPath $ swaggerDoc nextVer
  updateDeps

writeDocs :: IO ()
writeDocs = do
  nextVer <- nextVersion
  writeDocsAt $ unpack $ swaggerPath nextVer
