{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
module MonopolyDeal.Doc (API, swaggerDoc, writeDocs, writeDocsAt) where

import ClassyPrelude.Yesod
import Control.Lens             ((&), (.~), (?~))
import Data.Aeson               (encode)
import Data.Swagger

import MonopolyDeal.Model
import MonopolyDeal.Model.Util
import MonopolyDeal.VersionMgmt (swaggerPath, nextVersion, Version)
import Servant.API
import Servant.Swagger


import qualified Data.ByteString.Lazy as B

declareEndpoint "LoginP" 
  [t|"login"  :> ReqBody '[JSON] UserAuth :> Post '[JSON] (Message Nil)|]
declareEndpoint "SignupP" 
  [t|"signup" :> ReqBody '[JSON] NewUser  :> Post '[JSON] (Message Nil)|]
declareEndpoint "cAuth" [t|APILoginP :<|> APISignupP|]

declareEndpoint "GameG" [t|"game" :> Get '[JSON] (Message Game)|]
declareEndpoint "GameP" 
  [t|"game" :> ReqBody '[JSON] NewGame :> Post '[JSON] (Message CreatedGame)|]
declareEndpoint "GameIdG" 
  [t|"game" :> Capture "gid" GameId :> Get '[JSON] (Message Game)|]
declareEndpoint "GameIdStatusG" [t|"game" :> Capture "gid" GameId :> 
  "status" :> Get '[JSON] (Message GameStatus)|]
declareEndpoint "GameIdHistG" [t|"game" :> Capture "gid" GameId :> 
  "history" :> Get '[JSON] (Message History)|]
declareEndpoint "cGame" [t|APIGameG :<|> APIGameP :<|> APIGameIdG 
  :<|> APIGameIdStatusG :<|> APIGameIdHistG|]

declareEndpoint "All" [t|APIcGame :<|> APIcAuth|]
type API = "api" :> "v1" :> APIAll

declareSubOperationsFor 'pAll 
  [ "LoginP", "SignupP" , "cAuth", "GameG", "GameP", "GameIdG", "GameIdStatusG"
  , "cGame", "GameIdHistG"]

mpdSwagger :: Swagger 
mpdSwagger = toSwagger pAll
  & info.title       .~ "Monopoly Deal API"
  & info.description ?~ "This is an API for the Monopoly Deal Game"
  & info.license     ?~ ("GNUv3" & url ?~ URL "https://www.gnu.org/licenses/")
  -- & info.version     .~ pack (show nextVersion)
  & applyTagsFor  socAuth ["auth" & description ?~ "Authenticating to service"]
  & applyTagsFor  socGame ["game" & description ?~ "Game status operations"]
  & soLoginP.summary     ?~ "Authenticate with username and password"  
  & soSignupP.summary    ?~ "Create new user"  
  & soGameG.summary      ?~ "Obtain all the open public games"  
  & soGameP.description  ?~ "this provides a list of game information " <> 
    "similar to the /api/v1/game/{gid} endpoint. It currently gives all " <>
    "available games, but pagination + private games will be implemented later."
  & soGameP.summary      ?~ "Create a new game" 
  & soGameP.description  ?~ "Creates a game, making the host as the creator " <>
    "of this game. The host may also choose the card deck (game mode) to " <>
    "from."
  & soGameIdG.summary    ?~ "Obtain the game information of a specific ID"
  & soGameIdStatusG.summary     ?~ "Obtain more in-depth game information"
  & soGameIdStatusG.description ?~ "This obtains some basic game " <>
    "information such as current player making moves, and what players are " <>
    "in the game at the moment."
  & soGameIdHistG.summary       ?~ "Obtain play history of a game"
  & soGameIdHistG.description   ?~ "This gives a list of all cards that " <>
    "have been played so far in the specified game, where the latest event " <>
    "occurs earlier in the list. Eventually this should only filter out " <>
    "only the latest actions so to minimize the size of the request"

swaggerDoc :: Version -> B.ByteString
swaggerDoc vers = encode $ prependPath "/api/v1" mpdSwagger
  & info.version .~ pack (show vers)

writeDocsAt :: FilePath -> IO ()
writeDocsAt docPath = do
  nextVer <- nextVersion
  B.writeFile docPath $ swaggerDoc nextVer

writeDocs :: IO ()
writeDocs = do
  nextVer <- nextVersion
  writeDocsAt $ unpack $ swaggerPath nextVer
