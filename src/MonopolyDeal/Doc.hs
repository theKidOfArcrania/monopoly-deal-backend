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
module MonopolyDeal.Doc (API, swaggerDoc, writeDocs) where

import Control.Lens             ((&), (.~), (?~))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Swagger

import MonopolyDeal.Model
import MonopolyDeal.Model.Util
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
declareEndpoint "cGame" [t|APIGameG :<|> APIGameP :<|> APIGameIdG 
  :<|> APIGameIdStatusG|]

declareEndpoint "All" [t|APIcGame :<|> APIcAuth|]
type API = "api" :> "v1" :> APIAll

declareSubOperationsFor 'pAll 
  [ "LoginP", "SignupP" , "cAuth", "GameG", "GameP", "GameIdG", "GameIdStatusG"
  , "cGame"]

mpdSwagger :: Swagger 
mpdSwagger =  toSwagger pAll
  & info.title       .~ "Monopoly Deal API"
  & info.version     .~ "0.1"
  & info.description ?~ "This is an API for the Monopoly Deal Game"
  & info.license     ?~ ("GNUv3" & url ?~ URL "https://www.gnu.org/licenses/")
  & host             ?~ "example.com" -- TODO: change this to real host
  & applyTagsFor  socAuth ["auth" & description ?~ "Authenticating to service"]
  & applyTagsFor  socGame ["game" & description ?~ "Game status operations"]
  & soLoginP.summary     ?~ "Authenticate with username and password"  
  & soSignupP.summary    ?~ "Create new user"  
  & soGameG.summary      ?~ "Obtain all the open public games"  
  & soGameP.summary      ?~ "Create a new game"  
  & soGameP.description  ?~ "Creates a game, making the host as the creator " <>
    "of this game. The host may also choose the card deck (game mode) to " <>
    "from."
  & soGameIdG.summary    ?~ "Obtain the game information of a specific ID"
  & soGameIdStatusG.summary     ?~ "Obtain more in-depth game information"
  & soGameIdStatusG.description ?~ "This obtains some basic game " <>
    "information such as current player making moves, and what players are " <>
    "in the game at the moment."

swaggerDoc :: B.ByteString
swaggerDoc = encodePretty $ prependPath "/api/v1" mpdSwagger

writeDocs :: FilePath -> IO ()
writeDocs path = B.writeFile path swaggerDoc
