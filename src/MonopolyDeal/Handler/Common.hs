{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module MonopolyDeal.Handler.Common where

import Data.FileEmbed (embedFile)
import MonopolyDeal.Import
import qualified Data.HashMap.Strict.InsOrd as InsOrd

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent

getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

getSwaggerR :: Version -> Handler TypedContent
getSwaggerR vers = do
  let res = InsOrd.lookup vers swaggerFilesMap
  case res of
    Just cont -> pure $ TypedContent typeJson cont
    Nothing -> notFound
