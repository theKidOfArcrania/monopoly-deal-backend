{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
module MonopolyDeal.Swagger where

import Prelude()
import ClassyPrelude.Yesod
import MonopolyDeal.VersionMgmt
import qualified Data.HashMap.Strict.InsOrd as InsOrd

-- Creates "swaggerFiles" and "versions" variables, from all the files found in
-- the config/swagger directory
mkSwagger

latestVersion :: Version
latestVersion = maybe (Version 0 0 0) maximum $ fromNullable versions

swaggerFilesMap :: InsOrd.InsOrdHashMap Version Content
swaggerFilesMap = InsOrd.fromList swaggerFiles
