{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
module MonopolyDeal.Swagger where

import Prelude (reads)
import ClassyPrelude.Yesod
import MonopolyDeal.VersionMgmt

-- Creates "swaggerFiles" and "versions" variables, plus the "SwaggerVersion"
-- data constructor, from all the files found in the config/swagger directory
mkSwagger

instance PathPiece SwaggerVersion where
  toPathPiece i = pack $ show i
  fromPathPiece s =
      case reads $ unpack s of
          (i, ""):_ -> Just i
          _ -> Nothing

-- TODO
latestVersion = V0_1_1
