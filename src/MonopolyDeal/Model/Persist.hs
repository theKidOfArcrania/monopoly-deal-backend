{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module MonopolyDeal.Model.Persist where

import ClassyPrelude.Yesod
import Data.Aeson (ToJSONKey, FromJSONKey)
import Data.Docs.DocGen
import Database.Persist.Quasi
import MonopolyDeal.Model.Cards
import MonopolyDeal.Model.Entity()


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkModels sqlSettings "docPersist"] 
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

-- deriving instance Generic PlayerId
instance ToJSONKey PlayerId
instance FromJSONKey PlayerId
