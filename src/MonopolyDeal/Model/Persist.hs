{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE RankNTypes                 #-}
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
module MonopolyDeal.Model.Persist
  ( module MonopolyDeal.Model.Persist ) where

import ClassyPrelude.Yesod
import Data.Docs.DocGen
import Database.Persist.Quasi
import MonopolyDeal.Model.SubPersist as MonopolyDeal.Model.Persist
import MonopolyDeal.Model.Entity()

-- | A convenient synonym for database access functions.
type DB a = forall (m :: * -> *).
  (MonadUnliftIO m) => ReaderT SqlBackend m a

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkModels sqlSettings "docPersist"] 
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")
