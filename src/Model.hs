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
module Model where

import Model.Cards
import ClassyPrelude.Yesod
import Database.Persist.Quasi
import DocGen

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll", mkModels sqlSettings "docPersist"] 
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")

share [mkPersist sqlSettings, mkModels sqlSettings "docLocal"] 
  $(persistFileWith lowerCaseSettings "config/models.local")

toUserEntity :: NewUser -> User
toUserEntity ent = User
  { userProfile     = Nothing
  , userUserName    = newUserUserName ent
  , userDisplayName = newUserUserName ent
  , userEmail       = newUserEmail ent
  , userPassword    = newUserPassword ent}


