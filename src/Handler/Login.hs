{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Login 
  ( postJsonLoginR
  , postSignupR
  ) where
import Import
import qualified Model.UserAuth as UserAuth
import qualified Model.NewUser as NewUser

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- TODO: change these to actual errors
badPass :: Value
badPass = toJSON $ msg403 "Invalid username or password" Null

alreadyExist :: Text -> Value
alreadyExist m = toJSON $ msg409 m Null

success :: Text -> Value
success m = toJSON $ msg200 m Null

chkExists :: (MonadIO m, PersistUniqueRead backend, PersistEntity record, 
             PersistEntityBackend record ~ BaseBackend backend) =>
               Unique record -> ReaderT backend m (Bool)
chkExists uniq = getBy uniq >>= return . isJust


postJsonLoginR :: Handler Value
postJsonLoginR = do
  uauth <- requireCheckJsonBody :: Handler UserAuth.T
  let uname = UserAuth.userName uauth
  ent <- runDB $ getBy $ UniqueUserName uname
  case ent of
    Nothing -> return $ badPass
    Just (Entity _ u) ->
      -- TODO: check password hash instead
      if userPassword u == UserAuth.password uauth
         then do
           setCreds False $ Creds "json-login" uname []
           return $ success "You have successfully authenticated"
         else return $ badPass

postSignupR :: Handler Value 
postSignupR = do
  unew <- requireCheckJsonBody :: Handler NewUser.T
  runDB $ do
    badUname <- chkExists $ UniqueUserName $ NewUser.userName unew
    badEmail <- chkExists $ UniqueEmail $ NewUser.email unew
    if' badUname (return $ alreadyExist "Username already exists") $
      if' badEmail (return $ alreadyExist "Email already exists") $
      do
        _ <- insert $ NewUser.toUserEntity unew
        return $ success "Successfully created account, now log in."

