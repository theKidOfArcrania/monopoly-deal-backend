{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE NoImplicitPrelude #-}
module MonopolyDeal.Handler.Login 
  ( postJsonLoginR
  , postSignupR
  ) where
import MonopolyDeal.Import

-- TODO: change these to actual errors
badPass :: MonadHandler m => m a
badPass = permissionDenied "Invalid username or password"

postJsonLoginR :: Handler Value
postJsonLoginR = do
  uauth <- requireCheckJsonBody :: Handler UserAuth
  let uname = userAuthUserName uauth
  ent <- runDB $ getBy $ UniqueUserName uname
  case ent of
    Nothing -> badPass
    Just (Entity _ u) -> do
       -- TODO: check password hash instead
       when (userPassword u /= userAuthPassword uauth) badPass
       setCreds False $ Creds "json-login" uname []
       return $ success "You have successfully authenticated"

postSignupR :: Handler Value 
postSignupR = do
  unew <- requireCheckJsonBody :: Handler NewUser
  runDB $ do
    badUname <- chkExists $ UniqueUserName $ newUserUserName unew
    when badUname (invalidArgs ["Username already exists"])
    badEmail <- chkExists $ UniqueEmail $ newUserEmail unew
    when badEmail (invalidArgs ["Email already exists"])

    _ <- insert $ toUser unew
    return $ success "Successfully created account, now log in."

