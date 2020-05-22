{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}

module MonopolyDeal.Foundation where

import MonopolyDeal.Import.NoFoundation

import Prelude              (reads)
import Control.Monad.Logger (LogSource)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Jasmine         (minifym)

import Yesod.Auth.Message   (AuthMessage(InvalidLogin))
import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.Text.Encoding as TSE

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings  :: AppSettings
  , appStatic    :: Static -- ^ Settings for static file serving.
  , appConnPool  :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger    :: Logger
  }


-- This is where we define all of the routes in our application
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ = Just <$> defaultClientSessionBackend
    120  -- timeout in minutes
    "config/client_session_key.aes"

  isAuthorized :: Route App -> Bool -> HandlerFor App AuthResult
  isAuthorized r _ = 
    if "auth" `member` attrs && (not $ "noauth" `member` attrs)
       then isAuthenticated
       else return $ Authorized 
   where attrs = routeAttrs r

  -- Since most of these routes are JSON endpoints, we give error in JSON format
  errorHandler :: ErrorResponse -> HandlerFor App TypedContent
  errorHandler err = do
    m <- errorHandlerMsg err
    return $ toTypedContent $ toJSON m

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent
    :: Text  -- ^ The file extension
    -> Text -- ^ The MIME content type
    -> LByteString -- ^ The contents of the file
    -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
    -- Generate a unique filename based on the content itself
    genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
    appShouldLogAll (appSettings app)
      || level == LevelWarn
      || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

errorHandlerMsg :: ErrorResponse -> HandlerFor App (Message [Text])
errorHandlerMsg (InvalidArgs ia) = return $ msg400P "Invalid arguments" ia
errorHandlerMsg NotAuthenticated = do
  rend <- getUrlRender
  return $ msg401P "Not logged in" [rend $ ApiR JsonLoginR]
errorHandlerMsg (PermissionDenied m) =
  return $ msg403 ("Permission Denied. " <> m) 
errorHandlerMsg NotFound = return $ msg404 "Invalid path"
errorHandlerMsg (BadMethod m) = return $ msg405P "Bad method" [TSE.decodeUtf8 m]
errorHandlerMsg (InternalError e) = do
  $logErrorS "Foundation" e
  return $ msg500P "Internal server error" [e]

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = HomeR
  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR
  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate :: (MonadHandler m, HandlerSite m ~ App)
               => Creds App -> m (AuthenticationResult App)
  authenticate creds = liftHandler $ runDB $ do
    x <- getBy $ UniqueUserName $ credsIdent creds
    case x of
      Just (Entity uid _) -> return $ Authenticated uid
      Nothing -> return $ UserError InvalidLogin

  -- Nothing
  -- authPlugins :: App -> [AuthPlugin App]
  -- authPlugins app = []

isAuthenticated :: Handler AuthResult
isAuthenticated = do
  muid <- maybeAuthId
  return $ case muid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool



-- authenticateWith :: (MonadHandler m, HandlerSite m ~ App)
--        => Text -> Text -> m (AuthenticationResult App)
-- authenticateWith uName passwd = liftHandler $ runDB $ do
--   ent <- getBy $ UniqueUserName $ uName
--   case ent of
--     Just (Entity uid u) -> 
--       if userPassword u == passwd
--          then do
--            return $ Authenticated uid
--          else return $ UserError InvalidLogin
--     Nothing -> return $ UserError InvalidLogin

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

chkExists :: (MonadIO m, PersistUniqueRead backend, PersistEntity record, 
             PersistEntityBackend record ~ BaseBackend backend) =>
               Unique record -> ReaderT backend m (Bool)
chkExists uniq = getBy uniq >>= return . isJust

chkExistsId :: (MonadIO m, PersistUniqueRead backend, PersistEntity record, 
              PersistEntityBackend record ~ BaseBackend backend) =>
                Key record -> ReaderT backend m (Bool)
chkExistsId ident = get ident >>= return . isJust

-- Fetches an entity by ID, and requires the entity to exist, otherwise bails
-- out with an invalid argument
requireId :: (MonadIO m, PersistUniqueRead backend, PersistEntity record, 
  MonadHandler m, PersistEntityBackend record ~ BaseBackend backend) =>
    Text -> Key record -> ReaderT backend m (record)
requireId entName key = do
  ment <- get key
  case ment of
    Just ent -> pure ent
    Nothing -> invalidArgs ["Invalid " <> entName <> "!"]

success :: Text -> Value
success m = toJSON $ (msg200 m :: Message Nil)

maybeParam :: (MonadHandler m, Read a) => Text -> m (Maybe a)
maybeParam pname = do
  mp <- lookupGetParam pname
  case maybe Nothing (Just . reads . unpack) mp of
    -- Parameter does not exit
    Nothing -> pure Nothing
    -- We have it
    Just [(val, "")] -> pure $ Just val
    -- Malformed parameter
    Just _ -> invalidArgs ["Invalid parameter `" <> pname <> "`"]

requireParam :: (MonadHandler m, Read a) => Text -> m a
requireParam pname = do
  mp <- maybeParam pname
  case mp of
    Nothing -> invalidArgs ["Missing parameter `" <> pname <> "`"]
    Just val -> pure val

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
