module Foundation where

import Prelude
import Data.Text hiding (null)
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.BrowserId
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Nic
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist
import Database.Persist.Sql (SqlBackend)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Text.Hamlet (hamletFile)
import Yesod.Core.Types (Logger)

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

plural :: Int -> String -> String -> String
plural 1 x _ = x
plural _ _ y = y

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuth

        extra <- getExtra
        let pagename = extraPagename extra

        categories <- runDB $ selectList [] [Asc CategoryName]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(combineStylesheets 'StaticR
                [ css_normalize_css
                , css_bootstrap_css
                , css_prism_css
                ])
            $(widgetFile "default-layout")
        scripts <- widgetToPageContent
            $(combineScripts 'StaticR
                [ js_jquery_js
                , js_bootstrap_js
                , js_prism_js ])

        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    makeLogger = return . appLogger

    maximumContentLength _ (Just NewUploadR) = Just $ 16 * 1024 * 1024
    maximumContentLength _ _ = Just $ 2 * 1024 * 1024

    isAuthorized BlogR True = adminAuthorized
    isAuthorized (EntryR _) True = allUsersAuthorized
    isAuthorized NewEntryR _ = adminAuthorized
    isAuthorized (EditEntryR _) _ = adminAuthorized
    isAuthorized (DeleteEntryR _) _ = adminAuthorized
    isAuthorized (DeleteCommentR commentId) _ = do
      mauth <- maybeAuth
      case mauth of
        Nothing -> return AuthenticationRequired
        Just (Entity userId user)
          | isAdmin user -> return Authorized
          | otherwise    -> do
            commentAuthor <- runDB $ do
                comment <- get404 commentId
                return $ commentUser comment
            if commentAuthor == userId
              then return Authorized
              else unauthorizedI MsgAdminAndAuthorAuthorizedDeleteComment
    isAuthorized BlogR                 _ = allAuthorized
    isAuthorized (EntryR _)            _ = allAuthorized
    isAuthorized CategoriesR           _ = adminAuthorized
    isAuthorized (EditCategoryR   _)   _ = adminAuthorized
    isAuthorized (DeleteCategoryR _)   _ = adminAuthorized
    isAuthorized UsersR                _ = adminAuthorized
    isAuthorized (EditUserR _)         _ = adminAuthorized
    isAuthorized (StaticR _)           _ = allAuthorized
    isAuthorized (AuthR _)             _ = allAuthorized
    isAuthorized FaviconR              _ = allAuthorized
    isAuthorized RobotsR               _ = allAuthorized
    isAuthorized HomeR                 _ = allAuthorized
    isAuthorized RssFeedR              _ = allAuthorized
    isAuthorized AtomFeedR             _ = allAuthorized
    isAuthorized (CategoryR _)         _ = allAuthorized
    isAuthorized (CategoryRssFeedR _)  _ = allAuthorized
    isAuthorized (CategoryAtomFeedR _) _ = allAuthorized
    isAuthorized UploadsR              _ = adminAuthorized
    isAuthorized NewUploadR            _ = adminAuthorized
    isAuthorized (UploadR _)           _ = allAuthorized
--     isAuthorized _ _ = do
--         return Authorized

allAuthorized :: Monad m => m AuthResult
allAuthorized = return Authorized

allUsersAuthorized :: Handler AuthResult
allUsersAuthorized = do
    mauth <- maybeAuth
    case mauth of
        Nothing -> return AuthenticationRequired
        Just _  -> return Authorized

adminAuthorized :: Handler AuthResult
adminAuthorized = do
    mauth <- maybeAuth
    case mauth of
      Nothing -> return AuthenticationRequired
      Just (Entity _ user)
        | isAdmin user -> return   Authorized
        | otherwise    -> unauthorizedI MsgNotAnAdmin

isAdmin :: User -> Bool
isAdmin user = userAdmin user

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = do
        extra <- getExtra
        runDB $ do
            x <- getBy $ UniqueUser $ credsIdent creds
            case x of
                Just (Entity uid _) -> return $ Just uid
                Nothing -> do
                    fmap Just $ insert User
                        { userIdent = credsIdent creds
                        , userPassword = Nothing
                        , userAdmin    = (extraAdmin extra == credsIdent creds)
                        }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [ authBrowserId def
                    , authOpenId OPLocal [] ]

    authHttpManager = httpManager

instance YesodAuthPersist App where
    type AuthEntity App = User

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- To be able to use the Nic HTML editor.
instance YesodNic App

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
