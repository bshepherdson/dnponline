{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module DnP
    ( DnP (..)
    , DnPRoute (..)
    , resourcesDnP
    , Handler
    , Widget
    , maybeAuth
    , requireAuth
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , AuthRoute (..)
    , Table (..)
    , TableId (..)
    , Message (..)
    , Token (..)
    , Client (..)
    ) where

import Yesod
import Yesod.Helpers.Static
import Yesod.Helpers.Auth
import Yesod.Helpers.Auth.OpenId
import Yesod.Helpers.Auth.Email
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Web.Routes.Site (Site (formatPathSegments))
import Database.Persist.GenericSql
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Model
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)



import Control.Concurrent.STM
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import qualified Data.Map as M
import Data.Map (Map)


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data DnP = DnP
    { getStatic    :: Static -- ^ Settings for static file serving.
    , connPool     :: Settings.ConnectionPool -- ^ Database connection pool.
    , userTables   :: TVar (Map UserId TableId)
    , tables       :: TVar (Map TableId Table)
    }

type TableId = String

data Table = Table
    { clients :: Map UserId Client
    , password :: String
    , gm :: UserId -- initially the host
    , board :: Map UserId (Map String Token)
    }

data Client = Client
    { channel :: TChan Message
    , clientNick :: String
    , lastToken :: Maybe String
    , vars :: Map String String
    }

data Token = Token
    { tokenX :: Int
    , tokenY :: Int
    , file :: FilePath
    , tokenName :: String
    }
  deriving (Show)

data Message = MessageChat String String -- sender, message
             | MessageBoard [Token]
             | MessageWhisper String String -- sender, message
             | MessageVars [(String,(String,String))] -- nick, var name, value


-- | A useful synonym; most of the handler functions in your application
-- will need to be of this type.
type Handler = GHandler DnP DnP

-- | A useful synonym; most of the widgets functions in your application
-- will need to be of this type.
type Widget = GWidget DnP DnP

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://docs.yesodweb.com/book/web-routes-quasi/
--
-- This function does three things:
--
-- * Creates the route datatype DnPRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route DnP = DnPRoute
-- * Creates the value resourcesDnP which contains information on the
--   resources declared below. This is used in Controller.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- DnP. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the DnPRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "DnP" [$parseRoutes|
/static StaticR Static getStatic
/auth   AuthR   Auth   getAuth

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/ RootR GET

/check CheckInR GET
/say   SayR   GET
/table TableR  GET
|]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod DnP where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) =
        Just $ uncurry (joinPath a Settings.staticroot) $ format s
      where
        format = formatPathSegments ss
        ss :: Site StaticRoute (String -> Maybe (GHandler Static DnP ChooseRep))
        ss = getSubSite
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

-- How to run database actions.
instance YesodPersist DnP where
    type YesodDB DnP = SqlPersist
    runDB db = fmap connPool getYesod >>= Settings.runConnectionPool db

instance YesodAuth DnP where
    type AuthId DnP = UserId

    -- Where to send a user after successful login
    loginDest _ = TableR
    -- Where to send a user after logout
    logoutDest _ = RootR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (uid, _) -> return $ Just uid
            Nothing -> do
                  fmap Just $ insert $ User (credsIdent creds) Nothing ("user" ++ take 10 (credsIdent creds))

    showAuthId _ = showIntegral
    readAuthId _ = readIntegral

    authPlugins = [ authOpenId
                  , authEmail
                  ]

instance YesodAuthEmail DnP where
    type AuthEmailId DnP = EmailId

    showAuthEmailId _ = showIntegral
    readAuthEmailId _ = readIntegral

    addUnverified email verkey =
        runDB $ insert $ Email email Nothing $ Just verkey
    sendVerifyEmail email _ verurl = renderSendMail Mail
        { mailHeaders =
            [ ("From", "noreply")
            , ("To", email)
            , ("Subject", "Verify your email address")
            ]
        , mailParts = [[textPart, htmlPart]]
        }
      where
        textPart = Part
            { partType = "text/plain; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                          $ Data.Text.Lazy.pack $ unlines
                [ "Please confirm your email address by clicking on the link below."
                , ""
                , verurl
                , ""
                , "Thank you"
                ]
            }
        htmlPart = Part
            { partType = "text/html; charset=utf-8"
            , partEncoding = None
            , partFilename = Nothing
            , partContent = renderHtml [$hamlet|
%p Please confirm your email address by clicking on the link below.
%p
    %a!href=$verurl$ $verurl$
%p Thank you
|]
            }
    getVerifyKey = runDB . fmap (join . fmap emailVerkey) . get
    setVerifyKey eid key = runDB $ update eid [EmailVerkey $ Just key]
    verifyAccount eid = runDB $ do
        me <- get eid
        case me of
            Nothing -> return Nothing
            Just e -> do
                let email = emailEmail e
                case emailUser e of
                    Just uid -> return $ Just uid
                    Nothing -> do
                        uid <- insert $ User email Nothing (takeWhile (/='@') email)
                        update eid [EmailUser $ Just uid, EmailVerkey Nothing]
                        return $ Just uid
    getPassword = runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = runDB $ update uid [UserPassword $ Just pass]
    getEmailCreds email = runDB $ do
        me <- getBy $ UniqueEmail email
        case me of
            Nothing -> return Nothing
            Just (eid, e) -> return $ Just EmailCreds
                { emailCredsId = eid
                , emailCredsAuthId = emailUser e
                , emailCredsStatus = isJust $ emailUser e
                , emailCredsVerkey = emailVerkey e
                }
    getEmail = runDB . fmap (fmap emailEmail) . get
