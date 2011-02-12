{-# LANGUAGE TemplateHaskell, OverloadedStrings, QuasiQuotes #-}
module Handler.Handlers where

import DnP
import Control.Monad
import Control.Applicative
import Control.Arrow

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Data.Maybe (fromMaybe)


import Data.List (sortBy)
import Data.Ord (comparing)

import Handler.Commands
import Handler.Util



getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    h2id <- newIdent
    defaultLayout $ do
        setTitle "Dice and Paper Online"
        addWidget $(widgetFile "homepage")


getCheckInR :: Handler RepJson
getCheckInR = do
  (uid,u) <- requireAuth
  --liftIO $ putStrLn $ "checkIn from " ++ show uid
  table <- getTable uid
  chan <- maybe (invalidArgs ["Invalid User ID"]) (return.channel) $ M.lookup uid (clients table)
  -- blocks until we have something to send
  msg <- liftIO . atomically $ readTChan chan
  case msg of
    MessageChat s c -> do
      jsonToRepJson $ zipJson ["type", "sender", "content"] ["chat",s,c]
    MessageWhisper s c -> do
      jsonToRepJson $ zipJson ["type", "sender", "content"] ["whisper",s,c]
    MessageBoard ts -> do
      jsonToRepJson $ jsonMap [("type", jsonScalar "board"), 
                               ("tokens", jsonList $ map (\t -> zipJson ["x","y","image","name"] 
                                                                        $ map ($ t) [show.tokenX, show.tokenY, file, tokenName]) 
                                                         ts
                                )]
    MessageVars vs -> do
      jsonToRepJson $ jsonMap [("type", jsonScalar "vars"),
                               ("vars", jsonList $ map (\(c,cvs) -> jsonMap [("nick", jsonScalar c), ("vars", jsonPairs cvs)]) vs)]
    MessageJunk -> jsonToRepJson $ jsonMap [("type", jsonScalar "junk")]
    MessageColor cs -> jsonToRepJson $ jsonMap [("type", jsonScalar "colors"), ("colors", jsonPairs cs)]
    MessageCommands cmds -> jsonToRepJson $ jsonMap [("type", jsonScalar "commands"), ("commands", jsonPairs cmds)]


jsonPairs xs = jsonList $ map (\(x,y) -> jsonList [jsonScalar x, jsonScalar y]) xs


postSayR :: Handler RepJson
postSayR = do
  (uid,u) <- requireAuth
  mmsg  <- lookupPostParam "message"
  let msg  = fromMaybe "" mmsg -- blank messages won't get sent
      nick = userNick u
  --liftIO $ putStrLn $ nick ++ " (" ++ show uid ++ ") said: " ++ msg
  res <- case msg of
           "" -> return $ ResponseSuccess
           _  -> runCommand uid u nick msg []

  case res of
    ResponseSuccess -> jsonToRepJson $ zipJson ["status"] ["success"]
    ResponsePrivate s -> jsonToRepJson $ zipJson ["status","message"] ["private",s]


-- handles the main logic on an incoming chat message. does the actual feeding of clients with data
runCommand :: UserId -> User -> String -> String -> [String] -> Handler CommandResponse
runCommand _ _ _ []    _ = return ResponseSuccess -- do nothing on empty messages
runCommand _ _ _ ['/'] _ = return ResponseSuccess -- do nothing on just a slash
runCommand uid u nick ('/':msg) prevcmds = do
  let (cmd:args) = words msg -- guaranteed to be at least one by the ['/'] case above
  when (not . null . filter (==cmd) $ prevcmds) $ sendPrivate "Loop detected. Illegal command."
  let mf = M.lookup cmd commandMap
  case mf of
    Just f  -> do
      mc <- getClientById uid
      case fmap muted mc of
        Nothing    -> f uid u nick cmd args -- happens when you're not in a table, and you can't be muted then.
        Just False -> f uid u nick cmd args
        Just True  | null (filter (==cmd) mutedWhitelist) -> return $ ResponsePrivate $ "The command " ++ cmd ++ " is not allowed while muted."
                   | otherwise                            -> f uid u nick cmd args
    Nothing -> do 
      -- retrieve the user's saved commands from the DB
      mc <- getClientById uid
      case join $ fmap (M.lookup cmd . commands) mc of
        Nothing    -> return $ ResponsePrivate $ "Unknown command: '" ++ cmd ++ "'"
        Just value -> runCommand uid u nick value (cmd:prevcmds)

runCommand uid u nick msg _ = do
  mc <- getClientById uid
  case fmap muted mc of
    Just True -> return $ ResponsePrivate "You are currently muted and cannot speak."
    _         -> send uid nick msg


-- these are the commands it's legal to use while muted. it should be commands that reach the GM only, or commands that only respond privately.
mutedWhitelist = ["gmwhisper", "gmw", "quit", "proll", "pr", "gmroll", "gmr", "define", "undef", "who", "tables", "tokens", "help", "muted"]


getTableR :: Handler RepHtml
getTableR = do
  (uid, u) <- requireAuth
  mc <- getClientById uid
  case mc of
    Nothing -> return ()
    Just c  -> do
      t <- getTable uid
      liftIO . atomically $ do
        sequence_ . replicate 3 $ writeTChan (channel c) MessageJunk
        sendVarUpdate t UpdateAll
        sendBoardUpdate t (UpdateUser uid)
        sendColorUpdate t UpdateAll
        sendCommandUpdate t (UpdateUser uid)
  defaultLayout $ do
    setTitle "Dice and Paper Online - Table"
    addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
    addScript $ StaticR $ StaticRoute ["table.js"] []
    $(widgetFile "table")




gridWidget :: Widget ()
gridWidget = [$hamlet|\
  <table .grid>
    $forall row <- tableContent
      \^{row}
  \
|]
  where tableContent = map tableRows [0..gridRows-1]
        tableRows r  = let row = map (tableSquare r) [0..gridCols-1] in [$hamlet|\
          <tr .grid>
            $forall square <- row
              \^{square}
          \
|]
        tableSquare r c = let squareId = "sq_" ++ show c ++ "x" ++ show r in [$hamlet| <td id="#{squareId}" .grid>
|]





getManualR :: Handler RepHtml
getManualR = defaultLayout $ do
  setTitle "Dice and Paper Online - Manual"
  addHamlet $(hamletFile "manual")
  addCassius $(cassiusFile "manual")

getSyntaxR :: String -> Handler RepHtml
getSyntaxR cmd = case M.lookup cmd helpMap of
                  Nothing -> notFound
                  Just help -> defaultLayout [$hamlet|\
                    <h1>Syntax for '#{cmd}'
                    <p>#{snd help} |]



