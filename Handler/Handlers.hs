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
  liftIO $ putStrLn $ "checkIn from " ++ show uid
  table <- getTable uid
  chan <- maybe (invalidArgs ["Invalid User ID"]) (return.channel) $ M.lookup uid (clients table)
  -- blocks until we have something to send
  msg <- liftIO . atomically $ readTChan chan
  case msg of
    MessageChat s c -> do
      liftIO $ putStrLn $ "Responding to " ++ show uid ++ " with " ++ show (s,c)
      jsonToRepJson $ zipJson ["type", "sender", "content"] ["chat",s,c]
    MessageWhisper s c -> do
      liftIO $ putStrLn $ "Whispering to " ++ show uid ++ " with " ++ show (s,c)
      jsonToRepJson $ zipJson ["type", "sender", "content"] ["whisper",s,c]
    MessageBoard ts -> do
      liftIO $ putStrLn $ "Sending Tokens to " ++ show uid
      jsonToRepJson $ jsonMap [("type", jsonScalar "board"), 
                               ("tokens", jsonList $ map (\t -> zipJson ["x","y","image","name"] 
                                                                        $ map ($ t) [show.tokenX, show.tokenY, file, tokenName]) 
                                                         ts
                                )]
    MessageVars vs -> do
      let sorted = sortBy (comparing fst) . sortBy (comparing (fst.snd)) $ vs  -- sorted by nick and then by var name
      liftIO $ putStrLn $ "Sending vars to " ++ show uid
      jsonToRepJson $ jsonMap [("type", jsonScalar "vars"),
                               ("vars", jsonList $ map (\v -> zipJson ["nick","var","value"]
                                                                      $ map ($ v) [fst, fst.snd, snd.snd])
                                                       vs
                               )]


getSayR :: Handler RepJson
getSayR = do
  (uid,u) <- requireAuth
  mmsg  <- lookupGetParam "message"
  let msg  = fromMaybe "" mmsg -- blank messages won't get sent
      nick = userNick u
  liftIO $ putStrLn $ nick ++ " (" ++ show uid ++ ") said: " ++ msg
  res <- case msg of
           "" -> return $ ResponseSuccess
           _  -> runCommand uid u nick msg

  case res of
    ResponseSuccess -> jsonToRepJson $ zipJson ["status"] ["success"]
    ResponsePrivate s -> jsonToRepJson $ zipJson ["status","message"] ["private",s]


-- handles the main logic on an incoming chat message. does the actual feeding of clients with data
runCommand :: UserId -> User -> String -> String -> Handler CommandResponse
runCommand _ _ _ []    = return ResponseSuccess -- do nothing on empty messages
runCommand _ _ _ ['/'] = return ResponseSuccess -- do nothing on just a slash
runCommand uid u nick ('/':msg) = do
  let (cmd:args) = words msg -- guaranteed to be at least one by the ['/'] case above
  let mf = M.lookup cmd commandMap
  case mf of
    Just f  -> f uid u nick cmd args
    Nothing -> do 
      -- retrieve the user's saved commands from the DB
      cmds <- runDB $ selectList [CommandUserEq uid, CommandNameEq cmd] [] 0 0
      case cmds of
        []  -> return $ ResponsePrivate $ "Unknown command: '" ++ cmd ++ "'"
        [(_,Command _ _ usrcmd)] -> runCommand uid u nick usrcmd
        _   -> return $ ResponsePrivate $ "ERROR: Multiple possible commands returned. Can't happen."

runCommand uid u nick msg = send uid nick msg



getTableR :: Handler RepHtml
getTableR = defaultLayout $ do
  setTitle "Dice and Paper Online - Table"
  addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js"
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



