{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Handlers where

import DnP
import Control.Monad
import Control.Applicative

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan



getRootR :: Handler RepHtml
getRootR = do
    mu <- maybeAuth
    defaultLayout $ do
        h2id <- newIdent
        setTitle "Dice and Paper Online"
        addWidget $(widgetFile "homepage")


getCheckR :: Handler Chat RepJson
getCheckR = do
  (uid,u) <- requireAuth
  dnp <- getYesod
  mtable <- liftIO . atomically $ readTVar (clientTables dnp) >>= return . M.lookup uid
  tid <- case mtable of
           Nothing  -> invalidArgs ["Not in a table yet"]
           Just tid -> return tid




  client <- do
    c  invalidArgs ["No client value in Check request"]
      Just c' -> return $ read c'
  cs <- liftIO . atomically $ readTVar clients
  chan  invalidArgs ["Bad client value"]
            Just ch -> return ch
  -- block until there's something there
  first <- liftIO . atomically $ readTChan chan
  let Message s c = first
  jsonToRepJson $ zipJson ["sender", "content"] [s,c]



zipJson x y = jsonMap $ map (id *** (jsonScalar.string)) $ zip x y



