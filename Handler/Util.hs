{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Util where

import DnP

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan



data CommandResponse = ResponseSuccess | ResponsePrivate String


-- name by which the server appears in chat messages
serverName = "::!:" -- will appear as, eg.:  ::!:: oldnick is now known as newnick


getTable :: UserId -> Handler Table
getTable uid = do
  dnp <- getYesod
  mtid <- liftIO . atomically $ readTVar (userTables dnp) >>= return . M.lookup uid
  tid <- case mtid of
           Nothing  -> invalidArgs ["Not in a table yet"]
           Just tid -> return tid
  mtable <- liftIO . atomically $ readTVar (tables dnp) >>= return . M.lookup tid
  maybe (invalidArgs ["Invalid table ID"]) return mtable


-- sends from inside an STM transaction
rawSend :: Table -> String -> String -> STM ()
rawSend t nick msg = mapM_ (flip writeTChan (Message nick msg)) (M.elems (clients t))

send :: UserId -> String -> String -> Handler CommandResponse
send uid nick msg = do
  table <- getTable uid
  liftIO . atomically $ rawSend table nick msg
  return ResponseSuccess



