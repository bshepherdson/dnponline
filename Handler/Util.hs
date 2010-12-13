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


updateTable :: UserId -> (Table -> Maybe Table) -> Handler ()
updateTable uid f = do
  dnp <- getYesod
  mtid <- liftIO . atomically $ readTVar (userTables dnp) >>= return . M.lookup uid
  tid <- case mtid of
           Nothing  -> invalidArgs ["Not in a table yet"]
           Just tid -> return tid
  mtable <- liftIO . atomically $ readTVar (tables dnp) >>= return . M.lookup tid
  case mtable of
    Nothing -> invalidArgs ["Invalid table ID"]
    Just t  -> liftIO . atomically $ modifyTVar (tables dnp) (M.update f tid)


-- sends from inside an STM transaction
rawSend :: Table -> String -> String -> STM ()
rawSend t nick msg = mapM_ (flip writeTChan (MessageChat nick msg) . channel) (M.elems (clients t))

send :: UserId -> String -> String -> Handler CommandResponse
send uid nick msg = do
  table <- getTable uid
  liftIO . atomically $ rawSend table nick msg
  return ResponseSuccess


-- if uid is Nothing, sends to everyone
-- otherwise to just the one user
sendBoardUpdate :: Table -> Maybe UserId -> STM ()
sendBoardUpdate t Nothing    = mapM_ (sendBoardUpdate t . Just) $ M.keys (clients t)
sendBoardUpdate t (Just uid) = do
  case M.lookup uid (clients t) of
    Nothing -> error "can't happen"
    Just (Client { channel = chan }) -> writeTChan chan $ MessageBoard (concatMap M.elems $ M.elems (board t))


modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = do
  x <- readTVar tv
  writeTVar tv $ f x


