{-# LANGUAGE TemplateHaskell, OverloadedStrings, ExistentialQuantification #-}
module Handler.Util where

import DnP

import qualified Data.Map as M
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import Control.Arrow ((***))
import Data.Maybe
import Control.Applicative


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


-- sends a message to everyone from inside an STM transaction
rawSend :: Table -> String -> String -> STM ()
rawSend t nick msg = mapM_ (flip writeTChan (MessageChat nick msg) . channel) (M.elems (clients t))

-- sends a message to everyone
send :: UserId -> String -> String -> Handler CommandResponse
send uid nick msg = do
  table <- getTable uid
  liftIO . atomically $ rawSend table nick msg
  return ResponseSuccess


data UpdateWhom = UpdateAll | UpdateUser UserId

sendBoardUpdate :: Table -> UpdateWhom -> STM ()
sendBoardUpdate t UpdateAll = mapM_ (sendBoardUpdate t . UpdateUser) $ M.keys (clients t)
sendBoardUpdate t (UpdateUser uid) = do
  case M.lookup uid (clients t) of
    Nothing -> error "can't happen"
    Just (Client { channel = chan }) -> writeTChan chan $ MessageBoard (concatMap M.elems $ M.elems (board t))


modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar tv f = do
  x <- readTVar tv
  writeTVar tv $ f x


maybeRead :: forall a . Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x,[])] -> Just x
  _   -> Nothing



updateLastToken :: UserId -> String -> Handler ()
updateLastToken uid name = updateClient uid $ \c -> Just c { lastToken = Just name }

updateClient :: UserId -> (Client -> Maybe Client) -> Handler ()
updateClient uid f = updateTable uid $ \t -> Just t { clients = M.update f uid (clients t) }



sendPrivate :: String -> Handler a
sendPrivate s = do
  json <- jsonToRepJson $ zipJson ["status","message"] ["private",s]
  sendResponse json
  return undefined



zipJson x y = jsonMap $ map (id *** jsonScalar) $ zip x y

updateBoard :: UserId -> String -> (Maybe Token -> Maybe Token) -> Handler CommandResponse
updateBoard uid name f = do
  t <- getTable uid
  updateTable uid $ \t -> let subboard = fromMaybe M.empty $ M.lookup uid (board t)
                          in  Just t { board = M.insert uid (M.alter f name subboard) (board t) }
  t' <- getTable uid
  liftIO . atomically $ sendBoardUpdate t' UpdateAll
  updateLastToken uid name
  return ResponseSuccess


getLastToken :: UserId -> String -> Handler String
getLastToken uid syntax = do
  t <- getTable uid
  case M.lookup uid (clients t) >>= lastToken of
    Nothing -> sendPrivate syntax
    Just lt -> return lt

