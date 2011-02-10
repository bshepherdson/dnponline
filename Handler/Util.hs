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

import Data.List
import Data.Char (toLower)


data CommandResponse = ResponseSuccess | ResponsePrivate String


gridCols, gridRows :: Int
gridCols = 30
gridRows = 15


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


-- userId of sender, userId of receiver, sender nick, message
sendTo :: UserId -> UserId -> Message -> Handler ()
sendTo sendId recvId msg = do
  t <- getTable sendId
  recv <- case M.lookup recvId (clients t) of
            Nothing -> sendPrivate $ "Error: userId " ++ showPersistKey recvId ++ " not found on this table."
            Just x  -> return x
  liftIO . atomically $ writeTChan (channel recv) msg
  return ()




data UpdateWhom = UpdateAll | UpdateUser UserId

sendBoardUpdate :: Table -> UpdateWhom -> STM ()
sendBoardUpdate t UpdateAll = mapM_ (sendBoardUpdate t . UpdateUser) $ M.keys (clients t)
sendBoardUpdate t (UpdateUser uid) = do
  case M.lookup uid (clients t) of
    Nothing -> error "can't happen"
    Just (Client { channel = chan }) -> writeTChan chan $ MessageBoard (concatMap M.elems $ M.elems (board t))


sendVarUpdate :: Table -> UpdateWhom -> STM ()
sendVarUpdate t whom = sendVarUpdate' t whom . concatMap (\c -> map ((,) (clientNick c)) (M.assocs (vars c))) . M.elems . clients $ t
  where sendVarUpdate' t UpdateAll allvars = mapM_ (\uid -> sendVarUpdate' t (UpdateUser uid) allvars) $ M.keys (clients t)
        sendVarUpdate' t (UpdateUser uid) allvars = do
          case M.lookup uid (clients t) of
            Nothing -> error "can't happen"
            Just (Client { channel = chan }) -> writeTChan chan $ MessageVars allvars


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
  return undefined -- unreachable

sendSuccess :: Handler a
sendSuccess = do
  json <- jsonToRepJson $ zipJson ["status"] ["success"]
  sendResponse json
  return undefined -- unreachable

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


-- userId of requester, target nick
getClientByNick :: UserId -> String -> Handler (UserId, Client)
getClientByNick uid target = do
  t <- getTable uid
  let target' = map toLower target
      cs      = M.assocs $ clients t
  case filter ((target' `isPrefixOf`) . map toLower . clientNick . snd) cs of
    [found] -> return found
    [] -> sendPrivate $ "No user with nickname " ++ target ++ " was found. Check /who for a list of users in the table."
    xs -> sendPrivate $ "Ambiguous nickname, multiple matches found: " ++ intercalate "," (map (clientNick . snd) xs)


getClientById :: UserId -> Handler (Maybe Client)
getClientById uid = do
  t <- getTable uid
  return $ M.lookup uid (clients t)


getTokenAt :: Int -> Int -> Table -> Maybe Token
getTokenAt x y t = let tokens = concatMap M.elems $ M.elems (board t)
                       matches = filter (\(Token { tokenX=tx, tokenY=ty }) -> tx==x && ty==y ) tokens
                   in  case matches of
                         []    -> Nothing
                         (x:_) -> Just x


-- removes the given user from his table
removeClient :: UserId -> Handler ()
removeClient uid = do
  dnp <- getYesod
  liftIO . atomically $ do
    ut <- readTVar $ userTables dnp
    let mtid = M.lookup uid ut
    case mtid of
      Nothing -> return ()
      Just tid -> do
        writeTVar (userTables dnp) $ M.delete uid ut
        ts <- readTVar $ tables dnp
        let mt = M.lookup tid ts
        case mt of 
          Nothing -> return ()
          Just t  -> do
            mt' <- case (gm t == uid, M.size (clients t)) of
                     (_, 1) -> do 
                        modifyTVar (tables dnp) (M.delete tid)
                        return Nothing
                     (True,_) -> let cs = M.delete uid (clients t)
                                 in  return $ Just t { gm = head (M.keys cs), clients = cs }
                     _        -> return $ Just t { clients = M.delete uid (clients t) }
            case mt' of
              Nothing -> return ()
              Just t' -> modifyTVar (tables dnp) (M.insert tid t')

