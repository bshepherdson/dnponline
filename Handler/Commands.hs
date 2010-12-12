{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Handler.Commands (
    commandMap
  , CommandResponse (..)
) where

import DnP
import Handler.Util
import Control.Monad
import Control.Applicative

import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan


commandMap = M.fromList [ ("nick", cmdNick)
                        , ("host", cmdHost)
                        , ("join", cmdJoin)
                        ]

type Command = UserId -> User -> String -> String -> [String] -> Handler CommandResponse



cmdNick :: Command
cmdNick uid u nick cmd []   = return $ ResponsePrivate "Syntax: /nick <new nickname>"
cmdNick uid u nick cmd args = do
  let newnick = unwords args
  setSession "nick" newnick
  send uid serverName $ nick ++ " is now known as " ++ newnick

cmdHost :: Command
cmdHost uid u nick cmd [name,pass] = do
  dnp <- getYesod
  liftIO . atomically $ do
    ts <- readTVar $ tables dnp
    case M.lookup name ts of
      Just _  -> return $ ResponsePrivate $ "Table " ++ name ++ " already exists."
      Nothing -> do
        chan <- newTChan
        let t = Table (M.singleton uid chan) pass
        writeTVar (tables dnp) $ M.insert name t ts
        return $ ResponsePrivate $ "Table " ++ name ++ " successfully created."
  

cmdJoin :: Command
cmdJoin uid u nick cmd [name,pass] = do
  dnp <- getYesod
  liftIO . atomically $ do
    ut <- readTVar $ userTables dnp
    ts <- readTVar $ tables dnp
    case M.lookup uid ut of
      Just t  -> return $ ResponsePrivate $ "You are already in the table " ++ t ++ ". /part first."
      Nothing -> do
        case M.lookup name ts of
          Nothing -> return $ ResponsePrivate $ "Table " ++ name ++ " does not exist."
          Just t  -> do
            case pass == password t of
              False -> return $ ResponsePrivate $ "Bad password for table " ++ name
              True  -> do
                chan <- newTChan
                let t'  = t { clients = M.insert uid chan (clients t) }
                    ut' = M.insert uid name ut
                    ts' = M.insert name t' ts
                writeTVar (userTables dnp) ut'
                writeTVar (tables dnp) ts'
                rawSend t serverName $ nick ++ " has joined the table." -- deliberately t, sends to everyone else, not the new client
                return $ ResponsePrivate $ "Successfully joined table " ++ name
cmdJoin uid u nick cmd _ = return $ ResponsePrivate $ "Syntax: /join <table name> <password>"


