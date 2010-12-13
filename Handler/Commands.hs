{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
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

import System.Random
import Text.Parsec

import Data.List

sum' = foldl' (+) 0

commandMap = M.fromList [ ("nick", cmdNick)
                        , ("host", cmdHost)
                        , ("join", cmdJoin)
                        , ("debug", cmdDebug)
                        , ("roll", cmdRoll)
                        , ("r",    cmdRoll)
                        , ("d3",   cmdRoll)
                        , ("d4",   cmdRoll)
                        , ("d6",   cmdRoll)
                        , ("d8",   cmdRoll)
                        , ("d10",  cmdRoll)
                        , ("d12",  cmdRoll)
                        , ("d20",  cmdRoll)
                        , ("d100", cmdRoll)
                        , ("d%",   cmdRoll)
                        ]

-- user id, user, nick, command, args
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
        modifyTVar (userTables dnp) $ M.insert uid name
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


cmdDebug :: Command
cmdDebug _ _ _ _ _ = do
  dnp <- getYesod
  (uts,ts) <- liftIO . atomically $ do
                uts <- readTVar (userTables dnp)
                ts  <- readTVar (tables dnp)
                return (uts,ts)
  liftIO $ print uts
  liftIO $ print (M.map (\(Table cs p) -> M.keys cs) ts)
  return $ ResponseSuccess



syntaxRoll :: String
syntaxRoll = "Syntax: /roll AdB+C\nExamples: /roll 2d6-2   /roll d20   /roll 1d8+3"

cmdRoll :: Command
cmdRoll uid u nick "d3"   _ = cmdRoll uid u nick "roll" ["1d3"]
cmdRoll uid u nick "d4"   _ = cmdRoll uid u nick "roll" ["1d4"]
cmdRoll uid u nick "d6"   _ = cmdRoll uid u nick "roll" ["1d6"]
cmdRoll uid u nick "d8"   _ = cmdRoll uid u nick "roll" ["1d8"]
cmdRoll uid u nick "d10"  _ = cmdRoll uid u nick "roll" ["1d10"]
cmdRoll uid u nick "d12"  _ = cmdRoll uid u nick "roll" ["1d12"]
cmdRoll uid u nick "d20"  _ = cmdRoll uid u nick "roll" ["1d20"]
cmdRoll uid u nick "d100" _ = cmdRoll uid u nick "roll" ["1d100"]
cmdRoll uid u nick "d%"   _ = cmdRoll uid u nick "roll" ["1d100"]
cmdRoll uid u nick _ []     = return $ ResponsePrivate syntaxRoll
cmdRoll uid u nick _ (x:_)  = do
    case parse parseDice "" (case x of ('d':_) -> '1':x; _ -> x) of
      Left _ -> return $ ResponsePrivate syntaxRoll
      Right (a,b,c) | a <= 0 -> return $ ResponsePrivate "Number of dice cannot be less than 1."
                    | b <= 0 -> return $ ResponsePrivate "Size of dice cannot be less than 1."
                    | otherwise -> do
                        rolls <- replicateM a $ liftIO (randomRIO (1,b))
                        let total = sum' rolls + c
                        send uid serverName $ nick ++ " rolled " ++ show a ++ "d" ++ show b ++ (if c < 0 then show c else "+" ++ show c) ++ " and got " ++ show total
  where parseDice = do
          a <- many1 digit
          char 'd'
          b <- many1 digit
          mc1 <- optionMaybe $ char '+' >> many1 digit
          mc2 <- optionMaybe $ char '-' >> many1 digit
          let c = case (mc1,mc2) of
                    (Nothing,Nothing) -> 0
                    (Just x, Nothing) -> read x
                    (Nothing,Just y)  -> read y
          return ((read a, read b, c) :: (Int,Int,Int))


