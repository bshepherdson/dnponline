{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
module Handler.Commands (
    commandMap
  , CommandResponse (..)
) where

import DnP
import Handler.Util
import Control.Monad
import Control.Applicative
import Data.Maybe

import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

import System.Random
import Text.ParserCombinators.Parsec

import Data.List

sum' = foldl' (+) 0

commandMap = M.fromList [ ("nick", cmdNick)
                        , ("host", cmdHost)
                        , ("join", cmdJoin)
                        , ("debug", cmdDebug)

                        -- dice commands
                        , ("roll", cmdRoll)
                        , ("r",    cmdRoll)
                        , ("proll",cmdRoll)
                        , ("pr",   cmdRoll)
                        , ("d3",   cmdRoll)
                        , ("d4",   cmdRoll)
                        , ("d6",   cmdRoll)
                        , ("d8",   cmdRoll)
                        , ("d10",  cmdRoll)
                        , ("d12",  cmdRoll)
                        , ("d20",  cmdRoll)
                        , ("d100", cmdRoll)
                        , ("d%",   cmdRoll)

                        -- board commands
                        , ("place",  cmdPlace)
                        , ("move",   cmdMove)
                        , ("delete", cmdRemove)
                        , ("remove", cmdRemove)
                        , ("clear",  cmdClear)
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
        let t = Table (M.singleton uid (Client chan Nothing)) pass uid M.empty
        writeTVar (tables dnp) $ M.insert name t ts
        modifyTVar (userTables dnp) $ M.insert uid name
        return $ ResponsePrivate $ "Table " ++ name ++ " successfully created."
  

cmdJoin :: Command
cmdJoin uid u nick cmd [name,pass] = do
  dnp <- getYesod
  liftIO . atomically $ do
    userTable <- readTVar $ userTables dnp
    ts <- readTVar $ tables dnp
    case M.lookup uid userTable of
      Just t  -> return $ ResponsePrivate $ "You are already in the table " ++ t ++ ". /part first."
      Nothing -> do
        case M.lookup name ts of
          Nothing -> return $ ResponsePrivate $ "Table " ++ name ++ " does not exist."
          Just t  -> do
            case pass == password t of
              False -> return $ ResponsePrivate $ "Bad password for table " ++ name
              True  -> do
                chan <- newTChan
                let t'  = t { clients = M.insert uid (Client chan Nothing) (clients t) }
                    userTable' = M.insert uid name userTable
                    ts' = M.insert name t' ts
                writeTVar (userTables dnp) userTable'
                writeTVar (tables dnp) ts'
                rawSend t serverName $ nick ++ " has joined the table." -- deliberately t, sends to everyone else, not the new client
                sendBoardUpdate t' (UpdateUser uid)
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
  liftIO $ print (M.map (\(Table { clients = cs }) -> M.keys cs) ts)
  return $ ResponseSuccess



syntaxRoll :: String
syntaxRoll = "Syntax: /roll AdB+C\nExamples: /roll 2d6-2   /roll d20   /roll 1d8+3\n\nSyntax: /proll AdB+C -- Roll privately to yourself. Alias: pr."

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
cmdRoll uid u nick "pr"   r = cmdRoll uid u nick "proll" r
cmdRoll uid u nick _ []     = return $ ResponsePrivate syntaxRoll
cmdRoll uid u nick cmd (x:_)  = do
    case parse parseDice "" (case x of ('d':_) -> '1':x; _ -> x) of
      Left _ -> return $ ResponsePrivate syntaxRoll
      Right (a,b,c) | a <= 0 -> return $ ResponsePrivate "Number of dice cannot be less than 1."
                    | b <= 0 -> return $ ResponsePrivate "Size of dice cannot be less than 1."
                    | otherwise -> do
                        rolls <- replicateM a $ liftIO (randomRIO (1,b))
                        let total = sum' rolls + c
                        case cmd of
                          "roll" -> send uid serverName $ nick ++ " rolled " ++ show a ++ "d" ++ show b ++ (if c < 0 then show c else "+" ++ show c) ++ " and got " ++ show total
                          "proll" -> return $ ResponsePrivate $ "You privately rolled " ++ show a ++ "d" ++ show b ++ (if c < 0 then show c else "+" ++ show c) ++ " and got " ++ show total
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



syntaxPlace :: String
syntaxPlace = "Syntax: /place <x> <y> -- moves your last-used token to the absolute position (x,y)\n        /place <x> <y> <image> -- if there is already a token with this image, move it to the absolute position. If not, add a token at the given location using the given image. The name of the token is the same as the image name.        /place <x> <y> <image> <name> -- If a token with this name already exists, move it to the given location. If the given image differs from that token's existing image, the given image replaces the old one. If such a token does not exist, create it."


cmdPlace :: Command
cmdPlace uid u nick cmd [] = return $ ResponsePrivate syntaxPlace
cmdPlace uid u nick cmd [_] = return $ ResponsePrivate syntaxPlace
cmdPlace uid u nick cmd [x,y] = do
  case (maybeRead x, maybeRead y) of
    (Nothing,_) -> return $ ResponsePrivate "Failed to parse 'x'"
    (_,Nothing) -> return $ ResponsePrivate "Failed to parse 'y'"
    (Just rx, Just ry) -> do
      lt <- getLastToken uid syntaxPlace
      updateBoard uid lt $ \x -> case x of Nothing -> Nothing; Just tok -> Just tok { tokenX = rx, tokenY = ry }


cmdPlace uid u nick cmd [x,y,image] = do
  case (maybeRead x, maybeRead y) of
    (Nothing,_) -> return $ ResponsePrivate "Failed to parse 'x'"
    (_,Nothing) -> return $ ResponsePrivate "Failed to parse 'y'"
    (Just rx, Just ry) -> do
      t <- getTable uid
      -- find all tokens on the subboard using the given image
      let subboard = fromMaybe M.empty $ M.lookup uid (board t)
      case M.elems $ M.filter ((== image) . file) subboard of
        []    -> updateBoard uid image $ \_ -> Just (Token rx ry image image)
        [tok] -> updateBoard uid (tokenName tok) $ \_ -> Just tok { tokenX = rx, tokenY = ry }
        _     -> sendPrivate "Ambiguous command. Please specify an image name instead."

cmdPlace uid u nick cmd [x,y,image,name] = do
  case (maybeRead x, maybeRead y) of
    (Nothing,_) -> return $ ResponsePrivate "Failed to parse 'x'"
    (_,Nothing) -> return $ ResponsePrivate "Failed to parse 'y'"
    (Just rx, Just ry) -> updateBoard uid name $ \_ -> Just (Token rx ry image name)


syntaxMove :: String
syntaxMove = "Syntax: /move <x> <y> -- moves your last-used token by a relative amount: x tiles right (negative left) and y tiles up (negative down)\n        /move <x> <y> <name> -- moves your token named <name> by a relative amount."

cmdMove uid u nick cmd [x,y] = do
  rx <- case maybeRead x of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'x'"
  ry <- case maybeRead y of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'y'"
  lt <- getLastToken uid syntaxMove
  updateBoard uid lt $ \x -> case x of Nothing -> Nothing; Just tok -> Just tok { tokenX = tokenX tok + rx, tokenY = tokenY tok - ry }

cmdMove uid u nick cmd [x,y,name] = do
  rx <- case maybeRead x of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'x'"
  ry <- case maybeRead y of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'y'"
  updateBoard uid name $ \x -> case x of Nothing -> Nothing; Just tok -> Just tok { tokenX = tokenX tok + rx, tokenY = tokenY tok - ry }

cmdMove uid u nick cmd _ = sendPrivate syntaxMove


syntaxRemove :: String
syntaxRemove = "Syntax: /remove [name] -- removes the last-used token, or the named token. Alias: delete"

cmdRemove uid u nick cmd [] = do
  lt <- getLastToken uid syntaxRemove
  updateBoard uid lt $ \_ -> Nothing

cmdRemove uid u nick cmd [name] = updateBoard uid name $ \_ -> Nothing
cmdRemove uid u nick cmd _      = sendPrivate syntaxRemove



syntaxClear :: String
syntaxClear = "Syntax: /clear -- clears all tokens you own.\n        /clear all -- clears all tokens belonging to everyone (GM only)."

cmdClear uid u nick cmd [] = do
  updateTable uid $ \t -> Just t { board = M.delete uid (board t) }
  t <- getTable uid
  liftIO . atomically $ sendBoardUpdate t UpdateAll
  return ResponseSuccess

cmdClear uid u nick cmd ["all"] = do
  t <- getTable uid
  when (gm t /= uid) $ sendPrivate "/clear all is a GM-only command."
  updateTable uid $ \t -> Just t { board = M.map (const M.empty) (board t) }
  t' <- getTable uid
  liftIO . atomically $ sendBoardUpdate t' UpdateAll
  return ResponseSuccess

cmdClear uid u nick cmd _ = sendPrivate syntaxClear

