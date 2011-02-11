{-# LANGUAGE TemplateHaskell, OverloadedStrings, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Handler.Commands (
    commandMap
  , CommandResponse (..)
  , helpMap
  , helpDetails
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

import System.Directory
import System.Random
import Text.ParserCombinators.Parsec

import Data.List hiding (insert)

import Control.Arrow (first)

sum' = foldl' (+) 0

commandMap = M.fromList [ ("nick", cmdNick)
                        , ("host", cmdHost)
                        , ("join", cmdJoin)
                        , ("debug", cmdDebug)
                        , ("who",  cmdWho)
                        , ("tables", cmdTables)
                        , ("gm",     cmdGM)
                        , ("whisper", cmdWhisper)
                        , ("w",       cmdWhisper)
                        , ("help",    cmdHelp)
                        , ("quit",    cmdQuit)
                        , ("kick",    cmdKick)

                        -- dice commands
                        , ("roll", cmdRoll)
                        , ("r",    cmdRoll)
                        , ("proll",cmdRoll)
                        , ("pr",   cmdRoll)
                        , ("gmroll",cmdRoll)
                        , ("gmr",  cmdRoll)
                        , ("d3",   cmdRoll)
                        , ("d4",   cmdRoll)
                        , ("d6",   cmdRoll)
                        , ("d8",   cmdRoll)
                        , ("d10",  cmdRoll)
                        , ("d12",  cmdRoll)
                        , ("d20",  cmdRoll)
                        , ("d100", cmdRoll)
                        , ("d%",   cmdRoll)
                        , ("thac0", cmdThac0)

                        -- board commands
                        , ("place",  cmdPlace)
                        , ("move",   cmdMove)
                        , ("delete", cmdRemove)
                        , ("remove", cmdRemove)
                        , ("clear",  cmdClear)
                        , ("tokens", cmdTokens)

                        -- custom commands
                        , ("define", cmdDefine)
                        , ("def",    cmdDefine)
                        , ("undef",  cmdUndef)
                        , ("var",    cmdVar)
                        , ("delvar", cmdDelVar)
                        ]



chatHelpSummary = ("Chat Commands", "These commands deal with hosting and joining tables, changing your nickname, and so on.")
chatHelp = [ ("nick",    ("Changes your nickname.", syntaxNick))
           , ("host",    ("Hosts a new table.", syntaxHost))
           , ("join",    ("Joins an existing table.", syntaxJoin))
           , ("who",     ("Lists the members of the current table.", syntaxWho))
           , ("tables",  ("Lists the tables currently active on the server.", syntaxTables))
           , ("gm",      ("Transfers GM powers to someone else. GM only.", syntaxGM))
           , ("whisper", ("Sends a message privately to another user.", syntaxWhisper))
           , ("help",    ("Displays this list, or details on a command.", syntaxHelp))
           , ("quit",    ("Leaves the table you're currently in.", syntaxQuit))
           , ("kick",    ("Kicks a user from the table. GM only.", syntaxKick))
           ]

rollHelpSummary = ("Dice Commands", "These commands roll dice, publicly, privately, or shared with the GM.")
rollHelp = [ ("roll",    ("Rolls dice and shows the result to everyone.", syntaxRoll))
           , ("proll",   ("Rolls dice and only shows the result to you.", syntaxRoll))
           , ("gmroll",  ("Rolls dice and only shows the result to you and the GM.", syntaxRoll))
           , ("d3",      ("Shortcut to roll 1d3.", "Syntax: /d3"))
           , ("d4",      ("Shortcut to roll 1d4.", "Syntax: /d4"))
           , ("d6",      ("Shortcut to roll 1d6.", "Syntax: /d6"))
           , ("d8",      ("Shortcut to roll 1d8.", "Syntax: /d8"))
           , ("d10",     ("Shortcut to roll 1d10.", "Syntax: /d10"))
           , ("d12",     ("Shortcut to roll 1d12.", "Syntax: /d12"))
           , ("d20",     ("Shortcut to roll 1d20.", "Syntax: /d20"))
           , ("d100",    ("Shortcut to roll 1d100.", "Syntax: /d100"))
           , ("d%",      ("Shortcut to roll d%.", "Syntax: /d%"))
           , ("thac0",   ("Make and attack roll against your THAC0 (used in AD&D 2nd ed.)", syntaxThac0))
           ]

gridHelpSummary = ("Grid Commands", "These commands manipulate the combat grid: placing and moving tokens.")
gridHelp = [ ("place",   ("Places a tile on the board.", syntaxPlace))
           , ("move",    ("Moves a token on the board.", syntaxMove))
           , ("delete",  ("Removes a token from the board.", syntaxRemove))
           , ("remove",  ("Alias for /delete.", syntaxRemove))
           , ("clear",   ("Removes all your tokens from the board. (GM-only: remove all tokens)", syntaxClear))
           , ("tokens",  ("List the tokens on the board.", syntaxTokens))
           ]

varHelpSummary = ("Custom Commands and Variables", "These commands define custom commands and variables. Both are saved for when you return. Commands are private to you, but variables are seen by everyone in the table below the chat box.")
varHelp  = [ ("define",  ("Defines custom commands as shortcuts.", syntaxDefine))
           , ("undef",   ("Deletes a user-defined command.", syntaxUndef))
           , ("var",     ("Defines a variable that everyone can see.", syntaxVar))
           , ("delvar",  ("Deletes a variable you've defined previously.", syntaxDelVar))
           ]

helpDetails :: [((String, String), [(String, (String, String))])]
helpDetails = [(chatHelpSummary, chatHelp), (rollHelpSummary, rollHelp), (gridHelpSummary, gridHelp), (varHelpSummary, varHelp)]

helpList = chatHelp ++ rollHelp ++ gridHelp ++ varHelp

helpMap = M.fromList helpList

-- user id, user, nick, command, args
type Cmd = UserId -> User -> String -> String -> [String] -> Handler CommandResponse



syntaxNick = "Syntax: /nick <new nickname>"

cmdNick :: Cmd
cmdNick uid u nick cmd []   = return $ ResponsePrivate syntaxNick
cmdNick uid u nick cmd (newnick:_) = do
  runDB $ update uid [UserNick newnick]
  updateClient uid $ \c -> Just c { clientNick = newnick }
  send uid serverName $ nick ++ " is now known as " ++ newnick
  t <- getTable uid
  liftIO . atomically $ sendVarUpdate t UpdateAll -- so the name refreshes for everyone.
  return ResponseSuccess



syntaxHost = "Syntax: /host <table name> <password for table>"

cmdHost :: Cmd
cmdHost uid u nick cmd [name,pass] = do
  dnp <- getYesod
  vars <- fmap (map (\(_, Var _ var val) -> (var,val))) . runDB $ selectList [VarUserEq uid] [] 0 0
  liftIO . atomically $ do
    ts <- readTVar $ tables dnp
    case M.lookup name ts of
      Just _  -> return $ ResponsePrivate $ "Table " ++ name ++ " already exists."
      Nothing -> do
        chan <- newTChan
        let t = Table (M.singleton uid (Client chan nick Nothing (M.fromList vars))) pass uid M.empty
        writeTVar (tables dnp) $ M.insert name t ts
        modifyTVar (userTables dnp) $ M.insert uid name
        sendVarUpdate t UpdateAll
        return $ ResponsePrivate $ "Table " ++ name ++ " successfully created."
  


syntaxJoin = "Syntax: /join <table name> <password>"

cmdJoin :: Cmd
cmdJoin uid u nick cmd [name,pass] = do
  dnp <- getYesod
  vars <- fmap (map (\(_, Var _ var val) -> (var,val))) . runDB $ selectList [VarUserEq uid] [] 0 0
  liftIO . atomically $ do
    userTable <- readTVar $ userTables dnp
    ts <- readTVar $ tables dnp
    case M.lookup uid userTable of
      Just t  -> return $ ResponsePrivate $ "You are already in the table " ++ t ++ ". /quit first."
      Nothing -> do
        case M.lookup name ts of
          Nothing -> return $ ResponsePrivate $ "Table " ++ name ++ " does not exist."
          Just t  -> do
            case pass == password t of
              False -> return $ ResponsePrivate $ "Bad password for table " ++ name
              True  -> do
                chan <- newTChan
                let t'  = t { clients = M.insert uid (Client chan nick Nothing (M.fromList vars)) (clients t) }
                    userTable' = M.insert uid name userTable
                    ts' = M.insert name t' ts
                writeTVar (userTables dnp) userTable'
                writeTVar (tables dnp) ts'
                rawSend t serverName $ nick ++ " has joined the table." -- deliberately t, sends to everyone else, not the new client
                sendBoardUpdate t' (UpdateUser uid)
                sendVarUpdate   t' UpdateAll
                return $ ResponsePrivate $ "Successfully joined table " ++ name
cmdJoin uid u nick cmd _ = return $ ResponsePrivate $ syntaxJoin


cmdDebug :: Cmd
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
syntaxRoll = "Syntax: /roll AdB+C\nExamples: /roll 2d6-2   /roll d20   /roll 1d8+3\n\nSyntax: /proll AdB+C -- Roll privately to yourself. Alias: pr.\nSyntax: /gmroll AdB+C -- Roll to yourself and the GM. Alias: gmr."

cmdRoll :: Cmd
cmdRoll uid u nick "d3"   r = cmdRoll uid u nick "roll" (["1d3"]++r)
cmdRoll uid u nick "d4"   r = cmdRoll uid u nick "roll" (["1d4"]++r)
cmdRoll uid u nick "d6"   r = cmdRoll uid u nick "roll" (["1d6"]++r)
cmdRoll uid u nick "d8"   r = cmdRoll uid u nick "roll" (["1d8"]++r)
cmdRoll uid u nick "d10"  r = cmdRoll uid u nick "roll" (["1d10"]++r)
cmdRoll uid u nick "d12"  r = cmdRoll uid u nick "roll" (["1d12"]++r)
cmdRoll uid u nick "d20"  r = cmdRoll uid u nick "roll" (["1d20"]++r)
cmdRoll uid u nick "d100" r = cmdRoll uid u nick "roll" (["1d100"]++r)
cmdRoll uid u nick "d%"   r = cmdRoll uid u nick "roll" (["1d100"]++r)
cmdRoll uid u nick "r"    r = cmdRoll uid u nick "roll" r
cmdRoll uid u nick "pr"   r = cmdRoll uid u nick "proll" r
cmdRoll uid u nick "gmr"  r = cmdRoll uid u nick "gmroll" r
cmdRoll uid u nick _ []     = return $ ResponsePrivate syntaxRoll
cmdRoll uid u nick cmd [x,y] = case y of
                                 ('+':_) -> cmdRoll uid u nick cmd [x++y]
                                 ('-':_) -> case maybeRead y :: Maybe Integer of
                                              Nothing -> return $ ResponsePrivate "Couldn't make sense of the numbers."
                                              Just y' -> cmdRoll uid u nick cmd [x++y]
                                 _ -> cmdRoll uid u nick cmd [x ++ "+" ++ y] -- insert a +
cmdRoll uid u nick cmd [x]  = do
    case parse parseDice "" (case x of ('d':_) -> '1':x; _ -> x) of
      Left _ -> return $ ResponsePrivate syntaxRoll
      Right (a,b,c) | a <= 0 -> return $ ResponsePrivate "Number of dice cannot be less than 1."
                    | b <= 0 -> return $ ResponsePrivate "Size of dice cannot be less than 1."
                    | otherwise -> do
                        rolls <- replicateM a $ liftIO (randomRIO (1,b))
                        let total = sum' rolls + c
                        case cmd of
                          "roll" -> send uid serverName $ nick ++ " rolled " ++ showDice a b c ++ " and got " ++ show total ++ " (" ++ unwords (map show rolls) ++ ")."
                          "proll" -> return $ ResponsePrivate $ "You privately rolled " ++ showDice a b c ++ " and got " ++ show total ++ " (" ++ unwords (map show rolls) ++ ")."
                          "gmroll" -> do
                            t <- getTable uid
                            when (uid /= gm t) $ sendTo uid (gm t) $ MessageChat serverName $ nick ++ " privately rolled " ++ showDice a b c ++ " and got " ++ show total ++ " (" ++ unwords (map show rolls) ++ ")."
                            return $ ResponsePrivate $ "You GM-rolled " ++ showDice a b c ++ " and got " ++ show total ++ " (" ++ unwords (map show rolls) ++ ")."
  where parseDice = do
          a <- many1 digit
          char 'd'
          b <- many1 digit
          mc1 <- optionMaybe $ char '+' >> many1 digit
          mc2 <- optionMaybe $ char '-' >> many1 digit
          let c = case (mc1,mc2) of
                    (Nothing,Nothing) -> 0
                    (Just x, Nothing) -> read x
                    (Nothing,Just y)  -> - read y
          return ((read a, read b, c) :: (Int,Int,Int))
        showDice a b c = show a ++ "d" ++ show b ++ (if c < 0 then show c else "+" ++ show c)

cmdRoll uid u nick cmd _ = return $ ResponsePrivate syntaxRoll


syntaxPlace :: String
syntaxPlace = "Syntax: /place <x> <y> -- moves your last-used token to the absolute position (x,y)\n        /place <x> <y> <image> -- if there is already a token with this image, move it to the absolute position. If not, add a token at the given location using the given image. The name of the token is the same as the image name.        /place <x> <y> <image> <name> -- If a token with this name already exists, move it to the given location. If the given image differs from that token's existing image, the given image replaces the old one. If such a token does not exist, create it."


cmdPlace :: Cmd
cmdPlace uid u nick cmd [] = return $ ResponsePrivate syntaxPlace
cmdPlace uid u nick cmd [_] = return $ ResponsePrivate syntaxPlace
cmdPlace uid u nick cmd [x,y] = do
  case (maybeRead x, maybeRead y) of
    (Nothing,_) -> return $ ResponsePrivate "Failed to parse 'x'"
    (_,Nothing) -> return $ ResponsePrivate "Failed to parse 'y'"
    (Just rx, Just ry) -> do
      when (rx < 0 || ry < 0 || rx >= gridCols || ry >= gridRows) $ sendSuccess -- do nothing silently when placing outside the grid
      lt <- getLastToken uid syntaxPlace
      t <- getTable uid
      case getTokenAt rx ry t of
        Nothing -> updateBoard uid lt $ \x -> case x of Nothing -> Nothing; Just tok -> Just tok { tokenX = rx, tokenY = ry }
        Just _  -> return ResponseSuccess -- do nothing, silently.


cmdPlace uid u nick cmd [x,y,image] = do
  case (maybeRead x, maybeRead y) of
    (Nothing,_) -> return $ ResponsePrivate "Failed to parse 'x'"
    (_,Nothing) -> return $ ResponsePrivate "Failed to parse 'y'"
    (Just rx, Just ry) -> do
      when (rx < 0 || ry < 0 || rx >= gridCols || ry >= gridRows) $ sendSuccess -- do nothing silently when placing outside the grid
      when (".." `isInfixOf` image || "/" `isPrefixOf` image) $ sendPrivate "Illegal image name."
      exists <- liftIO . doesFileExist $ "static/images/" ++ image
      case exists of
        True  -> return ()
        False -> sendPrivate "Image doesn't exist."
      t <- getTable uid
      case getTokenAt rx ry t of
        Just _  -> sendSuccess -- do nothing, silently
        Nothing -> do
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
    (Just rx, Just ry) -> do
      when (rx < 0 || ry < 0 || rx >= gridCols || ry >= gridRows) $ sendSuccess -- do nothing silently when placing outside the grid
      when (".." `isInfixOf` image || "/" `isPrefixOf` image) $ sendPrivate "Illegal image name."
      exists <- liftIO . doesFileExist $ "static/images/" ++ image
      case exists of
        True  -> return ()
        False -> sendPrivate "Image doesn't exist."
      t <- getTable uid
      case getTokenAt rx ry t of
        Just _  -> sendSuccess
        Nothing -> updateBoard uid name $ \_ -> Just (Token rx ry image name)


syntaxMove :: String
syntaxMove = "Syntax: /move <x> <y> -- moves your last-used token by a relative amount: x tiles right (negative left) and y tiles up (negative down)\n        /move <x> <y> <name> -- moves your token named <name> by a relative amount."

cmdMove uid u nick cmd [x,y] = do
  rx <- case maybeRead x of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'x'"
  ry <- case maybeRead y of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'y'"
  lt <- getLastToken uid syntaxMove
  t  <- getTable uid
  updateBoard uid lt $ move rx ry t

cmdMove uid u nick cmd [x,y,name] = do
  rx <- case maybeRead x of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'x'"
  ry <- case maybeRead y of Just a -> return a; Nothing -> sendPrivate "Failed to parse 'y'"
  t  <- getTable uid
  updateBoard uid name $ move rx ry t

cmdMove uid u nick cmd _ = sendPrivate syntaxMove

-- helper function for cmdMove
move rx ry t Nothing    = Nothing
move rx ry t (Just tok) = let x' = tokenX tok + rx
                              y' = tokenY tok - ry
                          in  case (x' < 0 || y' < 0 || x' >= gridCols || y' >= gridRows,   getTokenAt x' y' t) of
                                (True,_)    -> Just tok
                                (_,Just _)  -> Just tok
                                (_,Nothing) -> Just tok { tokenX = x', tokenY = y' }



syntaxRemove :: String
syntaxRemove = "Syntax: /remove [name] -- removes the last-used token, or the named token. Alias: delete"

cmdRemove uid u nick cmd [] = do
  lt <- getLastToken uid syntaxRemove
  updateBoard uid lt $ \_ -> Nothing

cmdRemove uid u nick cmd [name] = updateBoard uid name $ \_ -> Nothing
cmdRemove uid u nick cmd _      = sendPrivate syntaxRemove



syntaxClear :: String
syntaxClear = "Syntax: /clear -- clears all tokens you own.\n        /clear all -- clears all tokens belonging to everyone. (GM only)"

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


syntaxWho = "Syntax: /who"

cmdWho uid u nick cmd _ = do
  t <- getTable uid
  let nicks = M.elems $ M.mapWithKey (\k c -> (if gm t == k then "*" else "") ++ clientNick c) (clients t)
  return $ ResponsePrivate $ "Current members of the table: " ++ intercalate ", " nicks


syntaxTables = "Syntax: /tables"

cmdTables uid u nick cmd _ = do
  dnp <- getYesod
  ts  <- liftIO . atomically $ readTVar (tables dnp)
  let descriptions = M.elems $ M.mapWithKey (\k t -> k ++ " (" ++ show (M.size $ clients t) ++ " users)") ts
  return $ ResponsePrivate $ "\nCurrently active tables:\n" ++ unlines descriptions



syntaxGM :: String
syntaxGM = "Syntax: /gm <nickname> -- transfers GM powers to the given user. (GM only)"

cmdGM uid u nick cmd args = do
  t <- getTable uid
  when (gm t /= uid) $ sendPrivate "/gm is a GM-only command."
  let target  = unwords args        -- these two are used independently below
  (tid,targetClient) <- getClientByNick uid target
  updateTable uid $ \t -> Just t { gm = tid }
  send uid serverName $ nick ++ " has made " ++ clientNick targetClient ++ " the new GM."



syntaxWhisper :: String
syntaxWhisper = "Syntax: /whisper <nickname> <msg> -- send a message privately to the given user. Giving the first few letters of the name is usually sufficient (case-insensitive). Alias: w"

cmdWhisper uid u nick cmd (targetName:msgwords) = do
  let msg = unwords msgwords
  (targetId, target) <- getClientByNick uid targetName
  sendTo uid targetId $ MessageWhisper nick msg
  return $ ResponsePrivate $ "Whisper to " ++ clientNick target ++ ": " ++ msg

cmdWhisper uid u nick cmd _ = sendPrivate "You must supply a target user and a message."

syntaxTokens :: String
syntaxTokens = "Syntax: /tokens -- lists all board tokens belonging to you.\n        /tokens all -- lists all board tokens."

cmdTokens uid u nick cmd []      = showTokens uid (==uid)
cmdTokens uid u nick cmd ["all"] = showTokens uid (const True)
cmdTokens uid u nick cmd _       = sendPrivate syntaxTokens


showTokens :: UserId -> (UserId -> Bool) -> Handler CommandResponse
showTokens uid p = do
  t <- getTable uid
  let tokens = M.assocs $ M.filterWithKey (\k _ -> p k) (board t)
      subboards = map (first $ \k -> fromMaybe ("userId " ++ show k) . fmap clientNick $ M.lookup k (clients t)) tokens -- yields (nick, subboard) pairs
  case subboards of
    [] -> return $ ResponsePrivate "No tokens found."
    _  -> return $ ResponsePrivate . unlines . concatMap (\(nick, subboard) -> ("Tokens belonging to " ++ nick ++ ": ") : map showToken (M.elems subboard)) $ subboards

 where showToken (Token x y file name) = "    " ++ name ++ ": (" ++ show x ++ ", " ++ show y ++ ")  " ++ file



syntaxHelp = "Syntax: /help -- displays a list of commands.\n        /help <command> -- displays detailed help on a command."

cmdHelp uid u nick cmd [] = return $ ResponsePrivate $ "--- Available Commands: ---\n" ++ concatMap (\(c,(desc,_)) -> "    " ++ c ++ " -- " ++ desc ++ "\n") helpList
cmdHelp uid u nick cmd [query] = case M.lookup query helpMap of
                                   Nothing -> return $ ResponsePrivate $ "Command " ++ query ++ " not found. Run /help for a list of commands."
                                   Just (desc,syntax) -> return $ ResponsePrivate $ desc ++ "\n" ++ syntax ++ "\n"
cmdHelp uid u nick cmd _ = return $ ResponsePrivate syntaxHelp



syntaxDefine :: String
syntaxDefine = "Syntax: /define <name> <command...> -- the name must be one word but the command can be many words long. The command can then be executed with /<name>. See examples below.\nExamples of definition:\n  /define attack_sword /roll 1d20+3\n  /define spam_chat This is a chat message I can spam.\nExamples of use:\n  /attack_sword\n  /spam_chat\n"

cmdDefine uid u nick cmd [] = return $ ResponsePrivate "You must supply a name and command. Run '/help define' for more details."
cmdDefine uid u nick cmd [name] = return $ ResponsePrivate "You must supply a name and command. Run '/help define' for more details. To remove a command definition, use /undef."
cmdDefine uid u nick cmd (name:newcmds) = do
  let newcmd = unwords newcmds
  -- store the command into the DB, removing any old one first
  runDB $ do
    deleteWhere [CommandUserEq uid, CommandNameEq name]
    insert $ Command uid name newcmd
  return $ ResponsePrivate $ "New command /"++name++" successfully stored."


syntaxUndef :: String
syntaxUndef = "Syntax: /undef <name> -- Undefines a previously defined command."


cmdUndef uid u nick cmd [name] = do
  cmdList <- runDB $ selectList [CommandUserEq uid, CommandNameEq name] [] 0 0
  case cmdList of
    [] -> return $ ResponsePrivate $ "No user-defined command named '" ++ name ++ "' found."
    [x] -> do runDB $ deleteWhere [CommandUserEq uid, CommandNameEq name]
              return $ ResponsePrivate $ "Command /"++name++" removed."
    _   -> return $ ResponsePrivate $ "Multiple user-defined commands named '" ++ name ++ "' were found. This should be impossible, there has been a bug. Please notify the developers."

cmdUndef uid u nick cmd _ = return $ ResponsePrivate syntaxUndef



syntaxVar :: String
syntaxVar = "Syntax: /var <name> <value> -- Sets the variable 'name' to have the value 'value'. Everyone will see the updated value."


cmdVar uid u nick cmd (invar:val) = do
  (ownerId, ownerNick, var) <- varBreakdown uid nick invar
  let value = unwords val
  runDB $ do
    deleteWhere [VarUserEq ownerId, VarNameEq var]
    insert $ Var ownerId var value
  updateClient ownerId $ \c -> Just c { vars = M.insert var value (vars c) }
  case ownerId == uid of
    False -> send uid serverName $ "GM set " ++ ownerNick ++ "'s variable '" ++ var ++ "' to '" ++ value ++ "'."
    True  -> send uid serverName $ nick ++ " set '" ++ var ++ "' to '" ++ value ++ "'."
  t <- getTable uid
  liftIO . atomically $ sendVarUpdate t UpdateAll
  return ResponseSuccess

cmdVar uid u nick cmd _ = return $ ResponsePrivate syntaxVar


syntaxDelVar :: String
syntaxDelVar = "Syntax: /delvar <name> -- Deletes the variable 'var'."

cmdDelVar uid u nick cmd [invar] = do
  (ownerId, ownerNick, var) <- varBreakdown uid nick invar
  mc <- getClientById ownerId
  case mc of
    Nothing -> sendPrivate "Client doesn't exist"
    Just c  -> do
      case M.lookup var (vars c) of
        Nothing -> sendPrivate $ "No var '" ++ var ++ "' found."
        Just _  -> do
          runDB $ deleteWhere [VarUserEq ownerId, VarNameEq var]
          updateClient ownerId $ \c -> Just c { vars = M.delete var (vars c) }
          case ownerId == uid of
            True  -> send uid serverName $ ownerNick ++ " deleted '" ++ var ++ "'."
            False -> send uid serverName $ "GM deleted " ++ ownerNick ++ "'s variable '" ++ var ++ "'."
          t <- getTable uid
          liftIO . atomically $ sendVarUpdate t UpdateAll
          return ResponseSuccess

cmdDelVar uid u nick cmd _ = return $ ResponsePrivate syntaxDelVar


varBreakdown :: UserId -> String -> String -> Handler (UserId, String, String)
varBreakdown uid nick invar = do
  t <- getTable uid
  case ":" `isInfixOf` invar of
    False -> return (uid, nick, invar)
    True  -> do
      when (gm t /= uid) $ sendPrivate "Only the GM can modify others' variables."
      let (owner,_:var) = span (/=':') invar
      (cid,_) <- getClientByNick uid owner
      return (cid,owner,var)


syntaxQuit :: String
syntaxQuit = "Syntax: /quit"

cmdQuit uid u nick cmd _ = do
  send uid serverName $ nick ++ " has quit."
  removeClient uid
  return ResponseSuccess
  

syntaxKick :: String
syntaxKick = "Syntax: /kick <nickname> -- kicks the player with the given nickname from the table. GM only."

cmdKick uid u nick cmd [name] = do
  t <- getTable uid
  when (gm t /= uid) $ sendPrivate "The /kick command is GM-only."
  (cid, c) <- getClientByNick uid name
  send uid serverName $ name ++ " was kicked by the GM."
  removeClient cid
  return ResponseSuccess

cmdKick uid u nick cmd _ = return $ ResponsePrivate syntaxKick



syntaxThac0 :: String
syntaxThac0 = "Syntax: /thac0 <THAC0> -- (Note: That's a zero.) For example, if your THAC0 was 16, you would run /thac0 16. The command returns the AC you would hit."

cmdThac0 uid u nick cmd [strthac0] = do
  thac0 <- case maybeRead strthac0 of
    Nothing -> sendPrivate "Couldn't read the THAC0 number"
    Just t  -> return (t::Integer)
  roll <- liftIO $ randomRIO (1,20)
  let hitAC = thac0 - roll
  send uid serverName $ nick ++ " rolled against THAC0 " ++ show thac0 ++ " and hit AC " ++ show hitAC ++ " (" ++ show roll ++ ")"
  return ResponseSuccess


