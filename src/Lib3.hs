{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp(..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    Command(..),
    Statements(..)
    ) where

import Control.Concurrent.STM

import Control.Concurrent ( Chan )
import qualified Lib2
import Control.Arrow (ArrowChoice(left))
import Lib2 (State(workouts))
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import System.IO (readFile, writeFile)
import Control.Exception (SomeException (SomeException), try, catch, displayException)
import Control.Monad.Trans.RWS (state)
import Control.Monad.Trans.Cont (resetT)
data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop :: IO ()
    loop = do
      operation <- readChan chan
      case operation of
        Save queryString chan -> do
          writeFile filePath queryString
          writeChan chan ()
          loop
        Load chan -> do
          content <- readFile filePath
          writeChan chan content
          loop

filePath :: FilePath
filePath = "state.txt"

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand 
               deriving (Show, Eq)

data Command' = Single' Command |
                Multi' [Command]
                deriving (Show, Eq)


parseCommand :: String -> Either String (Command', String)
parseCommand str = 
    case or2 parsePaste' parseCommand' str of
      Right(command,rest) -> Right(command,rest)
      Left e -> Left e
    

parseCommand' :: String -> Either String (Command', String)
parseCommand' str = 
    case or3 parseStatements' parseLoad parseSave str of
      Right(command,rest) -> Right(Single' command,rest)
      Left e -> Left e

parseCommand'' :: String -> Either String (Command, String)
parseCommand'' str = 
    case or3 parseStatements' parseLoad parseSave str of
      Right(command,rest) -> Right(command,rest)
      Left e -> Left e
type Parser a = String -> Either String (a, String)

parsePaste :: Parser [Command]
parsePaste str =
  case parseCommand'' str of
    Right(cmd,rest) ->
      case Lib2.parseChar' '\n' rest of
        Right(_,rest') ->
          case parsePaste rest' of
            Right(commands,rest'') ->
              Right(cmd:commands,rest'')
            Left e -> Right([cmd],rest')
        Left e -> Left e
    Left e -> Left e

parsePaste' :: Parser Command'
parsePaste' str =
    case parsePaste str of
      Right (commands, rest) ->
        Right (Multi' commands,rest)
      Left e -> Left e


parseStatements :: String -> Either String (Statements, String)
parseStatements str =
    case or2 parseQuery' parseBatch' str of
      Right(query,rest) -> Right(query,rest)
      Left e -> Left e
  
parseLoad :: String -> Either String (Command, String)
parseLoad str =
  case Lib2.parseWord str of
    Right(word,rest) ->
      if word == "LoadState"
        then Right(LoadCommand,rest)
      else Left "No such Load command"
    Left e -> Left e

parseSave :: String -> Either String (Command, String)
parseSave str =
  case Lib2.parseWord str of
    Right(word,rest) ->
      if word == "SaveState"
        then Right(SaveCommand,rest)
      else Left "No such Save command"
    Left e -> Left e

parseStatements' :: String -> Either String (Command, String)
parseStatements' str =
  case parseStatements str of
    Right(statement ,rest) -> Right( StatementCommand statement,rest)
    Left e -> Left e
parseQuery' :: String -> Either String (Statements,String)
parseQuery' str =
  case Lib2.parseQuery str of
    Right(query,rest) -> Right(Single query,rest)
    Left e -> Left e

or2 :: Parser a -> Parser a -> Parser a
or2 p1 p2 str =
    case p1 str of
        Right r1 -> Right r1
        Left e1 -> case p2 str of
            Right r2 -> Right r2
            Left e2 -> Left (e1 ++ "; " ++ e2)

or3 :: Parser a -> Parser a -> Parser a -> Parser a
or3 p1 p2 p3 str =
    case p1 str of
        Right r1 -> Right r1
        Left e1 -> case p2 str of
            Right r2 -> Right r2
            Left e2 -> case p3 str of
                Right r3 -> Right r3
                Left e3 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3)

or4 :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or4 p1 p2 p3 p4 str =
    case p1 str of
        Right r1 -> Right r1
        Left e1 -> case p2 str of
            Right r2 -> Right r2
            Left e2 -> case p3 str of
                Right r3 -> Right r3
                Left e3 -> case p4 str of
                    Right r4 -> Right r4
                    Left e4 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4)

parseBatch :: Parser [Lib2.Query]

parseBatch str = 
    case Lib2.parseQuery str of
      Right(query,rest) ->
        case Lib2.parseWord rest of
          Right(word,rest'') ->
            if word /= "end"
              then
                case parseBatch rest of 
                  Right(querys,rest') ->
                      Right(query : querys,rest')
                  Left e -> Left e
            else
            Right([query],rest'')
          Left e -> Left e    
      Left e -> Left e

parseBatch' :: String -> Either String (Statements,String)
parseBatch' str = 
    case Lib2.parseWord str of
      Right(word,rest) ->
        if word == "begin"
          then 
            case Lib2.parseChar' ' ' rest of
              Right(_,rest') ->
                case parseBatch rest' of
                  Right (batch,rest''') ->
                    Right(Batch batch,rest''')
                  Left e -> Left e
              Left e -> Left e
          else 
            Left "not batch"
      Left e -> Left e  


marshallState :: Lib2.State -> Statements
marshallState st =
    if ((Lib2.meals st) == [] && (Lib2.workouts st) == [])
      then Batch[Lib2.FitnessApp (Lib2.appName st) [Lib2.Workout Lib2.Cardio Lib2.Low []] [Lib2.Meal Lib2.Main 200 []],Lib2.RemoveMeal, Lib2.RemoveWorkout] 
    else if ((Lib2.meals st) == [])
      then Batch[Lib2.FitnessApp (Lib2.appName st) (Lib2.workouts st) [Lib2.Meal Lib2.Main 200 []],Lib2.RemoveMeal]
    else if((workouts st) == [])
      then Batch[Lib2.FitnessApp (Lib2.appName st) [Lib2.Workout Lib2.Cardio Lib2.Low []] (Lib2.meals st), Lib2.RemoveWorkout] 
    else 
      Single $ Lib2.FitnessApp (Lib2.appName st) (Lib2.workouts st) (Lib2.meals st)



renderStatements :: Statements -> String
renderStatements (Single query) =
    renderQuery (query)
renderStatements (Batch queries) =
    "begin " ++ unwords (map renderQuery (queries)) ++ " end"

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.FitnessApp name workouts meals) =
    "FitnessApp " ++ name ++ " " ++ parserForQuery'(show(workouts)) ++ " " ++ parserForQuery'( show (meals))
renderQuery Lib2.RemoveWorkout = "RemoveWorkout"
renderQuery Lib2.RemoveMeal = "RemoveMeal"


parserForQuery' :: String -> String
parserForQuery' str =
    case Lib2.and3_0 ( \a _ b -> b ) Lib2.parseWord (Lib2.parseChar' ' ') Lib2.parseWord str of
        Right(firstWord, rest) -> 
            case Lib2.and3_0 ( \_ b _ -> b ) (Lib2.parseChar' ' ') Lib2.parseWord (Lib2.parseChar' ' ') rest of
                Right(secondWord,rest') ->
                    case Lib2.parseChar' '[' rest' of
                        Right(_, rest'') ->
                            case Lib2.parseChar' ']' rest'' of
                                Right(_,_) ->
                                    case parserForQuery'' rest'' of
                                        Right(skliaustai,rest''') ->
                                            (firstWord ++ " "  ++ secondWord ++ " { " ++ skliaustai ++ (parserForQuery' rest'''))
                                        Left _ -> ""
                                Left _ ->
                                 (firstWord ++ " "  ++ secondWord ++ " { " ++ (parserForQuery' rest''))
                        Left e -> ""        
                Left e -> ""                      
        Left e -> ""                    

parserForQuery'' :: Parser String
parserForQuery'' "]]" = Right (" }","")
parserForQuery'' str =
    case Lib2.parseChar' ']' str of
        Right(_,rest) ->
            case parserForQuery'' rest of
                Right(head, tail) ->
                    Right(" } " ++ head,tail)
                Left e -> 
                    Right(" } ", rest)
        Left e -> Left e

stateTransition :: TVar Lib2.State -> Command' -> Chan StorageOp ->
                   IO (Either String (Maybe String))

stateTransition stateVar (Single' LoadCommand)chan =
  do
    chan' <- newChan
    writeChan chan (Load chan')
    queryString <- readChan chan'
    case parseStatements queryString of
      Right(statements,_) ->do
        result <- atomically $ atomicStatements stateVar statements
        
        case result of
          Right _ -> return $ Right (Just "State loaded successfully.")
          Left err -> return $ Left ("Failed to apply statements: " ++ err)
      Left err -> return $ Left ("Failed to parse loaded state: " ++ err)

stateTransition stateVar (Single' SaveCommand) chan =
  do
    currentState <- readTVarIO stateVar
    let statementsAsString = renderStatements $ marshallState currentState
    responceChan <- newChan
    writeChan chan (Save statementsAsString responceChan)
    _ <- readChan responceChan
    return $ Right $ Just "State saved"

stateTransition stateVar (Single'(StatementCommand statements)) chan =
  do
    result <- try $ atomically $ atomicStatements stateVar statements
    case result of
      Right res -> return res
      Left (SomeException e) -> do
        return $ Left $ "Transaction failed: " 

stateTransition stateVar (Multi' commands) chan = executeCommands commands Nothing
  where
    executeCommands :: [Command] -> Maybe String -> IO (Either String (Maybe String))
    executeCommands [] accumulatedMessage =
      case accumulatedMessage of
        Just msg -> return $ Right $ Just msg
        Nothing  -> return $ Right Nothing
    executeCommands (cmd:cmds) accumulatedMessage = do
      result <- stateTransition stateVar (Single' cmd) chan
      let updatedMessage = case result of
            Left err         -> Just (maybe err (\acc -> acc ++ "\n" ++ err) accumulatedMessage)
            Right (Just msg) -> Just (maybe msg (\acc -> acc ++ "\n" ++ msg) accumulatedMessage) 
            Right Nothing    -> accumulatedMessage 
      executeCommands cmds updatedMessage


atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements stateVar (Single query)  = do
    currentState <- readTVar stateVar
    case applyQuery currentState query of
      Right (Just msg,newState) -> do
        writeTVar stateVar newState
        return $ Right $ Just msg
      Left err -> return $ Left err

atomicStatements stateVar (Batch []) = return $ Right Nothing
atomicStatements stateVar (Batch (h:tail)) = do
    currentState <- readTVar stateVar
    case applyQuery currentState h of
       Right (Just msg,newState) -> do
        writeTVar stateVar newState
        result <- atomicStatements stateVar (Batch tail)
        case result of
          Right (Just restMsg) -> return $ Right (Just (msg ++ "\n" ++ restMsg))
          Right Nothing -> return $ Right (Just msg) 
          Left err -> return $ Left err
        
       Left e -> throwSTM (userError "")


applyQuery ::  Lib2.State -> Lib2.Query -> Either String (Maybe String, Lib2.State)
applyQuery state query = 
    case Lib2.stateTransition state query of
      Right(msg,newState) -> Right (msg,newState)
      Left e -> Left e

