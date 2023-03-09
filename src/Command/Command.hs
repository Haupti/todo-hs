module Command.Command where

import Command.AddTodoCommand (addTodos)
import Command.CheckTodoCommand (checkTodos)
import Classes (PresentableProvider (providePresentable), Presenter (..))
import Data.Maybe (mapMaybe)
import Command.ListTodosCommand (listTodos)
import Command.ListDonesCommand (listDones)
import Logger (WithLogs)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Todo (TodoState (..), FinalStateProvider(..))

data Command = AddTodo [String] | CheckTodo [Int] | ListTodos | ListDones deriving (Show, Eq)

data CommandParsingError = NoSuchCommand String | MissingParamError String String | NoSuchOptions String [String] | NoCommand deriving (Show, Eq)

parseCalledCommand :: [String] -> Either Command CommandParsingError
parseCalledCommand [] = Right NoCommand
parseCalledCommand (x : xs)
  | x == "add" && not (null xs) = Left $ AddTodo xs
  | x == "add" && null xs = Right $ MissingParamError "add" "string | [string]"
  | x == "done" && not (null xs) = Left $ CheckTodo (readIntAndDiscardFailureSilently xs)
  | x == "done" && null xs = Right $ MissingParamError "done" "integer | [integer]"
  | x == "list" && null xs = Left ListTodos
  | x == "list" && xs == ["--done"] = Left ListDones
  | x == "list" && not (null xs) = Right $ NoSuchOptions "list" xs 
  | otherwise = Right $ NoSuchCommand x

handleCommand :: Command -> TodoState -> WithLogs CommandResult
handleCommand cmd tds = case cmd of
  AddTodo todosRaw -> toResult <$> addTodos todosRaw tds
  CheckTodo todosToCheck -> toResult <$> checkTodos todosToCheck tds
  ListTodos -> toResult <$> listTodos tds
  ListDones -> toResult <$> listDones tds
  where
    toResult :: (PresentableProvider a, FinalStateProvider a) => a -> CommandResult
    toResult a = CommandResult {presentable = providePresentable a, finalState = provideFinalState a}

commandParsingFailedError :: CommandParsingError -> IO ()
commandParsingFailedError cmdErr =
  case cmdErr of
    NoCommand -> putStrLn "ERROR: no command given"
    NoSuchCommand str -> putStrLn ("ERROR: no such command: " ++ str)
    MissingParamError cmd paramType -> putStrLn ("ERROR: " ++ cmd ++ " requires parameters of type: " ++ paramType)
    NoSuchOptions cmd params -> putStrLn ("ERROR: " ++ cmd ++ " has no such option or takes no arguments: " ++ show params)

getCalledCommand :: IO (Either Command CommandParsingError)
getCalledCommand = parseCalledCommand <$> getArgs

readIntAndDiscardFailureSilently :: [String] -> [Int]
readIntAndDiscardFailureSilently strs =
  let readIntMaybe = readMaybe :: String -> Maybe Int
   in mapMaybe readIntMaybe strs

data CommandResult = CommandResult
  { presentable :: String,
    finalState :: TodoState
  }

instance PresentableProvider CommandResult where
  providePresentable = presentable

instance Presenter CommandResult where
  present = putStrLn . providePresentable

instance FinalStateProvider CommandResult where
  provideFinalState = finalState
