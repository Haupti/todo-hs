module Command where

import AddTodoCommand (addTodos)
import CheckTodoCommand (checkTodos)
import Classes (FinalStateProvider (..), PresentableProvider (providePresentable), Presenter (..))
import Data.Maybe (mapMaybe)
import ListTodosCommand (listTodos)
import Logger (WithLogs)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Todo (TodoState (..))

data Command = AddTodo [String] | CheckTodo [Int] | ListTodos deriving (Show, Eq)

data CommandParsingError = NoSuchCommand String | MissingParamError String String | TooManyParams String [String] | NoCommand deriving (Show, Eq)

parseCalledCommand :: [String] -> Either Command CommandParsingError
parseCalledCommand [] = Right NoCommand
parseCalledCommand (x : xs)
  | x == "add" && not (null xs) = Left $ AddTodo xs
  | x == "add" && null xs = Right $ MissingParamError "add" "string | [string]"
  | x == "done" && not (null xs) = Left $ CheckTodo (readIntAndDiscardFailureSilently xs)
  | x == "done" && null xs = Right $ MissingParamError "done" "integer | [integer]"
  | x == "list" && null xs = Left $ ListTodos
  | x == "list" && not (null xs) = Right $ TooManyParams "list" xs
  | otherwise = Right $ NoSuchCommand x

handleCommand :: Command -> TodoState -> WithLogs CommandResult
handleCommand cmd tds = case cmd of
  AddTodo todosRaw -> toResult <$> addTodos todosRaw tds
  CheckTodo todosToCheck -> toResult <$> checkTodos todosToCheck tds
  ListTodos -> toResult <$> listTodos tds
  where
    toResult :: (PresentableProvider a, FinalStateProvider a) => a -> CommandResult
    toResult a = CommandResult {presentable = providePresentable a, finalState = provideFinalState a}

commandParsingFailedError :: CommandParsingError -> IO ()
commandParsingFailedError cmdErr =
  case cmdErr of
    NoCommand -> putStrLn "ERROR: no command given"
    NoSuchCommand str -> putStrLn ("ERROR: no such command: " ++ str)
    MissingParamError cmd paramType -> putStrLn ("ERROR: " ++ cmd ++ " requires parameters of type: " ++ paramType)
    TooManyParams cmd params -> putStrLn ("ERROR: " ++ cmd ++ " got too many arguments: " ++ show params)

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
