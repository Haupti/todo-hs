module Command where

import System.Environment (getArgs)
import Classes (Presenter(..), FinalStateProvider(..))
import Todo (TodoState(..))
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Command = AddTodo [String] | CheckTodo [Int] deriving (Show, Eq)
data CommandParsingError = NoSuchCommand String | NoCommand deriving (Show, Eq)

getCalledCommand :: IO (Either Command CommandParsingError)
getCalledCommand = parseCalledCommand <$> getArgs

parseCalledCommand :: [String] -> Either Command CommandParsingError
parseCalledCommand [] = Right NoCommand
parseCalledCommand (x : xs)
  | x == "add" && not (null xs) = Left $ AddTodo xs
  | x == "done" && not (null xs) = Left $ CheckTodo (readInts xs)
  | otherwise = Right $ NoSuchCommand x

-- discards failures silently
readInts :: [String] -> [Int]
readInts strs =
  let readIntMaybe = readMaybe :: String -> Maybe Int
   in mapMaybe readIntMaybe strs



newtype CommandResult = CommandResult TodoState deriving Show

instance Presenter CommandResult where
  present = print

instance FinalStateProvider CommandResult where
  finalTodoState (CommandResult a) = a

