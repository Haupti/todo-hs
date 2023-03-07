module Command where

import Classes (FinalStateProvider(..), Presenter(..), PresentableProvider (providePresentable))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Todo (TodoState (..))

data Command = AddTodo [String] | CheckTodo [Int] deriving (Show, Eq)

data CommandParsingError = NoSuchCommand String | MissingParamError String String | NoCommand deriving (Show, Eq)

getCalledCommand :: IO (Either Command CommandParsingError)
getCalledCommand = parseCalledCommand <$> getArgs

parseCalledCommand :: [String] -> Either Command CommandParsingError
parseCalledCommand [] = Right NoCommand
parseCalledCommand (x : xs)
  | x == "add" && not (null xs) = Left $ AddTodo xs
  | x == "add" && null xs = Right $ MissingParamError "add" "string | [string]"
  | x == "done" && not (null xs) = Left $ CheckTodo (readIntAndDiscardFailureSilently xs)
  | x == "done" && null xs = Right $ MissingParamError "done" "integer | [integer]"
  | otherwise = Right $ NoSuchCommand x

readIntAndDiscardFailureSilently :: [String] -> [Int]
readIntAndDiscardFailureSilently strs =
  let readIntMaybe = readMaybe :: String -> Maybe Int
   in mapMaybe readIntMaybe strs

data CommandResult = CommandResult {
  presentable :: String,
  finalState :: TodoState
}

instance PresentableProvider CommandResult where
  providePresentable = presentable

instance Presenter CommandResult where
  present = putStrLn . providePresentable

instance FinalStateProvider CommandResult where
  provideFinalState = finalState