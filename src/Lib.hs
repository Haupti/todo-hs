module Lib
  ( todo,
  )
where

import Data.Functor ((<&>))
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Text.Read (readMaybe)

stateFile :: String
stateFile = "data.hsrn"

data Todo = Todo
  { orderNumber :: Int,
    todoDescription :: String
  }
  deriving (Read, Show)

newtype DoneTodo = DoneTodo
  { donedescription :: String
  }
  deriving (Read, Show)

data TodoState = TodoState
  { todos :: [Todo],
    doneTodos :: [String]
  }
  deriving (Read, Show)

todo :: IO ()
todo = do
  command <- getCalledCommand
  case command of
    Right (NoSuchCommand msg) -> putStrLn ("ERROR! no such command: " ++ msg)
    Right NoCommand -> putStrLn "ERROR! no command given"
    Left (AddTodo _) -> putStrLn "not implemented"
    Left (CheckTodo _) -> putStrLn "not implemented"

data Command = AddTodo [String] | CheckTodo [Int]

data CommandParsingError = NoSuchCommand String | NoCommand

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

getState :: IO (Maybe TodoState)
getState =
  readFile stateFile <&> (\content -> if not (null content) then (readMaybe content :: Maybe TodoState) else Nothing)

saveState :: TodoState -> IO (Maybe TodoState)
saveState state =
  writeFile stateFile (show state)
    >> getState
