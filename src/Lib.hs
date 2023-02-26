module Lib where

import Command (CommandParsingError(..), Command(..), getCalledCommand)
import Todo (TodoState(..))

todo :: IO ()
todo = do
  command <- getCalledCommand
  putStrLn (handleCommand command)

handleCommand :: Either Command CommandParsingError -> String
handleCommand command = 
  case command of
    Right (NoSuchCommand msg) -> "ERROR! no such command: " ++ msg
    Right NoCommand -> "ERROR! no command given"
    Left (AddTodo _) -> "not implemented"
    Left (CheckTodo _) -> "not implemented"