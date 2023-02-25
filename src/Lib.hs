module Lib
  ( todo,
  )
where

import Command (CommandParsingError(..), Command(..), getCalledCommand)


todo :: IO ()
todo = do
  command <- getCalledCommand
  case command of
    Right (NoSuchCommand msg) -> putStrLn ("ERROR! no such command: " ++ msg)
    Right NoCommand -> putStrLn "ERROR! no command given"
    Left (AddTodo _) -> putStrLn "not implemented"
    Left (CheckTodo _) -> putStrLn "not implemented"
