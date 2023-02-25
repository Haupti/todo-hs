module Lib where

import Command (CommandParsingError(..), Command(..), getCalledCommand)
import Repository (getState)
import Data.Function ((&))
import Todo (Todo(..), TodoState(..))

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

addTodos :: [String] -> IO (Maybe TodoState)
addTodos descriptions = do
  state <- getState
  return $ addTodosToState descriptions <$> state

addTodosToState :: [String] -> TodoState -> TodoState
addTodosToState descriptions state = 
    let startNumber = map orderNumber (todos state) & maximum
        newTodos = mapToNewTodos descriptions (startNumber + 1)
         in
          state { todos = todos state ++ newTodos }

mapToNewTodos :: [String] -> Int -> [Todo]
mapToNewTodos (x:xs) number = Todo { orderNumber = number, todoDescription = x } : mapToNewTodos xs (number + 1)
mapToNewTodos [] number = []