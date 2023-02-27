module AddTodoCommand where

import Todo (Todo(..), TodoState(..)) 
import Repository (saveState, getState)
import Logger (WithLogs)
import Data.Function ((&))

addTodos2 :: [String] -> TodoState -> WithLogs TodoState
addTodos2 descriptions currentState = 
  pure $ addTodosToState descriptions currentState

addTodos :: [String] -> IO (Maybe TodoState)
addTodos descriptions = do
  state <- getState
  let mayState = addTodosToState descriptions <$> state :: Maybe TodoState
  case mayState of
    Just justState -> saveState justState
    Nothing -> return Nothing

addTodosToState :: [String] -> TodoState -> TodoState
addTodosToState descriptions state = 
    let startNumber = map orderNumber (todos state) & maximum
        newTodos = mapToNewTodos descriptions (startNumber + 1)
         in
          state { todos = todos state ++ newTodos }

mapToNewTodos :: [String] -> Int -> [Todo]
mapToNewTodos (x:xs) number = Todo { orderNumber = number, todoDescription = x } : mapToNewTodos xs (number + 1)
mapToNewTodos [] number = []