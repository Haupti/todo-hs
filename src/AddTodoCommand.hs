module AddTodoCommand where

import Classes (FinalStateProvider (..), PresentableProvider (..))
import Data.Function ((&))
import Logger (WithLogs)
import Todo (Todo (..), TodoState (..))

data AddTodoCommandResult = AddTodoCommandResult
  { addedTodos :: [Todo],
    todoStateAfterAdding :: TodoState
  }

instance FinalStateProvider AddTodoCommandResult where
  provideFinalState = todoStateAfterAdding

instance PresentableProvider AddTodoCommandResult where
  providePresentable (AddTodoCommandResult added _) = unlines $ map (\td -> show (orderNumber td) ++ " " ++ todoDescription td) added

addTodos :: [String] -> TodoState -> WithLogs AddTodoCommandResult
addTodos descriptions currentState =
  pure $ addTodosToState descriptions currentState

addTodosToState :: [String] -> TodoState -> AddTodoCommandResult
addTodosToState descriptions state =
  let startNumber = map orderNumber (todos state) & findMaximum & orElse 0
      newTodos = mapToNewTodos descriptions (startNumber + 1)
   in AddTodoCommandResult {todoStateAfterAdding = state {todos = todos state ++ newTodos}, addedTodos = newTodos}

mapToNewTodos :: [String] -> Int -> [Todo]
mapToNewTodos (x : xs) number = Todo {orderNumber = number, todoDescription = x} : mapToNewTodos xs (number + 1)
mapToNewTodos [] _ = []

findMaximum :: [Int] -> Maybe Int
findMaximum [] = Nothing
findMaximum lst = Just $ maximum lst

orElse :: a -> Maybe a -> a
orElse _ (Just a) = a
orElse b _ = b
