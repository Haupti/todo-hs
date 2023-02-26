module CheckTodoCommand where

import Repository (getState, saveState)
import Todo (TodoState(..), Todo(..), DoneTodo(..))
import Data.List (elem, (\\))
import Data.Function ((&))

checkTodos :: [Int] -> IO (Maybe TodoState)
checkTodos orderNumbers = do
    state <- getState
    return $ checkTodosFromState orderNumbers <$> state

checkTodosFromState :: [Int] -> TodoState -> TodoState
checkTodosFromState nums state = 
    let toCheck = getTodosByNumber nums state :: [Todo]
        asChecked = map (\(Todo _ description) -> DoneTodo { doneDescription = description }) toCheck
        in
            state {
                todos = todos state \\ toCheck,
                doneTodos = doneTodos state ++ asChecked
                }

getTodosByNumber :: [Int] -> TodoState -> [Todo]
getTodosByNumber orderNumbers state = 
    todos state & filter (\(Todo orderNum _) -> orderNum `elem` orderNumbers)
