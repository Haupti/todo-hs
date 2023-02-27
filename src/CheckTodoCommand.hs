module CheckTodoCommand where

import Repository (getState, saveState)
import Todo (TodoState(..), Todo(..), DoneTodo(..))
import Logger (WithLogs, addInfo)
import State (modify)
import Data.List (elem, (\\))
import Data.Function ((&))
import Control.Monad (unless)

checkTodos :: [Int] -> IO (Maybe TodoState)
checkTodos orderNumbers = do
    state <- getState
    return $ checkTodosFromState orderNumbers <$> state

checkTodosFromState2 :: [Int] -> TodoState -> WithLogs TodoState
checkTodosFromState2 nums state = 
    let toCheck = getTodosByNumber nums state :: [Todo]
        asChecked = map (\(Todo _ description) -> DoneTodo { doneDescription = description }) toCheck
        notFound = notContained nums state
        in do 
            unless (null notFound) (modify (addInfo $ "INFO: can not check todos: " ++ show notFound ++ ". not found."))
            return state {
                todos = todos state \\ toCheck,
                doneTodos = doneTodos state ++ asChecked
                }

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

notContained :: [Int] -> TodoState -> [Int]
notContained nums (TodoState tds _) = map orderNumber tds & (\\) nums
