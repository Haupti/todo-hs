module CheckTodoCommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import State (runState)
import Logger (initialLogState, Logs(..))
import Todo (Todo(..), TodoState(..), DoneTodo(..))
import CheckTodoCommand (CheckTodoCommandResult(..), getTodosByNumber, checkTodosFromState, notContained)

testState :: TodoState
testState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"},
                    Todo { orderNumber = 4, todoDescription = "existed before 4"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }
expectedState :: TodoState
expectedState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"},
                    DoneTodo { doneDescription = "existed before 2"},
                    DoneTodo { doneDescription = "existed before 4"}
                ]
        }

spec :: Spec
spec = do
  describe "checking todos" $ do
    it "get todos by number" $ do
        getTodosByNumber [1,3] testState `shouldBe` [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"}
                    ]
    it "checks todo from state" $ do
        let actualStateWithLogs = checkTodosFromState [4,2] testState
            (commandResult, logState) = runState initialLogState actualStateWithLogs
            CheckTodoCommandResult checkedTodos todoState = commandResult
            in
               (checkedTodos, todoState) `shouldBe` ([DoneTodo "existed before 2", DoneTodo "existed before 4"], expectedState)

    it "todos not found but should be checked" $ do
        notContained [4,2, 31] testState `shouldBe` [31]

    it "check todos from state and log not found" $ do
        let actualStateWithLogs = checkTodosFromState [4,2, 31] testState
            (commandResult, logState) = runState initialLogState actualStateWithLogs
            CheckTodoCommandResult checkedTodos todoState = commandResult
            in
                (todoState, info logState) `shouldBe` (expectedState, ["INFO: can not check todos: [31]. not found."])
