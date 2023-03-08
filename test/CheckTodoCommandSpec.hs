module CheckTodoCommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import State (runState)
import Logger (initialLogState, Logs(..))
import Todo (Todo(..), TodoState(..), DoneTodo(..), newTodoState)
import CheckTodoCommand (CheckTodoCommandResult(..), getTodosByNumber, checkTodosFromState, notContained)
import TimeTestData (testLocalTime)

testState :: TodoState
testState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"},
                    Todo { orderNumber = 4, todoDescription = "existed before 4"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}],
                currentDate = testLocalTime
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
                ],
                currentDate = testLocalTime
        }

spec :: Spec
spec = do
  describe "checking todos" $ do
    it "get todos by number" $ do
        getTodosByNumber [1,3] testState `shouldBe` [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"}
                    ]
    it "get todos by number but nothing present" $ do
        getTodosByNumber [1,3] newTodoState `shouldBe` []
    it "checks todo from state" $ do
        let actualStateWithLogs = checkTodosFromState [4,2] testState
            (commandResult, _) = runState initialLogState actualStateWithLogs
            CheckTodoCommandResult checkedTodos todoState = commandResult
            in
               (checkedTodos, todoState) `shouldBe` ([DoneTodo "existed before 2", DoneTodo "existed before 4"], expectedState)
    it "checks todo from state but nothing present" $ do
        let actualStateWithLogs = checkTodosFromState [4,2] newTodoState
            (commandResult, _) = runState initialLogState actualStateWithLogs
            CheckTodoCommandResult checkedTodos todoState = commandResult
            in
               (checkedTodos, todoState) `shouldBe` ([], newTodoState)

    it "todos not found but should be checked" $ do
        notContained [4,2, 31] testState `shouldBe` [31]

    it "check todos from state and log not found" $ do
        let actualStateWithLogs = checkTodosFromState [4,2, 31] testState
            (commandResult, logState) = runState initialLogState actualStateWithLogs
            CheckTodoCommandResult _ todoState = commandResult
            in
                (todoState, info logState) `shouldBe` (expectedState, ["INFO: can not check todos: [31]. not found."])
