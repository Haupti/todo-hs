module CheckTodoCommandSpec where

import CheckTodoCommand (CheckTodoCommandResult (..), checkTodosFromState, getTodosByNumber, notContained)
import Logger (Logs (..), initialLogState)
import SpecHooks (beforeSpecHook)
import State (runState)
import Test.Hspec (Spec, describe, it, shouldBe)
import Time (LocalDate (..), LocalTime (..))
import TimeTestData (testLocalTime)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState)
import TodoTestData (createDone)

oldTime :: LocalTime
oldTime = testLocalTime {getLocalDate = (getLocalDate testLocalTime) {getYear = 1969}}

testState :: TodoState
testState =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "existed before 1"},
          Todo {orderNumber = 2, todoDescription = "existed before 2"},
          Todo {orderNumber = 3, todoDescription = "existed before 3"},
          Todo {orderNumber = 4, todoDescription = "existed before 4"}
        ],
      doneTodos = [createDone "done"],
      currentDate = testLocalTime
    }

expectedState :: TodoState
expectedState =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "existed before 1"},
          Todo {orderNumber = 3, todoDescription = "existed before 3"}
        ],
      doneTodos =
        [ createDone "done",
          createDone "existed before 2",
          createDone "existed before 4"
        ],
      currentDate = testLocalTime
    }

spec :: Spec
spec = do
  describe "checking todos" $ beforeSpecHook $ do
    it "get todos by number" $ do
      getTodosByNumber [1, 3] testState
        `shouldBe` [ Todo {orderNumber = 1, todoDescription = "existed before 1"},
                     Todo {orderNumber = 3, todoDescription = "existed before 3"}
                   ]
    it "get todos by number but nothing present" $ do
      getTodosByNumber [1, 3] newTodoState `shouldBe` []
    it "checks todo from state" $ do
      let actualStateWithLogs = checkTodosFromState [4, 2] testState
          (commandResult, _) = runState initialLogState actualStateWithLogs
          CheckTodoCommandResult checkedTodos todoState = commandResult
       in (checkedTodos, todoState) `shouldBe` ([createDone "existed before 2", createDone "existed before 4"], expectedState)
    it "checks todo from state but nothing present" $ do
      let actualStateWithLogs = checkTodosFromState [4, 2] newTodoState
          (commandResult, _) = runState initialLogState actualStateWithLogs
          CheckTodoCommandResult checkedTodos todoState = commandResult
       in (checkedTodos, todoState) `shouldBe` ([], newTodoState)

    it "todos not found but should be checked" $ do
      notContained [4, 2, 31] testState `shouldBe` [31]

    it "check todos from state and log not found" $ do
      let actualStateWithLogs = checkTodosFromState [4, 2, 31] testState
          (commandResult, logState) = runState initialLogState actualStateWithLogs
          CheckTodoCommandResult _ todoState = commandResult
       in (todoState, info logState) `shouldBe` (expectedState, ["INFO: can not check todos: [31]. not found."])
