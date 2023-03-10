module Command.PurgeCommandSpec where

import Command.PurgeCommand (PurgeTodosCommandResult (..), purgeTodos)
import Logger (Logs (..), initialLogState)
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
        [ createDone "done"
        ],
      currentDate = testLocalTime
    }

spec :: Spec
spec = do
  describe "purge todos" $
    let purgeTodoCommandResultWithLogs = purgeTodos [2, 4] testState
        (commandResult, logs) = runState initialLogState purgeTodoCommandResultWithLogs
        PurgeTodosCommandResult (TodoState todos dones time) = commandResult
     in do
          it "purged todos are removed from todos" $ do
            todos `shouldBe` [Todo 1 "existed before 1", Todo 3 "existed before 3"]
          it "logs purged ids" $ do
            logs `shouldBe` Logs [] [] ["The following todos are purged from existence and history: [2,4]"]
          it "leaves dones as is" $ do
            dones `shouldBe` [createDone "done"]
