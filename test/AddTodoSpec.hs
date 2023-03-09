module AddTodoSpec where

import AddTodoCommand (AddTodoCommandResult (..), addTodos, addTodosToState, mapToNewTodos)
import Data.Function ((&))
import Logger (Logs (..), initialLogState)
import State (runState)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (asExpectation)
import TimeTestData (testLocalTime)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState)
import TodoTestData (createDone)

newTodoStateCurrentTime = newTodoState {currentDate = testLocalTime}

testState :: TodoState
testState =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "existed before 1"},
          Todo {orderNumber = 2, todoDescription = "existed before 2"}
        ],
      doneTodos = [createDone "done"],
      currentDate = testLocalTime
    }

expectedState :: TodoState
expectedState =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "existed before 1"},
          Todo {orderNumber = 2, todoDescription = "existed before 2"},
          Todo {orderNumber = 3, todoDescription = "desc1"},
          Todo {orderNumber = 4, todoDescription = "desc2"}
        ],
      doneTodos = [createDone "done"],
      currentDate = testLocalTime
    }

expectedState2 :: TodoState
expectedState2 =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "desc1"},
          Todo {orderNumber = 2, todoDescription = "desc2"}
        ],
      doneTodos = [],
      currentDate = testLocalTime
    }

spec :: Spec
spec = do
  describe "add todo" $ do
    it "makes new todos starting from number" $ do
      mapToNewTodos ["description1", "description2"] 3
        `shouldBe` [ Todo {orderNumber = 3, todoDescription = "description1"},
                     Todo {orderNumber = 4, todoDescription = "description2"}
                   ]
    it "does nothing on empty list" $ do
      mapToNewTodos [] 3 `shouldBe` []
    it "add new todos to state" $ do
      (addTodosToState ["desc1", "desc2"] testState & todoStateAfterAdding) `shouldBe` expectedState
    it "add new todos to new state" $ do
      (addTodosToState ["desc1", "desc2"] newTodoStateCurrentTime & todoStateAfterAdding) `shouldBe` expectedState2
    it "does not add logs" $ do
      let addTodosStateWithLogs = addTodos ["desc1", "desc2"] testState
          (commandResult, logs) = runState initialLogState addTodosStateWithLogs
          AddTodoCommandResult addedTodos resultState = commandResult
       in asExpectation
            [ logs `shouldBe` Logs [] [] [],
              addedTodos `shouldBe` [Todo 3 "desc1", Todo 4 "desc2"],
              resultState `shouldBe` expectedState
            ]
