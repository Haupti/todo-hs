module ListTodosCommandSpec where

import Classes (providePresentable)
import Data.Function ((&))
import ListTodosCommand (ListTodosCommandResult (..), listTodos)
import Logger (Logs (..), initialLogState)
import State (runState)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (asExpectation)
import TimeTestData (testLocalTime)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState, provideFinalState)
import TodoTestData (createDone)

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

spec :: Spec
spec = do
  describe "list todos" $ do
    it "does nothing on the state" $ do
      let (listTodosResult, _) = runState initialLogState (listTodos testState)
          state = provideFinalState listTodosResult
       in state `shouldBe` testState
    it "adds no logs" $ do
      let (_, logs) = runState initialLogState (listTodos testState)
       in logs `shouldBe` initialLogState
    it "can present itslef" $ do
      let (listTodosResult, _) = runState initialLogState (listTodos testState)
          presentable = providePresentable listTodosResult
       in presentable `shouldBe` "1 existed before 1\n2 existed before 2\n"
