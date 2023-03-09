module Command.ListDonesCommandSpec where

import Classes (providePresentable)
import Data.Function ((&))
import Command.ListDonesCommand (ListDonesCommandResult (..), listDones)
import Logger (Logs (..), initialLogState)
import State (runState)
import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (asExpectation)
import TimeTestData (testLocalTime, createLocalDate, createLocalTime)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState, provideFinalState)
import TodoTestData (createDone)
import Time (LocalDate)

date :: LocalDate
date = createLocalDate 2023 3 9

testState :: TodoState
testState =
  TodoState
    { todos =
        [ Todo {orderNumber = 1, todoDescription = "existed before 1"}
        ],
      doneTodos = [ DoneTodo "done 1" $ createLocalTime date 21 32 11, DoneTodo "done 2" $ createLocalTime date 22 12 3],
      currentDate = createLocalTime date 23 2 59
    }

spec :: Spec
spec = do
  describe "list dones" $ do
    it "does nothing on the state" $ do
      let (listDonesResult, _) = runState initialLogState (listDones testState)
          state = provideFinalState listDonesResult
       in state `shouldBe` testState
    it "adds no logs" $ do
      let (_, logs) = runState initialLogState (listDones testState)
       in logs `shouldBe` initialLogState
    it "can present itslef" $ do
      let (listDonesResult, _) = runState initialLogState (listDones testState)
          presentable = providePresentable listDonesResult
       in presentable `shouldBe` "2023-3-9 22:12:3 done 2\n2023-3-9 21:32:11 done 1\n"
