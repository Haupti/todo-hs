module Command.HelpCommandSpec where

import Classes (providePresentable)
import Command.HelpCommand (help)
import Logger (initialLogState)
import State (runState)
import Test.Hspec (Spec, describe, it, shouldBe)
import TimeTestData (testLocalTime)
import Todo (Todo (..), TodoState (..), provideFinalState)
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
  describe "help" $ do
    it "does nothing on the state" $ do
      let (helpResult, _) = runState initialLogState (help testState)
          state = provideFinalState helpResult
       in state `shouldBe` testState
    it "adds no logs" $ do
      let (_, logs) = runState initialLogState (help testState)
       in logs `shouldBe` initialLogState
    it "can present itslef" $ do
      let (helpResult, _) = runState initialLogState (help testState)
          presentable = providePresentable helpResult
       in presentable
            `shouldBe` unlines
              [ "add [string]       adds todos to active. takes list of space separated todo descriptions",
                "done [int]         checks todos and moves them to done. takes list of space separated numbers",
                "list [--done]      lists active todos. lists done todos with check date when using option --done.",
                "--help             shows these infos"
              ]
