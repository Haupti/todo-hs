module RepositorySpec where

import Repository (getStateFromFile, saveStateToFile)
import SpecHooks (beforeSpecHook, testFileName, testFolderName)
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe)
import TimeTestData (testLocalTime)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState)
import TodoTestData (createDone)

testState :: TodoState
testState =
  TodoState
    { todos = [Todo {orderNumber = 1, todoDescription = "active todo for test"}],
      doneTodos = [createDone "done todo for test"],
      currentDate = testLocalTime
    }

spec :: Spec
spec = do
  describe "repository" $ beforeSpecHook $ do
    it "saves to file" $ do
      maybeState <- saveStateToFile testFileName testFolderName testState
      maybeState `shouldBe` Just testState
      contents <- readFile testFileName
      contents `shouldBe` show testState
    it "reads from file" $ do
      writeFile testFileName (show testState)
      maybeState <- getStateFromFile testFileName testFolderName
      maybeState `shouldBe` Just testState
  describe "repository that starts with no file present" $ beforeSpecHook $ do
    it "read: creates file and returns empty state" $ do
      maybeState <- getStateFromFile testFileName testFolderName
      maybeState `shouldBe` Just newTodoState
  describe "repository that starts with no file present" $ beforeSpecHook $ do
    it "save: creates file and returns empty state" $ do
      maybeState <- saveStateToFile testFileName testFolderName newTodoState
      maybeState `shouldBe` Just newTodoState
