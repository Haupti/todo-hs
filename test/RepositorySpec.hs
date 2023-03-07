module RepositorySpec where

import Test.Hspec (SpecWith, Spec, describe, it, shouldBe, before)
import Todo (Todo(..), DoneTodo(..), TodoState(..), newTodoState)
import System.Directory (removeFile)
import Repository (saveStateToFile, getStateFromFile)

testFileName :: FilePath
testFileName = "testdata.hsrn"

testState :: TodoState
testState = TodoState {
    todos = [Todo { orderNumber = 1, todoDescription = "active todo for test"}],
    doneTodos = [DoneTodo { doneDescription = "done todo for test"}]
}

beforeSpecHook :: SpecWith () -> Spec
beforeSpecHook = before (removeFile testFileName)

spec :: Spec
spec = do
  describe "repository" $ beforeSpecHook $ do
    it "saves to file" $ do
        maybeState <- saveStateToFile testFileName testState 
        maybeState `shouldBe` Just testState
        contents <- readFile testFileName
        contents `shouldBe` show testState
    it "reads from file" $ do
        writeFile testFileName (show testState)
        maybeState <- getStateFromFile testFileName
        maybeState `shouldBe` Just testState
  describe "repository that starts with no file present" $ beforeSpecHook $ do
    it "read: creates file and returns empty state" $ do
        maybeState <- getStateFromFile testFileName
        maybeState `shouldBe` Just newTodoState
  describe "repository that starts with no file present" $ beforeSpecHook $ do
    it "save: creates file and returns empty state" $ do
        maybeState <- saveStateToFile testFileName newTodoState
        maybeState `shouldBe` Just newTodoState


