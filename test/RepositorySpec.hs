module RepositorySpec where

import Test.Hspec (Spec, describe, it, shouldBe, before)
import Todo (Todo(..), DoneTodo(..), TodoState(..))
import System.Directory (removeFile)
import Repository (saveStateToFile, getStateFromFile)

testFileName :: FilePath
testFileName = "testdata.hsrn"

testState :: TodoState
testState = TodoState {
    todos = [Todo { orderNumber = 1, todoDescription = "active todo for test"}],
    doneTodos = [DoneTodo { doneDescription = "done todo for test"}]
}

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

