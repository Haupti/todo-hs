module RepositorySpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Todo (Todo(..), DoneTodo(..), TodoState(..))
import Repository (saveStateToFile, getStateFromFile)

testFileName :: FilePath
testFileName = "testdata.hsrn"

testState :: TodoState
testState = TodoState {
    todos = [Todo { orderNumber = 1, todoDescription = "active todo for test"}],
    doneTodos = [DoneTodo { doneDescription = "done todo for test"}]
}

spec :: Spec
spec = do
  describe "repository" $ do
    it "saves to file" $ do
        maybeState <- saveStateToFile testFileName testState 
        maybeState `shouldBe` Just testState
        contents <- readFile testFileName
        contents `shouldBe` show testState

      
