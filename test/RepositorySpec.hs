module RepositorySpec where

import Config (FolderPath)
import Control.Monad (when)
import Repository (getStateFromFile, saveStateToFile)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe)
import Todo (DoneTodo (..), Todo (..), TodoState (..), newTodoState)

testFolderName :: FolderPath
testFolderName = ""

testFileName :: FilePath
testFileName = testFolderName ++ "testdata.hsrn"

testState :: TodoState
testState =
  TodoState
    { todos = [Todo {orderNumber = 1, todoDescription = "active todo for test"}],
      doneTodos = [DoneTodo {doneDescription = "done todo for test"}]
    }

beforeSpecHook :: SpecWith () -> Spec
beforeSpecHook =
  before
    ( do
        isFileExists <- doesFileExist testFileName
        when isFileExists (removeFile testFileName)
    )

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
