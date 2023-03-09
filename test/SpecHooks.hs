module SpecHooks where

import Config (FolderPath)
import Control.Monad (when)
import System.Directory (doesFileExist, removeFile)
import Test.Hspec (Spec, SpecWith, before, describe, it, shouldBe)
import Todo (TodoState (..))

testFolderName :: FolderPath
testFolderName = ""

testFileName :: FilePath
testFileName = testFolderName ++ "testdata.hsrn"

beforeSpecHook :: SpecWith () -> Spec
beforeSpecHook =
  before
    ( do
        isFileExists <- doesFileExist testFileName
        when isFileExists (removeFile testFileName)
    )
