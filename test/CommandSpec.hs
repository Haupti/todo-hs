module CommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Command (parseCalledCommand, CommandParsingError(..), Command(..))
import Todo (Todo(..))

spec :: Spec
spec = do
  describe "parsing" $ do
    it "parses no argument result in failure" $ do
      parseCalledCommand [] `shouldBe` Right NoCommand
    it "parses invalid argument result in failure" $ do
      parseCalledCommand ["rofl rofl"] `shouldBe` Right (NoSuchCommand "rofl rofl")
    it "parses add argument" $ do
      parseCalledCommand ["add", "todo test"] `shouldBe` Left (AddTodo ["todo test"])
    it "parses add argument with multiple args" $ do
      parseCalledCommand ["add", "todo test", "todo test 2"] `shouldBe` Left (AddTodo ["todo test", "todo test 2"])
    it "parses check todo argument with multiple args" $ do
      parseCalledCommand ["done", "1", "2"] `shouldBe` Left (CheckTodo [1, 2])
    it "parses check todo with not-number args fails silently" $ do
      parseCalledCommand ["done", "koro"] `shouldBe` Left (CheckTodo [])