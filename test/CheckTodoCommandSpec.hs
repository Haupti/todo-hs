module CheckTodoCommandSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Todo (Todo(..), TodoState(..), DoneTodo(..))
import CheckTodoCommand (getTodosByNumber, checkTodosFromState)

testState :: TodoState
testState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"},
                    Todo { orderNumber = 4, todoDescription = "existed before 4"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }
expectedState :: TodoState
expectedState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"},
                    DoneTodo { doneDescription = "existed before 2"},
                    DoneTodo { doneDescription = "existed before 4"}
                ]
        }

spec :: Spec
spec = do
  describe "checking todos" $ do
    it "get todos by number" $ do
        getTodosByNumber [1,3] testState `shouldBe` [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 3, todoDescription = "existed before 3"}
                    ]
    it "checks todo from state" $ do
        checkTodosFromState [4,2] testState `shouldBe` expectedState
