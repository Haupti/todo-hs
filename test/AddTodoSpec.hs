module AddTodoSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Command (parseCalledCommand, CommandParsingError(..), Command(..))
import Todo (Todo(..), TodoState(..), DoneTodo(..))
import Lib (mapToNewTodos, addTodosToState)

testState :: TodoState
testState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }

spec :: Spec
spec = do
  describe "add todo" $ do
    it "makes new todos starting from number" $ do
        mapToNewTodos ["description1", "description2"] 3 `shouldBe` [
            Todo { orderNumber = 3, todoDescription = "description1"},
            Todo { orderNumber = 4, todoDescription = "description2"}
            ]
    it "add new todos to state" $ do
        addTodosToState ["desc1", "desc2"] testState `shouldBe` TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"},
                    Todo { orderNumber = 3, todoDescription = "desc1"},
                    Todo { orderNumber = 4, todoDescription = "desc2"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }