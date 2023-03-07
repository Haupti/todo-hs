module AddTodoSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (asExpectation)
import Logger (Logs(..), initialLogState)
import Data.Function ((&))
import State (runState)
import Todo (Todo(..), TodoState(..), DoneTodo(..), newTodoState)
import AddTodoCommand (mapToNewTodos, addTodosToState, AddTodoCommandResult(..), addTodos)

testState :: TodoState
testState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }

expectedState :: TodoState
expectedState = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "existed before 1"},
                    Todo { orderNumber = 2, todoDescription = "existed before 2"},
                    Todo { orderNumber = 3, todoDescription = "desc1"},
                    Todo { orderNumber = 4, todoDescription = "desc2"}
                ],
                doneTodos = [DoneTodo { doneDescription = "done"}]
        }

expectedState2 :: TodoState
expectedState2 = TodoState { 
                todos = [
                    Todo { orderNumber = 1, todoDescription = "desc1"},
                    Todo { orderNumber = 2, todoDescription = "desc2"}
                ],
                doneTodos = []
        }

spec :: Spec
spec = do
  describe "add todo" $ do
    it "makes new todos starting from number" $ do
        mapToNewTodos ["description1", "description2"] 3 `shouldBe` [
            Todo { orderNumber = 3, todoDescription = "description1"},
            Todo { orderNumber = 4, todoDescription = "description2"}
            ]
    it "does nothing on empty list" $ do
        mapToNewTodos [] 3 `shouldBe` []
    it "add new todos to state" $ do
        (addTodosToState ["desc1", "desc2"] testState & todoStateAfterAdding) `shouldBe` expectedState
    it "add new todos to new state" $ do
        (addTodosToState ["desc1", "desc2"] newTodoState & todoStateAfterAdding) `shouldBe` expectedState2
    it "does not add logs" $ do
        let addTodosStateWithLogs = addTodos ["desc1", "desc2"] testState
            (commandResult, logs) = runState initialLogState addTodosStateWithLogs
            AddTodoCommandResult addedTodos resultState = commandResult
            in 
                asExpectation [
                    logs `shouldBe` Logs [] [] [],
                    addedTodos `shouldBe` [Todo 3 "desc1", Todo 4 "desc2"],
                    resultState `shouldBe` expectedState
                ]
