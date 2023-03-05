module AddTodoSpec where

import Test.Hspec (Spec, describe, it, shouldBe)
import TestUtils (asExpectation)
import Logger (Logs(..), initialLogState)
import Data.Function ((&))
import State (runState)
import Command (parseCalledCommand, CommandParsingError(..), Command(..))
import Todo (Todo(..), TodoState(..), DoneTodo(..))
import AddTodoCommand (mapToNewTodos, addTodosToState2, AddTodoCommandResult(..), addTodos2)

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

spec :: Spec
spec = do
  describe "add todo" $ do
    it "makes new todos starting from number" $ do
        mapToNewTodos ["description1", "description2"] 3 `shouldBe` [
            Todo { orderNumber = 3, todoDescription = "description1"},
            Todo { orderNumber = 4, todoDescription = "description2"}
            ]
    it "add new todos to state" $ do
        (addTodosToState2 ["desc1", "desc2"] testState & todoStateAfterAdding) `shouldBe` expectedState
    it "does not add logs" $ do
        let addTodosStateWithLogs = addTodos2 ["desc1", "desc2"] testState
            (commandResult, logs) = runState initialLogState addTodosStateWithLogs
            AddTodoCommandResult addedTodos resultState = commandResult
            in 
                asExpectation [
                    logs `shouldBe` Logs [] [] [],
                    addedTodos `shouldBe` [Todo 3 "desc1", Todo 4 "desc2"],
                    resultState `shouldBe` expectedState
                ]
