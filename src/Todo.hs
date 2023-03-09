module Todo where

import Time (LocalTime, zeroLocalTime)

class FinalStateProvider a where
  provideFinalState :: a -> TodoState

data Todo = Todo
  { orderNumber :: Int,
    todoDescription :: String
  }
  deriving (Read, Show, Eq)

data DoneTodo = DoneTodo
  { doneDescription :: String,
    timeStamp :: LocalTime
  }
  deriving (Read, Show, Eq)

data TodoState = TodoState
  { todos :: [Todo],
    doneTodos :: [DoneTodo],
    currentDate :: LocalTime
  }
  deriving (Read, Show, Eq)

newTodoState :: TodoState
newTodoState = TodoState [] [] zeroLocalTime
