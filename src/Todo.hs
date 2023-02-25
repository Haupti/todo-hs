module Todo where

data Todo = Todo
  { orderNumber :: Int,
    todoDescription :: String
  }
  deriving (Read, Show, Eq)

newtype DoneTodo = DoneTodo
  { doneDescription :: String
  }
  deriving (Read, Show, Eq)

data TodoState = TodoState
  { todos :: [Todo],
    doneTodos :: [DoneTodo]
  }
  deriving (Read, Show, Eq)