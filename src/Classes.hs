module Classes where

import Todo (TodoState)

class FinalStateProvider a where
  finalTodoState :: a -> TodoState

class Presenter a where
  present :: a -> IO ()