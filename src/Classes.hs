module Classes where

import Todo (TodoState)

class FinalStateProvider a where
  provideFinalState :: a -> TodoState

class PresentableProvider a where
  providePresentable :: a -> String

class Presenter a where
  present :: a -> IO ()