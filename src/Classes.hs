module Classes where

class PresentableProvider a where
  providePresentable :: a -> String

class Presenter a where
  present :: a -> IO ()