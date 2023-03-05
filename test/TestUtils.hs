module TestUtils where

import Data.Function ((&))

asExpectation :: [IO ()] -> IO ()
asExpectation lst = sequence lst & (head <$>)