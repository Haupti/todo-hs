module TimeSpec where

import Test.Hspec
import Classes (present)
import Time (localDate, localTime)

spec :: Spec
spec = describe "time" $ do
    it "shows time" $ do
        t <- localTime
        present t
    it "shows date" $ do
        d <- localDate
        present d