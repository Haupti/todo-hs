module TimeTestData where

import Time (LocalTime(..), LocalDate(..))

testLocalDate :: LocalDate
testLocalDate = LocalDate 1970 1 1

testLocalTime :: LocalTime
testLocalTime = LocalTime testLocalDate 22 56 3