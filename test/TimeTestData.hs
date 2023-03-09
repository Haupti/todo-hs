module TimeTestData where

import Time (LocalTime(..), LocalDate(..), Year, Month, Day, Hour, Minute, Second)

testLocalDate :: LocalDate
testLocalDate = LocalDate 1970 1 1

testLocalTime :: LocalTime
testLocalTime = LocalTime testLocalDate 22 56 3

createLocalTime :: LocalDate -> Hour -> Minute -> Second -> LocalTime
createLocalTime date h m s = LocalTime { getLocalDate = date, getHour = h, getMinute = m, getSecond = s}

createLocalDate :: Year -> Month -> Day -> LocalDate
createLocalDate y m d = LocalDate { getYear = y, getMonth = m, getDay = d }