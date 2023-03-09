module TodoTestData where

import TimeTestData (testLocalTime)
import Todo (DoneTodo (..))

createDone :: String -> DoneTodo
createDone description = DoneTodo {doneDescription = description, timeStamp = testLocalTime}
