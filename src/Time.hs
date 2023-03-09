module Time (
  localDate,
  localTime,
  LocalDate (..),
  LocalTime (..),
  zeroLocalTime,
  Year,
  Month,
  Day,
  Hour,
  Minute,
  Second
  ) where

import Classes (PresentableProvider (providePresentable), Presenter (..))
import Data.Fixed (Pico)
import Data.Time (getCurrentTime, getTimeZone, localDay, localTimeOfDay, toGregorian, utcToLocalTime)
import qualified Data.Time as T
import GHC.Float.RealFracMethods (floorDoubleInt)


type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int
type Second = Int

data LocalDate = LocalDate {getYear :: Year, getMonth :: Month, getDay :: Day } deriving (Show, Eq, Read)

data LocalTime = LocalTime {getLocalDate :: LocalDate, getHour :: Hour, getMinute :: Minute, getSecond :: Second} deriving (Show, Eq, Read)

dateSeparator :: String
dateSeparator = "-"

timeSeparator :: String
timeSeparator = ":"

ds :: String
ds = dateSeparator

ts :: String
ts = timeSeparator

instance PresentableProvider LocalDate where
  providePresentable (LocalDate y m d) = show y ++ ds ++ show m ++ ds ++ show d

instance Presenter LocalDate where
  present = putStrLn . providePresentable

instance PresentableProvider LocalTime where
  providePresentable (LocalTime date h m s) = providePresentable date ++ " " ++ show h ++ ts ++ show m ++ ts ++ show s

instance Presenter LocalTime where
  present = putStrLn . providePresentable

time :: IO (LocalDate, LocalTime)
time = do
  now <- getCurrentTime
  tz <- getTimeZone now
  let lt = utcToLocalTime tz now

  let (T.TimeOfDay todH todM todS) = localTimeOfDay lt
  let (year, month, day) = toGregorian $ localDay lt

  let _localDate = LocalDate {getYear = yearToInt year, getMonth = month, getDay = day}
  let _localTime = LocalTime {getLocalDate = _localDate, getHour = todH, getMinute = todM, getSecond = picoToIntFloor todS}

  return (_localDate, _localTime)

localDate :: IO LocalDate
localDate = fst <$> time

localTime :: IO LocalTime
localTime = snd <$> time

picoToIntFloor :: Pico -> Int
picoToIntFloor p = floorDoubleInt (read $ show p :: Double)

yearToInt :: T.Year -> Int
yearToInt y = read (show y) :: Int

zeroLocalDate :: LocalDate
zeroLocalDate = LocalDate 1970 1 1

zeroLocalTime :: LocalTime
zeroLocalTime = LocalTime zeroLocalDate 0 0 0
