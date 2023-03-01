module Logger (Logs (..), WithLogs, addInfo, initialLogState, addError, showLogs) where

import State (State (..))

data Logs = Logs
  { err :: [String],
    warn :: [String],
    info :: [String]
  }

addInfo :: String -> Logs -> Logs
addInfo msg logs = logs {info = info logs ++ [msg]}

addError :: String -> Logs -> Logs
addError msg logs = logs {err = info logs ++ [msg]}

type WithLogs a = State Logs a

initialLogState :: Logs
initialLogState = Logs {err = [], warn = [], info = []}

showLogs :: Logs -> IO ()
showLogs logs =
  mapM_ putStrLn (err logs)
    >> mapM_ putStrLn (warn logs)
    >> mapM_ putStrLn (info logs)
