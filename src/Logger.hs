module Logger (Logs(..), WithLogs, addInfo, initialLogState) where
    
import State (State(..))

data Logs = Logs {
    err :: [String],
    warn :: [String],
    info :: [String]
}

addInfo :: String -> Logs -> Logs
addInfo msg logs = logs { info = info logs ++ [msg] }

type WithLogs a = State Logs a 

initialLogState :: Logs
initialLogState = Logs { err = [], warn = [], info = [] }