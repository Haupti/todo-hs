module Config (filepath) where

import Control.Exception (IOException, catch)
import System.Environment (getEnv)

defaultLocation :: String
defaultLocation = "/etc/lib/todo-hs/"

defaultFileName :: String
defaultFileName = "data.hsrn"

filepath :: IO FilePath
filepath = do
  path <- catch (getEnv "HOME") fallback
  return $ path ++ defaultFileName

fallback :: IOException -> IO String
fallback _ = do
  putStrLn $ "WARNING: $HOME is not set. Defaulting to " ++ defaultLocation
  return defaultLocation
