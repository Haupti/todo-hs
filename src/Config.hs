module Config (filepath, folderpath, FolderPath) where

import Control.Exception (IOException, catch)
import System.Environment (getEnv)

type FolderPath = FilePath

defaultFolder :: String
defaultFolder = "/todo-hs"

defaultLocation :: String
defaultLocation = "/etc/lib"

defaultFileName :: String
defaultFileName = "/data.hsrn"

folderpath :: IO FilePath
folderpath = do
  path <- catch (getEnv "HOME") fallback
  return $ path ++ defaultFolder

filepath :: IO FilePath
filepath = do
  path <- catch (getEnv "HOME") fallback
  return $ path ++ defaultFolder ++ defaultFileName

fallback :: IOException -> IO String
fallback _ = do
  putStrLn $ "WARNING: $HOME is not set. Defaulting to " ++ defaultLocation
  return defaultLocation
