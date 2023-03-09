module Command.ListDonesCommand where

import Classes (PresentableProvider (..))
import Logger (WithLogs)
import Todo (DoneTodo (..), TodoState (..), FinalStateProvider(..))

newtype ListDonesCommandResult = ListDonesCommandResult TodoState

instance FinalStateProvider ListDonesCommandResult where
  provideFinalState (ListDonesCommandResult x) = x

instance PresentableProvider ListDonesCommandResult where
  providePresentable (ListDonesCommandResult tds) = unlines $ map toPresentable (reverse (doneTodos tds))
        where toPresentable d = providePresentable (timeStamp d) ++ " " ++ doneDescription d
 

listDones :: TodoState -> WithLogs ListDonesCommandResult
listDones state = return $ ListDonesCommandResult state
