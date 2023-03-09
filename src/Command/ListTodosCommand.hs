module Command.ListTodosCommand where

import Classes (PresentableProvider (..))
import Logger (WithLogs)
import Todo (Todo (..), TodoState (..), FinalStateProvider(..))

newtype ListTodosCommandResult = ListTodosCommandResult TodoState

instance FinalStateProvider ListTodosCommandResult where
  provideFinalState (ListTodosCommandResult x) = x

instance PresentableProvider ListTodosCommandResult where
  providePresentable (ListTodosCommandResult tds) = unlines $ map (\td -> show (orderNumber td) ++ " " ++ todoDescription td) (todos tds)

listTodos :: TodoState -> WithLogs ListTodosCommandResult
listTodos state = return $ ListTodosCommandResult state
