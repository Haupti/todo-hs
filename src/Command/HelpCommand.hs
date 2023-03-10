module Command.HelpCommand where

import Classes (PresentableProvider (..))
import Logger (WithLogs)
import Todo (FinalStateProvider (..), Todo (..), TodoState (..))

newtype HelpCommandResult = HelpCommandResult TodoState

instance FinalStateProvider HelpCommandResult where
  provideFinalState (HelpCommandResult x) = x

instance PresentableProvider HelpCommandResult where
  providePresentable (HelpCommandResult _) =
    unlines
      [ "add                adds todos to active. takes list of space separated todo descriptions",
        "done               checks todos and moves them to done. takes list of space separated numbers",
        "list [--done]      lists active todos. lists done todos with check date when using option --done.",
        "purge              purges active todos from existence, does not add to done todos. takes a list of space separated numbers",
        "--help             shows these infos"
      ]

help :: TodoState -> WithLogs HelpCommandResult
help state = return $ HelpCommandResult state
