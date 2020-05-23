module Taskwarrior.Git.Command
  ( command
  )
where

import           Options.Generic                ( type (<?>)
                                                , getRecord
                                                , ParseRecord(parseRecord)
                                                , shortNameModifier
                                                , firstLetter
                                                , parseRecordWithModifiers
                                                , lispCaseModifiers
                                                , unHelpful
                                                )
import           Taskwarrior.Git.Repo           ( load
                                                , saveAll
                                                )
import           Taskwarrior.Git.Merge          ( merge )
import           Taskwarrior.Git.Hooks          ( onAdd
                                                , onModify
                                                )
import           Taskwarrior.Git.Config         ( readConfig
                                                , Config
                                                )

type ConfigFileOpt
  = Maybe Text <?> "Path to config file. Default: $TASKWARRIOR_GIT_CONFIG or ~/.config/taskwarrior-git/config.dhall"

data Command =
    Load { config :: ConfigFileOpt }
  | Save { config :: ConfigFileOpt }
  | OnAdd { config :: ConfigFileOpt }
  | OnModify { config :: ConfigFileOpt }
  | Merge
      (FilePath <?> "The path to the ancestor json task file")
      (FilePath <?> "The path to the current json task file. The result will be written to this file.")
      (FilePath <?> "The path to the json task file to merge into the current file.")
   deriving (Generic, Show)

instance ParseRecord Command where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = firstLetter
    }

getConfig :: ConfigFileOpt -> IO Config
getConfig = readConfig . unHelpful

command :: IO ()
command = do
  cmd :: Command <- getRecord "taskwarrior-git"
  case cmd of
    Load     configFile -> load =<< getConfig configFile
    Save     configFile -> saveAll =<< getConfig configFile
    OnAdd    configFile -> onAdd =<< getConfig configFile
    OnModify configFile -> onModify =<< getConfig configFile
    Merge ancestor old new ->
      merge (unHelpful ancestor) (unHelpful old) (unHelpful new)
