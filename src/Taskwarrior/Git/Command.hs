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
                                                , GitRepo
                                                )
import           Taskwarrior.Git.Merge          ( merge )
import           Taskwarrior.Git.Hooks          ( onAdd
                                                , onModify
                                                )

type RepoOption
  = FilePath <?> "The path to the git repository where your tasks are saved."

type CommitOption = Bool <?> "Commit the changes (if any) to git. UNIMPLEMENTED"

toRepo :: RepoOption -> GitRepo
toRepo = unHelpful

data Command =
    Load { repo :: RepoOption }
  | Save { repo :: RepoOption , commit :: CommitOption }
  | OnAdd { repo :: RepoOption , commit :: CommitOption }
  | OnModify { repo :: RepoOption , commit :: CommitOption }
  | Merge
      (FilePath <?> "The path to the ancestor json task file")
      (FilePath <?> "The path to the current json task file. The result will be written to this file.")
      (FilePath <?> "The path to the json task file to merge into the current file.")
      (Maybe String <?> "Conflict marker. Ignored. We will never report conflicts")
      (Maybe FilePath <?> "Finale name. Ignored.")
   deriving (Generic, Show)

instance ParseRecord Command where
  parseRecord = parseRecordWithModifiers lispCaseModifiers
    { shortNameModifier = firstLetter
    }

command :: IO ()
command = do
  cmd :: Command <- getRecord "taskwarrior-git"
  case cmd of
    Load r       -> load $ toRepo r
    Save     r c -> saveAll (toRepo r) (unHelpful c)
    OnAdd    r c -> onAdd (toRepo r) (unHelpful c)
    OnModify r c -> onModify (toRepo r) (unHelpful c)
    Merge ancestor old new _ _ ->
      merge (unHelpful ancestor) (unHelpful old) (unHelpful new)
