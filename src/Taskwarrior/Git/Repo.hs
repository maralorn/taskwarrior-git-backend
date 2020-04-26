module Taskwarrior.Git.Repo
  ( load
  , save
  , saveAll
  , GitRepo
  , readTask
  , writeTask
  )
where

import           Taskwarrior.Task               ( status
                                                , description
                                                , Task(Task)
                                                , uuid
                                                )
import qualified Data.Text                     as Text
import qualified Taskwarrior.Status            as Status
import           Data.Aeson                     ( eitherDecodeFileStrict'
                                                , toJSON
                                                , Value(Object)
                                                )
import           Data.Aeson.Encode.Pretty       ( encodePretty'
                                                , defConfig
                                                , confCompare
                                                )
import           Data.HashMap.Strict            ( lookup
                                                , filterWithKey
                                                )
import qualified Data.UUID                     as UUID
import           System.FilePath                ( (</>)
                                                , takeExtension
                                                , dropExtensions
                                                )
import           Taskwarrior.IO                 ( saveTasks
                                                , getTasks
                                                )
import           System.Directory               ( listDirectory
                                                , doesFileExist
                                                )
import           System.Process                 ( readProcess )

type GitRepo = FilePath

readTask :: FilePath -> IO Task
readTask path =
  eitherDecodeFileStrict' path
    >>= either (fail . ("No valid JSON Task at" <> path <> ". Error: " ++)) pure

writeTask :: FilePath -> Task -> IO ()
writeTask path =
  writeFileLBS path
    . encodePretty' defConfig { confCompare = compare }
    . taskToDiskJson

taskToDiskJson :: Task -> Value
taskToDiskJson task = case toJSON task of
  Object obj ->
    Object $ filterWithKey (const . (`notElem` ["id", "urgency"])) obj
  _ -> error ("Task was not encoded as a JSON object. This is a bug.")

readTaskMay :: FilePath -> IO (Maybe Task)
readTaskMay path =
  ifM (doesFileExist path) (Just <$> readTask path) (pure Nothing)

load :: GitRepo -> IO ()
load repo = do
  loadedTasks <- fromList . fmap (\task -> (uuid task, task)) <$> getTasks []
  files       <- filter ((".task" ==) . takeExtension) <$> listDirectory repo
  let taskToLoad path = do
        task     <- readTask (repo </> path)
        fileUuid <-
          maybe (fail (path ++ " has no filename of form '<uuid>.task'.")) pure
          . UUID.fromString
          . dropExtensions
          $ path
        let loaded = lookup fileUuid loadedTasks
        if hasChanged loaded task then pure (Just task) else pure Nothing
  tasksToLoad <- mapMaybe id <$> forM files taskToLoad
  when (not $ null tasksToLoad) $ saveTasks tasksToLoad

checkCleanForWrite :: GitRepo -> [Text] -> IO ()
checkCleanForWrite repo files = do
  statusLines <- gitStatus repo
  when (any ((`notElem` [Unmodified, Untracked]) . gitIndex) statusLines)
    $ fail ("Git repo " <> repo <> " has staged changes.")
  whenJust (find (`elem` (gitPath <$> statusLines)) files) $ \file -> fail
    (  "File "
    <> toString file
    <> " in git repo "
    <> repo
    <> " has uncommited changes."
    )

data GitCode = Unmodified | Untracked | Other deriving (Eq, Show)

data GitStatus = GitStatus {
   gitIndex :: GitCode,
   gitWorktree :: GitCode,
   gitPath :: Text,
   gitOrigPath :: Maybe Text
}

gitStatus :: GitRepo -> IO [GitStatus]
gitStatus path = mapMaybe parseLine . lines . toText <$> callGit
  path
  ["status", "--porcelain"]
  ""

charToGitCode :: Char -> GitCode
charToGitCode ' ' = Unmodified
charToGitCode '?' = Untracked
charToGitCode _   = Other

parseLine :: Text -> Maybe GitStatus
parseLine (toString -> (x : y : ' ' : path)) = Just GitStatus
  { gitIndex    = charToGitCode x
  , gitWorktree = charToGitCode y
  , gitPath     = toText path
  , gitOrigPath = Nothing
  }
parseLine _ = Nothing


callGit :: GitRepo -> [String] -> String -> IO String
callGit repo command = readProcess "git" ("-C" : repo : command)

commit :: GitRepo -> Text -> IO ()
commit repo msg = void $ callGit repo ["commit", "-F", "-"] (toString msg)

stage :: GitRepo -> Text -> IO ()
stage repo file = void $ callGit repo ["add", toString file] ""

save :: GitRepo -> Bool -> [Task] -> IO ()
save repo doCommit tasks = do
  changes <-
    mapMaybe id
      <$> (forM tasks $ \new -> do
            old <- readTaskMay (taskToFilepath repo new)
            if hasChanged old new
              then do
                pure $ Just (old, new)
              else pure Nothing
          )
  whenNotNull changes $ \neChanges -> do
    when doCommit
      $ checkCleanForWrite repo (taskToFilename . snd <$> toList neChanges)
    forM_ changes $ \(_, new) -> do
      writeTask (taskToFilepath repo new) new
      when doCommit $ stage repo (taskToFilename new)
    when doCommit $ commit repo (commitMessage neChanges)

hasChanged :: Maybe Task -> Task -> Bool
hasChanged (Just old) new = taskToDiskJson old /= taskToDiskJson new
hasChanged _          _   = True

commitMessage :: NonEmpty (Maybe Task, Task) -> Text
commitMessage (change :| []) = changeToLine $ getChange change
commitMessage changes =
  Text.intercalate "\n"
    .   (summary (fst <$> processedChanges) <> "\n" :)
    .   toList
    $   changeToLine
    <$> processedChanges
  where processedChanges = getChange <$> changes

count :: (a -> Bool) -> [a] -> Int
count prd = length . filter prd

summary :: NonEmpty TaskChange -> Text
summary (toList -> changes) =
  (Text.intercalate ", " $ mapMaybe
      id
      [ msg (count (== New) changes)       "Add"
      , msg (count (== Changed) changes)   "Modify"
      , msg (count (== Completed) changes) "Complete"
      , msg (count (== Deleted) changes)   "Delete"
      ]
    )
    <> " tasks"
 where
  msg :: Int -> Text -> Maybe Text
  msg 0 _    = Nothing
  msg n text = Just (text <> " " <> show n)

data TaskChange = New | Changed | Completed | Deleted deriving (Show, Eq)

changeToLine :: (TaskChange, Text) -> Text
changeToLine (New      , d) = "Add task " <> d
changeToLine (Changed  , d) = "Modify task " <> d
changeToLine (Completed, d) = "Complete task " <> d
changeToLine (Deleted  , d) = "Delete task " <> d

getChange :: (Maybe Task, Task) -> (TaskChange, Text)
getChange (Nothing, Task {..}) = (New, description)
getChange (Just (Task { status = Status.Completed _ }), Task { status = Status.Completed _, ..})
  = (Changed, description)
getChange (Just _, Task { status = Status.Completed _, ..}) =
  (Completed, description)
getChange (Just (Task { status = Status.Deleted _ }), Task { status = Status.Deleted _, ..})
  = (Changed, description)
getChange (Just _, Task { status = Status.Deleted _, ..}) =
  (Deleted, description)
getChange (_, Task {..}) = (Changed, description)

saveAll :: GitRepo -> Bool -> IO ()
saveAll repo doCommit = getTasks [] >>= save repo doCommit

taskToFilename :: Task -> Text
taskToFilename task = toText (UUID.toString (uuid task) <> ".task")

taskToFilepath :: GitRepo -> Task -> FilePath
taskToFilepath repo task = repo </> toString (taskToFilename task)
