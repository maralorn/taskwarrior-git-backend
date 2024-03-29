module Taskwarrior.Git.Repo (
  load,
  save,
  saveAll,
  GitRepo,
  readTask,
  writeTask,
) where

import Control.Monad.Catch (onException)
import Data.Aeson (
  Value (Object),
  eitherDecodeFileStrict',
  toJSON,
 )
import Data.Aeson.Encode.Pretty (
  confCompare,
  defConfig,
  encodePretty',
 )
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Time.Clock (
  UTCTime,
  getCurrentTime,
 )
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import System.Directory (
  doesFileExist,
  listDirectory,
 )
import System.FilePath (
  dropExtensions,
  takeExtension,
  (</>),
 )
import System.Process (readProcess)
import Taskwarrior.Git.Config (
  Config,
  commit,
  repository,
 )
import Taskwarrior.IO (
  getTasks,
  saveTasks,
 )
import Taskwarrior.Status qualified as Status
import Taskwarrior.Task (
  Task (Task),
  description,
  modified,
  status,
  until,
  uuid,
 )

type GitRepo = FilePath

readTask :: FilePath -> IO Task
readTask path =
  eitherDecodeFileStrict' path
    >>= either (fail . ("No valid JSON Task at" <> path <> ". Error: " ++)) pure

writeTask :: FilePath -> Task -> IO ()
writeTask path =
  writeFileLBS path
    . encodePretty' defConfig{confCompare = compare}
    . taskToDiskJson

taskToDiskJson :: Task -> Value
taskToDiskJson = taskToFilteredJson ["id", "urgency"]

taskToRelevantJson :: Task -> Value
taskToRelevantJson = taskToFilteredJson ["id", "urgency", "modified"]

taskToFilteredJson :: [Text] -> Task -> Value
taskToFilteredJson filteredKeys task = case toJSON task of
  Object obj -> Object $ KeyMap.filterWithKey (\key _ -> Key.toText key `notElem` filteredKeys) obj
  _ -> error "Task was not encoded as a JSON object. This is a bug."

readTaskMay :: FilePath -> IO (Maybe Task)
readTaskMay path =
  ifM (doesFileExist path) (Just <$> readTask path) (pure Nothing)

readSanitizedTask :: Config -> UUID.UUID -> IO Task
readSanitizedTask config uuid = do
  let path = uuidToFilepath (toString $ repository config) uuid
  task <- readTask path
  sanitizedTask <- sanitizeTask task
  when (hasChanged (Just task) sanitizedTask) $ save config [sanitizedTask]
  pure sanitizedTask

sanitizeTask :: Task -> IO Task
sanitizeTask task = do
  now <- getCurrentTime
  pure . checkUntil now $ task

checkUntil :: UTCTime -> Task -> Task
checkUntil now task
  | Status.Pending <- status task
    , Just until_ <- until task
    , until_ < now =
    task
      { status = Status.Deleted now
      , modified = Just now
      }
  | otherwise = task

load :: Config -> IO ()
load config = do
  loadedTasks <- fromList . fmap (\task -> (uuid task, task)) <$> getTasks []
  files <-
    filter ((".task" ==) . takeExtension)
      <$> listDirectory (toString $ repository config)
  let taskToLoad path = do
        fileUuid <-
          maybe (fail (path ++ " has no filename of form '<uuid>.task'.")) pure
            . UUID.fromString
            . dropExtensions
            $ path
        let loaded = Map.lookup fileUuid loadedTasks
        task <- readSanitizedTask config fileUuid
        if hasChanged loaded task then pure (Just task) else pure Nothing
  tasksToLoad <- catMaybes <$> forM files taskToLoad
  unless (null tasksToLoad) $ saveTasks tasksToLoad

checkCleanForWrite :: GitRepo -> IO ()
checkCleanForWrite repo = do
  statusLines <- gitStatus repo
  when (any ((/= Unmodified) . gitIndex) statusLines) $
    fail ("Git repo " <> repo <> " is dirty.")

data GitCode = Unmodified | Untracked | Other deriving stock (Eq, Show)

data GitStatus = GitStatus
  { gitIndex :: GitCode
  , gitWorktree :: GitCode
  , gitPath :: Text
  , gitOrigPath :: Maybe Text
  }

gitStatus :: GitRepo -> IO [GitStatus]
gitStatus path =
  mapMaybe parseLine . lines . toText
    <$> callGit
      path
      ["status", "--porcelain"]
      ""

charToGitCode :: Char -> GitCode
charToGitCode ' ' = Unmodified
charToGitCode '?' = Untracked
charToGitCode _ = Other

parseLine :: Text -> Maybe GitStatus
parseLine (toString -> (x : y : ' ' : path)) =
  Just
    GitStatus
      { gitIndex = charToGitCode x
      , gitWorktree = charToGitCode y
      , gitPath = toText path
      , gitOrigPath = Nothing
      }
parseLine _ = Nothing

callGit :: GitRepo -> [String] -> String -> IO String
callGit repo command = readProcess "git" ("-C" : repo : command)

makeCommit :: GitRepo -> Text -> IO ()
makeCommit repo msg = void $ callGit repo ["commit", "-F", "-"] (toString msg)

reset :: GitRepo -> IO ()
reset repo = void $ callGit repo ["reset", "--hard"] ""

stage :: GitRepo -> Text -> IO ()
stage repo file = void $ callGit repo ["add", toString file] ""

save :: Config -> [Task] -> IO ()
save config tasks = do
  let whenCommit = when (commit config)
      repo = toString $ repository config
  changes <-
    catMaybes
      <$> forM
        tasks
        ( \unsanitizedNew -> do
            new <- sanitizeTask unsanitizedNew
            old <- readTaskMay (taskToFilepath repo new)
            if hasChanged old new
              then do
                pure $ Just (old, new)
              else pure Nothing
        )
  whenNotNull changes $ \neChanges -> do
    whenCommit $ checkCleanForWrite repo
    onException
      ( do
          forM_ changes $ \(_, new) -> do
            writeTask (taskToFilepath repo new) new
            whenCommit $ stage repo (taskToFilename new)
          whenCommit $ makeCommit repo (commitMessage neChanges)
      )
      ( do
          putStrLn $ "Writing changes to repo " <> repo <> " failed"
          whenCommit $ do
            putStrLn "Reseting to HEAD"
            reset repo
      )

hasChanged :: Maybe Task -> Task -> Bool
hasChanged (Just old) new = taskToRelevantJson old /= taskToRelevantJson new
hasChanged _ _ = True

commitMessage :: NonEmpty (Maybe Task, Task) -> Text
commitMessage (change :| []) = changeToLine $ getChange change
commitMessage changes =
  Text.intercalate "\n"
    . (summary (fst <$> processedChanges) <> "\n" :)
    . toList
    $ changeToLine
      <$> processedChanges
 where
  processedChanges = getChange <$> changes

count :: (a -> Bool) -> [a] -> Int
count prd = length . filter prd

summary :: NonEmpty TaskChange -> Text
summary (toList -> changes) =
  Text.intercalate
    ", "
    ( catMaybes
        [ msg (count (== New) changes) "Add"
        , msg (count (== Changed) changes) "Modify"
        , msg (count (== Completed) changes) "Complete"
        , msg (count (== Deleted) changes) "Delete"
        ]
    )
    <> " tasks"
 where
  msg :: Int -> Text -> Maybe Text
  msg 0 _ = Nothing
  msg n text = Just (text <> " " <> show n)

data TaskChange = New | Changed | Completed | Deleted deriving stock (Show, Eq)

changeToLine :: (TaskChange, Text) -> Text
changeToLine (New, d) = "Add task " <> d
changeToLine (Changed, d) = "Modify task " <> d
changeToLine (Completed, d) = "Complete task " <> d
changeToLine (Deleted, d) = "Delete task " <> d

getChange :: (Maybe Task, Task) -> (TaskChange, Text)
getChange (Nothing, Task{..}) = (New, description)
getChange (Just (Task{status = Status.Completed _}), Task{status = Status.Completed _, ..}) =
  (Changed, description)
getChange (Just _, Task{status = Status.Completed _, ..}) =
  (Completed, description)
getChange (Just (Task{status = Status.Deleted _}), Task{status = Status.Deleted _, ..}) =
  (Changed, description)
getChange (Just _, Task{status = Status.Deleted _, ..}) =
  (Deleted, description)
getChange (_, Task{..}) = (Changed, description)

saveAll :: Config -> IO ()
saveAll config = getTasks [] >>= save config

taskToFilename :: Task -> Text
taskToFilename = uuidToFilename . uuid

uuidToFilename :: UUID -> Text
uuidToFilename uuid = toText (UUID.toString uuid <> ".task")

uuidToFilepath :: GitRepo -> UUID -> FilePath
uuidToFilepath repo uuid = repo </> toString (uuidToFilename uuid)

taskToFilepath :: GitRepo -> Task -> FilePath
taskToFilepath repo task = repo </> toString (taskToFilename task)
