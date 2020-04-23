module Taskwarrior.Git.Repo
  ( load
  , save
  , saveAll
  , GitRepo
  , readTask
  , writeTask
  )
where

import           Taskwarrior.Task               ( Task
                                                , uuid
                                                )
import           Data.Aeson                     ( eitherDecodeFileStrict'
                                                , encodeFile
                                                , toJSON
                                                , Value(Object)
                                                )
import           Data.HashMap.Strict            ( filterWithKey )
import           Data.UUID                     as UUID
import           System.FilePath                ( (</>) )
import           Taskwarrior.IO                 ( getTasks )

type GitRepo = FilePath

readTask :: FilePath -> IO Task
readTask path =
  eitherDecodeFileStrict' path
    >>= either (fail . ("No valid JSON Task at" <> path <> ". Error: " ++)) pure

writeTask :: FilePath -> Task -> IO ()
writeTask path task = do
  case toJSON task of
    Object obj -> encodeFile path
      $ filterWithKey (const . (`notElem` ["id", "urgency"])) obj
    _ -> fail ("Task was not encoded as a JSON object. This is a bug.")

load :: GitRepo -> IO ()
load = undefined

save :: GitRepo -> Task -> IO ()
save repo task = writeTask (taskToFilepath repo task) task

saveAll :: GitRepo -> Bool -> IO ()
saveAll repo doCommit = getTasks [] >>= mapM_ (save repo)

taskToFilename :: Task -> FilePath
taskToFilename task = UUID.toString (uuid task) <> ".task"

taskToFilepath :: GitRepo -> Task -> FilePath
taskToFilepath repo task = repo </> taskToFilename task
