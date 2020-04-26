module Taskwarrior.Git.Hooks
  ( onAdd
  , onModify
  )
where

import           Taskwarrior.Git.Repo           ( GitRepo
                                                , save
                                                )
import qualified Taskwarrior.IO                as TWIO
import           Taskwarrior.Task               ( modified )
import           Data.Time.Clock                ( getCurrentTime )

onAdd :: GitRepo -> Bool -> IO ()
onAdd repo doCommit = TWIO.onAdd $ \task -> do
  save repo doCommit [task]
  pure task

onModify :: GitRepo -> Bool -> IO ()
onModify repo doCommit = TWIO.onModify $ \_ modified -> do
  now <- getCurrentTime -- Overwriting the modified timestamp here because taswarrior seems to only set it after running the hook
  save repo doCommit [modified { modified = Just now }]
  pure modified
