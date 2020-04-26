module Taskwarrior.Git.Hooks
  ( onAdd
  , onModify
  )
where

import           Taskwarrior.Git.Repo           ( GitRepo
                                                , save
                                                )
import qualified Taskwarrior.IO                as TWIO

onAdd :: GitRepo -> Bool -> IO ()
onAdd repo doCommit = TWIO.onAdd $ \task -> do
  save repo doCommit [task]
  pure task

onModify :: GitRepo -> Bool -> IO ()
onModify repo doCommit = TWIO.onModify $ \_ modified -> do
  save repo doCommit [modified]
  pure modified
