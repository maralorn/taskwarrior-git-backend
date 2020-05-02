module Taskwarrior.Git.Hooks
  ( onAdd
  , onModify
  )
where

import           Taskwarrior.Git.Repo           ( save )
import           Taskwarrior.Git.Config         ( Config )
import qualified Taskwarrior.IO                as TWIO
import           Taskwarrior.Task               ( modified )
import           Data.Time.Clock                ( getCurrentTime )

onAdd :: Config -> IO ()
onAdd config = TWIO.onAdd $ \task -> do
  save config [task]
  pure task

onModify :: Config -> IO ()
onModify config = TWIO.onModify $ \_ modified -> do
  now <- getCurrentTime -- Overwriting the modified timestamp here because taswarrior seems to only set it after running the hook
  save config [modified { modified = Just now }]
  pure modified
