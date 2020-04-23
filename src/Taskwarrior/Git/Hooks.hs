module Taskwarrior.Git.Hooks
  ( onAdd
  , onModify
  )
where

import           Taskwarrior.Git.Repo           ( GitRepo )

onAdd :: GitRepo -> IO ()
onAdd = undefined

onModify :: GitRepo -> IO ()
onModify = undefined
