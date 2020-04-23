module Taskwarrior.Git.Hooks
  ( onAdd
  , onModify
  )
where

import           Taskwarrior.Git.Repo           ( GitRepo )

onAdd :: GitRepo -> Bool -> IO ()
onAdd = undefined

onModify :: GitRepo -> Bool -> IO ()
onModify = undefined
