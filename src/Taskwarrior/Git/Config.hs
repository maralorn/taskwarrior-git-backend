module Taskwarrior.Git.Config
  ( Config
  , repository
  , commit
  , readConfig
  )
where

import           Dhall                          ( auto
                                                , input
                                                , FromDhall
                                                )

data Config = Config {
    repository :: Text,
    commit :: Bool
} deriving (Show, Eq, Generic)

instance FromDhall Config

readConfig :: Maybe Text -> IO Config
readConfig configFile = input auto (defaults <> " // " <> config)
 where
  config = fromMaybe
    "(env:TASKWARRIOR_GIT_CONFIG ? ~/.config/taskwarrior-git/config.dhall)"
    configFile
  defaults = "{ commit = False }"
