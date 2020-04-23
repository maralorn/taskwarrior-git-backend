-- | This Module provides a threeway merge function for Tasks as for example useful for git merges.
--
module Taskwarrior.Git.Merge
  ( merge
  )
where

import           Prelude                 hiding ( id )
import           Taskwarrior.Task               ( Task(..) )
import           Taskwarrior.UDA                ( UDA )
import           Control.Applicative            ( liftA2 )
import           Data.HashMap.Strict            ( keys
                                                , lookup
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Taskwarrior.Git.Repo           ( writeTask
                                                , readTask
                                                )

merge :: FilePath -> FilePath -> FilePath -> IO ()
merge ancestorPath oldPath newPath = do
  ancestor <- readTask ancestorPath
  old      <- readTask oldPath
  new      <- readTask newPath
  writeTask oldPath $ threeWayMerge ancestor old new


-- | This merges two tasks given a common ancestor. The order is:
--
-- @'threeWayMerge' ancestor old new@
--
-- Warning: This merge algorithm always finds a solution. This solution is probably reasonable but not necessarily "correct" and may lead to data loss.
--
--
-- The merge happens by the following rules:
-- 1. You most likely shouldn‘t merge tasks with different UUIDs, but since this function does not need to rely on it, it will merge the uuid like any other editable field.
-- 2. If both new and old have a modified field we regard the task with the newer modified field as the new task.
-- 3. For most fields, including fields in UDA we take the change if only one task changed and the field from the new task if both tasks changed.
-- 4. The following fields are handled more delicately: annotations, tags, depends
threeWayMerge :: Task -> Task -> Task -> Task
threeWayMerge ancestor old' new'
  | Just True == liftA2 (<=) (modified new') (modified old') = threeWayMerge'
    new'
    old'
  | otherwise = threeWayMerge' old' new'
 where
  threeWayMerge' old new = Task
    { status         = mergeSimpleField status ancestor old new
    , uuid           = mergeSimpleField uuid ancestor old new
    , id             = mergeSimpleField id ancestor old new
    , entry          = mergeSimpleField entry ancestor old new
    , start          = mergeSimpleField start ancestor old new
    , recurringChild = mergeSimpleField recurringChild ancestor old new
    , description    = mergeSimpleField description ancestor old new
    , modified       = mergeSimpleField modified ancestor old new
    , due            = mergeSimpleField due ancestor old new
    , until          = mergeSimpleField until ancestor old new
    , annotations    = mergeListField annotations ancestor old new
    , scheduled      = mergeSimpleField scheduled ancestor old new
    , project        = mergeSimpleField project ancestor old new
    , priority       = mergeSimpleField priority ancestor old new
    , depends        = mergeListField depends ancestor old new
    , tags           = mergeListField tags ancestor old new
    , urgency        = mergeSimpleField urgency ancestor old new
    , uda            = mergeUDAField ancestor old new
    }

fieldMerger :: (a -> a -> a -> a) -> (Task -> a) -> Task -> Task -> Task -> a
fieldMerger merger field ancestor old new =
  merger (field ancestor) (field old) (field new)

mergeSimpleField :: Eq a => (Task -> a) -> Task -> Task -> Task -> a
mergeSimpleField = fieldMerger mergeSimple

mergeListField :: Eq a => (Task -> [a]) -> Task -> Task -> Task -> [a]
mergeListField = fieldMerger mergeList

mergeUDAField :: Task -> Task -> Task -> UDA
mergeUDAField = fieldMerger mergeUDA uda

mergeSimple :: Eq a => a -> a -> a -> a
mergeSimple ancestor old new | ancestor == new = old
                             | -- This covers the cases new == old.
                               otherwise       = new

mergeList :: Eq a => [a] -> [a] -> [a] -> [a]
mergeList ancestor old new =
  filter (\a -> not (a `elem` ancestor && a `notElem` old)) new
    <> filter (\a -> a `notElem` new && a `notElem` ancestor) old

mergeUDA :: UDA -> UDA -> UDA -> UDA
mergeUDA ancestor old new =
  let newKeys   = keys new
      oldKeys   = keys old
      jointKeys = newKeys ++ filter (`notElem` newKeys) oldKeys
  in  fromList $ mapMaybe
        (\key -> (key, ) <$> mergeSimple (lookup key ancestor)
                                         (lookup key old)
                                         (lookup key new)
        )
        jointKeys