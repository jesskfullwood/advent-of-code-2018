module Util
    ( insertOrUpdate, maximumByKey
    ) where

import Data.Map.Strict as M
import Data.List
import Data.Maybe


insertOrUpdate :: Ord k => k -> (a -> a) -> a -> M.Map k a -> M.Map k a
insertOrUpdate key updateFunc defaultVal dict =
  M.alter (\mbVal -> Just $ updateFunc (fromMaybe defaultVal mbVal)) key dict


maximumByKey :: Ord b => (a -> b) -> [a] -> a
maximumByKey f list = maximumBy (\l r -> compare (f l) (f r)) list
