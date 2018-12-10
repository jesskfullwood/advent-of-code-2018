module Util
    ( insertOrUpdate, maximumByKey, minimumByKey, trace, replaceNth
    ) where

import           Data.List
import qualified Data.Map.Strict as M
import           Data.Maybe
import qualified Debug.Trace     as T


insertOrUpdate :: Ord k => k -> (a -> a) -> a -> M.Map k a -> M.Map k a
insertOrUpdate key updateFunc defaultVal dict =
  M.alter (\mbVal -> Just $ updateFunc (fromMaybe defaultVal mbVal)) key dict


maximumByKey :: Ord b => (a -> b) -> [a] -> a
maximumByKey f list = maximumBy (\l r -> compare (f l) (f r)) list

minimumByKey :: Ord b => (a -> b) -> [a] -> a
minimumByKey f list = minimumBy (\l r -> compare (f l) (f r)) list

trace :: Show a => String -> a -> a
trace s val = T.trace (s ++ show val) val

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
  | n == 0 = newVal:xs
  | otherwise = x:replaceNth (n-1) newVal xs
