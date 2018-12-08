module Main where

import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Debug.Trace
import           Util

type Point = (Int, Int)

parseIntPair :: String -> Point
parseIntPair line =
  let intStrs = splitOn ", " line in
    (read . head $ intStrs, read . head $ drop 1 intStrs)


ingest :: IO [Point]
ingest = do
  raw <- readFile "data/06"
  pure $ parseIntPair <$> lines raw

manhattanDist :: Point -> Point -> Int
manhattanDist p1 p2 = abs (fst p1 - fst p2) + abs (snd p1 - snd p2)

boundingBox :: [Point] -> (Point, Point)
boundingBox points =
  let xmin = minimum $ fmap fst points
      xmax = maximum $ fmap fst points
      ymin = minimum $ fmap snd points
      ymax = maximum $ fmap snd points
  in ((xmin, ymin), (xmax, ymax))

closestPointToCoord :: [Point] -> Point -> Maybe Int
closestPointToCoord points coord =
  let comparer :: (Int, Maybe Int) -> (Int, Point) -> (Int, Maybe Int)
      comparer (minDist, nearestPointIx) (pointIx, point) =
        let dist = manhattanDist point coord in
            case compare dist minDist of
              GT -> (minDist, nearestPointIx)
              EQ -> (minDist, Nothing)
              LT -> (dist, Just pointIx)
  in
  snd $ foldl comparer (99999, Nothing) $ zip [0..] points

dedup :: Eq a => [a] -> [a]
dedup [] = []
dedup (v1:v2:rest) = if v1 == v2 then
                       dedup (v2:rest)
                     else
                       v1:dedup (v2:rest)
dedup other = other

filterInternalPoints :: Ord a => Int -> Int -> [a] -> [a]
filterInternalPoints xwidth ywidth coords =
  let filterFunc :: (Int, a) -> Bool
      filterFunc (ix, nearestPt) =
            ix < ywidth                        -- bottom row
            || ix > length coords - 1 - ywidth -- top row
            || (ix + 1) `mod` ywidth == 0      -- right col
            || ix `mod` ywidth == 0            -- left col

      forbiddenPoints = map snd . (filter filterFunc) $ zip [0..] coords
  in
    dedup . sort $ forbiddenPoints


mapCordinateSpace :: (Point, Point) -> [Point] -> [Maybe Int]
mapCordinateSpace (minPt, maxPt) points =
  let (minPt, maxPt) = boundingBox points
      coordSpace = [fst minPt .. fst maxPt] >>=
        (\x -> [snd minPt .. snd maxPt] >>=
          (\y -> pure (x, y)))
  in
    fmap (closestPointToCoord points) coordSpace


runLength :: Ord a => [a] -> [(a, Int)]
runLength list = fmap (\x -> (head x, length x)) $ (group . sort) list

sumDistanceToPoints :: [Point] -> Point -> Int
sumDistanceToPoints points coord =
  foldl (\acc point -> acc + manhattanDist point coord) 0 points


calcRegionSize :: [Point] -> Int -> Int
calcRegionSize points limit =
  let (minPt, maxPt) = boundingBox points
      coordSpace = [fst minPt .. fst maxPt] >>=
        (\x -> [snd minPt .. snd maxPt] >>=
          (\y -> pure (x, y)))
  in
    length $ filter (< limit) $ fmap (sumDistanceToPoints points) coordSpace


main ::IO ()
main = do
  points <- ingest
  let bounds@(minPt, maxPt) = boundingBox points
  let xwidth = fst maxPt - fst minPt + 1
  let ywidth = snd maxPt - snd minPt + 1
  let mappedCoords = mapCordinateSpace bounds points
  let forbiddenPoints = catMaybes $ filterInternalPoints xwidth ywidth mappedCoords
  let nearestPointCounts = runLength $ catMaybes mappedCoords
  let validPointCounts = filter (\(ix, ct) -> not $ any (== ix) forbiddenPoints) nearestPointCounts
  putStrLn $ "Largest dangerous region: " ++  (show . snd) (maximumByKey snd validPointCounts)
  putStrLn $ "Largest safe region: " ++  show (calcRegionSize points 10000)
