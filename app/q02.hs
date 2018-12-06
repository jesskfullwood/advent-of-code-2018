{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List
import           Debug.Trace


ingest :: IO [String]
ingest =
  lines <$> readFile "data/02"


runCount :: String -> Int -> Bool
runCount (startChar: word) target = runCount' word target 1 startChar

runCount' :: String -> Int -> Int -> Char -> Bool
runCount' [] targetCt curCt _ = targetCt == curCt
runCount' (c: rest) targetCt curCt prevChar =
  if c == prevChar then
    runCount' rest targetCt (curCt + 1) prevChar
  else
    if targetCt == curCt then
      True
    else
    runCount' rest targetCt 1 c

countTwos :: String -> Bool
countTwos word = runCount word 2

countThrees :: String -> Bool
countThrees word = runCount word 3

boolInt :: Bool -> Int
boolInt True  = 1
boolInt False = 0


countDiffs :: String -> String -> Int
countDiffs word1 word2 = foldl (\acc (l, r) -> acc + boolInt (l /= r)) 0 $ zip word1 word2

filterDiffs :: String -> String -> String
filterDiffs word1 word2 = fmap fst $ filter (uncurry (==)) $ zip word1 word2

main :: IO ()
main = do
  wordList <- ingest
  let sortedStrs = sort <$> wordList
  let twoCt = foldl (\ct inp -> ct + boolInt (countTwos inp)) 0 sortedStrs
  let threeCt = foldl (\ct inp -> ct + boolInt (countThrees inp)) 0 sortedStrs
  putStrLn $ "Twos: " ++ show twoCt
  putStrLn $ "Threes: " ++ show threeCt
  putStrLn $ show $ twoCt * threeCt
  let pairs :: [(String, String)] = wordList >>= (\left -> (wordList >>= \right -> pure (left, right)))
  let unqs = filter (uncurry (>)) pairs
  let differByOne = filter (\(l, r) -> (countDiffs l r) == 1) unqs
  putStrLn $ uncurry filterDiffs $ head differByOne
