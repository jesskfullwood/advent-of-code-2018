module Main where

-- Part 2 uses an alarming amount of RAM! Space leak?


import           Data.Sequence   (Seq (..), (|>))
import qualified Data.Sequence   as S
import Data.Foldable (toList)
import Data.List.Split

type State = (Seq Int, (Int, Int))

newState :: State
newState = (S.fromList [3, 7], (0, 1))

stepState :: State -> State
stepState (seq, (pos1, pos2)) =
  let s1 = seq `S.index` pos1
      s2 = seq `S.index` pos2
      newRecipes = read <$> (fmap pure $ show (s1 + s2))
      seq' = case newRecipes of
        [r1] -> seq |> r1
        [r1, r2] -> seq |> r1 |> r2
  in
    (seq', ((pos1 + s1 + 1) `mod` length seq', (pos2 + s2 + 1) `mod` length seq'))

targetCt = 702831

containsTarget :: S.Seq Int -> Maybe String
containsTarget seq =
  let s = concat $ show <$> toList seq
      targetS = show targetCt
      split' = splitOn targetS s
  in
    if length split' > 1 then
      Just (split' !! 0)
    else
      Nothing


main :: IO ()
main = do
  let iters = iterate stepState newState
  let target = last $ takeWhile (\state -> (length (fst state)) <= targetCt + 10) iters
  let res = (take 10) . (drop targetCt) . toList . fst $ target
  putStrLn $ "Part 1: " ++ (concat $ show <$> res)

  let target2 = head $ drop 20000000 iters
  case containsTarget (fst target2) of
    Just res' -> putStrLn $ "Part 2: " ++ (show . length) res'
    Nothing -> putStrLn "Part 2 failed"
