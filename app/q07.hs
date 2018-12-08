{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.Char
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Util

type Pair = (Char, Char)
type Network = (M.Map Char (S.Set Char), M.Map Char (S.Set Char))

rowToPair :: String -> Pair
rowToPair row =
  (row !! 5, row !! 36)

ingest :: IO [Pair]
ingest = do
  raw <- readFile "data/07"
  pure (rowToPair <$> lines raw)


buildNetwork :: [Pair] -> Network
buildNetwork pairs =
  let updateNetwork :: Network -> Pair -> Network
      updateNetwork (upstream, downstream) (parent, child) =
        let upstream'  = insertOrUpdate child (\parents -> S.insert parent parents) S.empty upstream
            upstream'' = insertOrUpdate parent (\parents -> parents) S.empty upstream'
            downstream'  = insertOrUpdate parent (\children -> S.insert child children) S.empty downstream
            downstream'' = insertOrUpdate child (\children -> children) S.empty downstream'
        in
          (upstream'', downstream'')
  in
    foldl updateNetwork (M.empty, M.empty) pairs


runNodes :: Network -> [Char]
runNodes network =
  let roots = identifyRoots network
  in
    runNodes' network S.empty roots

runNodes' :: Network -> S.Set Char -> S.Set Char -> [Char]
runNodes' network@(upstream, downstream) hasRun toRun =
  if S.size toRun == 0 then [] else
    let nextNode = S.elemAt 0 toRun
        toRun' = S.delete nextNode toRun
        hasRun' = S.insert nextNode hasRun
        children = downstream M.! nextNode
        readyChildren = filter (\child -> (upstream M.! child) `S.isSubsetOf` hasRun') (S.toList children)
        toRun'' = foldl (flip S.insert) toRun' readyChildren
    in
      nextNode : runNodes' network hasRun' toRun''


tick :: M.Map Char Int -> (M.Map Char Int, [Char])
tick runningNodes =
  let updateTick (running, finished) (c, time) =
        if time == 1 then
          (running, c:finished)
        else
          (M.insert c (time - 1) running, finished)
  in
    foldl updateTick (M.empty, []) (M.toList runningNodes)


runNodesWithWorkers :: Network -> Int -> (Int, [Char])
runNodesWithWorkers network workers =
  let roots = identifyRoots network
  in
    runNodesWithWorkers' network workers S.empty M.empty roots


runNodesWithWorkers' :: Network -> Int -> S.Set Char -> M.Map Char Int -> S.Set Char -> (Int, [Char])
runNodesWithWorkers' network@(upstream, downstream) workers hasRun isRunning readyToRun =
  let
    -- perform one tick to update isRunning
    (isRunning', finished) = tick isRunning
    -- add finished tasks to hasRun
    hasRun' = foldl (flip S.insert) hasRun finished
    -- Fetch children of the finished tasks
    children = concat $ map (\nextNode -> S.toList (downstream M.! nextNode)) finished
    -- Filter children to just those which are ready to run
    readyChildren = filter (\child -> (upstream M.! child) `S.isSubsetOf` hasRun') children
    -- Add ready children to the readyToRun list
    readyToRun' = foldl (flip S.insert) readyToRun readyChildren
    -- Fetch some more task to run
    (addToRunning, readyToRun'') = S.splitAt (workers - M.size isRunning') readyToRun'
    -- Add tasks to isRunning
    isRunning'' = foldl (\s c -> M.insert c (charTime c) s) isRunning' addToRunning
  in
  if M.size isRunning'' == 0 then
    (0, finished)
  else
    let (time, res) = runNodesWithWorkers' network workers hasRun' isRunning'' readyToRun'' in
    (time + 1, finished ++ res)

charTime :: Char -> Int
charTime c = (ord c - ord 'A') + 61

identifyRoots :: Network -> S.Set Char
identifyRoots (upstream, _) =
  S.fromList $ fst <$> filter ((== 0) . length . snd) (M.toList upstream)

main :: IO ()
main = do
  pairs <- ingest
  let network = buildNetwork pairs
  putStrLn $ show $ (runNodes network)
  putStrLn $ show $ (runNodesWithWorkers network 5)
