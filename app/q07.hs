module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Util
import           Debug.Trace
import           Data.List

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

identifyRoots :: Network -> S.Set Char
identifyRoots (upstream, _) =
  S.fromList $ fst <$> filter ((== 0) . length . snd) (M.toList upstream)

main :: IO ()
main = do
  pairs <- ingest
  let network = buildNetwork pairs
  putStrLn $ show $ (runNodes network)
