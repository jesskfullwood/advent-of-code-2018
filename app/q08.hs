{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List.Split

data Node = Node [Node] [Int] deriving Show


ingest :: IO [Int]
ingest = do
  raw <- readFile "data/08"
  let vals = read <$> splitOn " " raw
  pure vals


buildTree :: [Int] -> Node
buildTree input =
  fst . buildTree' $ input


buildTree' :: [Int] -> (Node, [Int])
buildTree' (subnodeCt:metaCt:rest) =
  let (subnodes, remainder) = buildSubTree subnodeCt rest
      (meta, remainder') = splitAt metaCt remainder
  in
    (Node subnodes meta, remainder')
buildTree' _ = error "unreachable"


buildSubTree :: Int -> [Int] -> ([Node], [Int])
buildSubTree 0 input = ([], input)
buildSubTree ct input =
  let (seed, rest) = buildTree' input
      collected :: [(Node, [Int])] = take ct $ iterate (\(_, inp) -> buildTree' inp) (seed, rest)
      retInput = snd . last $ collected
  in
    (fmap fst collected, retInput)


sumMeta :: Node -> Int
sumMeta (Node subnodes meta) = sum meta + (foldl (\acc node -> acc + sumMeta node) 0 subnodes)


scoreNode :: Node -> Int
scoreNode (Node subnodes meta) =
  let accum acc ix =
        acc + case get (ix - 1) subnodes of
          Nothing   -> 0
          Just node -> scoreNode node
  in
  if length subnodes == 0 then
    sum meta
  else
    foldl accum 0 meta


get :: Int -> [a] -> Maybe a
get (-1) _ = Nothing
get ix list =
  if ix < length list then
    Just $ list !! ix
  else
    Nothing


main :: IO ()
main = do
  vals <- ingest
  putStrLn $ "Sum of meta: " ++  (show . sumMeta $ buildTree vals)
  putStrLn $ "Score of root : " ++  (show . scoreNode $ buildTree vals)
