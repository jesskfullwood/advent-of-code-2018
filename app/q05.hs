module Main where

import           Data.Char

reverseChar :: Char -> Char
reverseChar c =
  if isLower c then
    toUpper c
  else
    toLower c


scanAndRemove :: [Char] -> [Char]
scanAndRemove input =
  case input of
    (c1:c2:rest) ->
      if c1 == reverseChar c2 then
        scanAndRemove rest
      else
        c1 : scanAndRemove (c2:rest)
    _ -> input


chainReaction :: [Char] -> [Char]
chainReaction input =
  let input' = scanAndRemove input
  in
    if length input' == length input then
      input
    else
      chainReaction input'


removeInstances :: [Char] -> Char -> [Char]
removeInstances [] _ = []
removeInstances (v:rest) c =
  if toLower v == c then
    removeInstances rest c
  else (v:removeInstances rest c)


testPolymerRemoval :: [Char] -> [Int]
testPolymerRemoval input =
  let alphabet = "abcdefghijklmnopqrstuvwxyz" in
  fmap (\c -> length . chainReaction $ removeInstances input c) alphabet


main :: IO ()
main = do
  input <- filter isAlpha <$> readFile "data/05"
  let result = chainReaction input
  let minSize = minimum $ testPolymerRemoval input
  putStrLn $ "Collapsed size: " ++  (show . length $ result)
  putStrLn $ "Min removed size: " ++ (show minSize)
