module Main where

import           Data.Maybe
import           Data.Set
import           Text.Read

textToInts :: String -> Maybe [Int]
textToInts text =
  sequence $ signedNumToInt <$> (lines text)

signedNumToInt :: String -> Maybe Int
signedNumToInt num =
  case num of
    '+' : rest -> readMaybe rest
    '-' : rest -> (*) (-1) <$> readMaybe rest
    _          -> Nothing


seenBefore :: Set Int -> Int -> Either Int (Set Int)
seenBefore set freq =
  case lookupIndex freq set of
    Nothing -> Right $ insert freq set
    Just _  -> Left freq

findRepeatedFreq :: [Int] -> Int
findRepeatedFreq freqs =
  let checkFreq (freq:rest) set cum =
        let cumFreq = freq + cum in
        case seenBefore set cumFreq of
          Left res   -> res
          Right set' -> checkFreq rest set' cumFreq
  in
    checkFreq (cycle freqs) empty 0

main :: IO ()
main =
  do
    text <- readFile "data/01"
    let vals = fromMaybe [] $ textToInts text
    putStrLn .  (++) "V1: " $ show $ sum vals
    putStrLn .  (++) "V2: " $ show $ findRepeatedFreq vals
