{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Util
import qualified Debug.Trace as T
import Data.List
import qualified Data.Sequence as S
import Data.Sequence (Seq (..) )
import Data.Foldable (Foldable(..))

type Mesh = S.Seq (S.Seq Int)

score :: Int -> Int -> Int
score x y =
  let racid = x + 10
      pl = ((racid * y) + serial) * racid
      hundredDigit = (pl `mod` 1000) `div` 100
  in
    hundredDigit - 5


imap :: (Int -> a -> b) -> S.Seq a -> S.Seq b
imap f seqnc =
  fmap (\(ix, val) -> f ix val) $ S.zip (S.fromList [0..length seqnc]) seqnc


newMesh :: Mesh
newMesh =
  let
      mesh = S.replicate sideLen (S.fromList [1..sideLen])
  in
    imap (\rowIx row' -> fmap (\colIx -> score colIx (rowIx + 1)) row') mesh


window :: Int -> Int -> Int -> Mesh -> Int
window size x y mesh =
  foldl (\acc ix -> acc + (sum . (S.take size). (S.drop (x - 1)) $ mesh `S.index` (y - 1 + ix))) 0 [0..size-1]


windowScan :: Mesh -> S.Seq (S.Seq ( S.Seq Int))
windowScan mesh =
  fmap rowScan mesh


rowScan :: S.Seq Int -> S.Seq ( S.Seq Int )
rowScan S.Empty = S.empty
rowScan row@(_:<|rest) =
  (S.scanl1 (+) row) :<| (rowScan rest)


maxWindow :: Mesh -> Int -> ((Int, Int), Int)
maxWindow mesh size =
  let row = [1..sideLen - size + 1]
      windowScores = map (\rowIx -> map (\colIx ->
                                           ((colIx, rowIx), window size colIx rowIx mesh)) row ) row
  in
    (maximumByKey snd . concat) windowScores


maxOfAllWindows :: Mesh -> (((Int, Int), Int), Int)
maxOfAllWindows mesh =
  let scanned = windowScan mesh
      row = [1..300]
  in
  maximumByKey snd $ map (\rowIx ->
         maximumByKey snd $ map (\colIx ->
                maximumByKey snd $ map (\size ->
                      if size + colIx <= 300 && size + rowIx <= 300 && size < 30 then
                        let scannedList = map (\windowRowIx -> (((scanned `S.index` (rowIx + windowRowIx)) `S.index` colIx) `S.index` (size - 1))) [0..size - 1]
                        in
                        (((colIx + 1, rowIx + 1), size), sum scannedList)
                      else
                        (((colIx, rowIx), size), 0)
                   ) row
             ) row
      ) row



showMesh :: Mesh -> IO ()
showMesh mesh =
  putStrLn $ unlines $ map (\row -> intercalate ", " (fmap show (toList row))) (toList mesh)


serial :: Int
serial = 6042

sideLen :: Int
sideLen = 300


main :: IO ()
main = do
  let !mesh = newMesh
  -- print $ windowScan [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
  putStrLn $ "Largest with size 3: " ++ show (maxWindow mesh 3)
  putStrLn $ "Largest of any size: " ++ show (maxOfAllWindows mesh)
  pure ()
