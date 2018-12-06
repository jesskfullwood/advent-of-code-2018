{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error

type Map = M.Map (Int, Int) Int

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void
data Record = Record { rid   :: Int
                     , posn :: (Int, Int)
                     , size :: (Int, Int)
                     }

ingest :: IO [String]
ingest = lines <$> readFile "data/03"

parseLine :: String -> Either Error Record
parseLine line = parse parseRecord "" line

parseRecord :: Parser Record
parseRecord = do
  char '#'
  id <- L.decimal
  space
  char '@'
  space
  xpos <- L.decimal
  char ','
  ypos <- L.decimal
  char ':'
  space
  xsize <- L.decimal
  char 'x'
  ysize <- L.decimal
  pure $ Record id (xpos, ypos) (xsize, ysize)


posnCounts :: [Record] -> Map
posnCounts records =
  foldl updateCounts M.empty records


updateCounts :: Map -> Record -> Map
updateCounts counts record =
  let updateOrInsert maybeCt = Just $ 1 + fromMaybe 0 maybeCt in
  foldl (\ctMap ix -> M.alter updateOrInsert ix ctMap) counts (recordIndices record)


recordIndices :: Record -> [(Int, Int)]
recordIndices (Record _ (xpos, ypos) (xsize, ysize)) =
  do
    x <- [xpos..xpos + xsize - 1]
    y <- [ypos..ypos + ysize - 1]
    [(x, y)]


doesNotOverlap :: Map -> Record -> Bool
doesNotOverlap counts record =
  all (\k -> counts M.! k <= 1) $ recordIndices record


main = do
  lines <- ingest
  let parsed :: Either Error [Record] = sequence $ parseLine <$> lines
  case parsed of
    Left err      -> putStrLn (errorBundlePretty err)
    Right records ->
      let counts = posnCounts $ records
          multipleCliams = filter (\(pos, ct) -> ct > 1) $ M.toList counts
          nonoverlapping = filter (doesNotOverlap counts) records
      in
        do
          putStrLn $ "Squares with multiple claims: " ++ (show . length) multipleCliams
          putStrLn $ "Records with no overlapping claims: " ++ show  (rid <$> nonoverlapping)

