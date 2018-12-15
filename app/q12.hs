module Main where

import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Sequence        (Seq (..))
import qualified Data.Sequence        as S
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

type Board = [Bool]
type Pattern = ([Bool], Bool)

parseElem :: Parser Bool
parseElem = (True <$ char '#') <|> (False <$ char '.')

parsePattern :: Parser Pattern
parsePattern = do
  pat <- some parseElem
  _ <- string " => "
  out <- parseElem
  pure (pat, out)


parseInput :: Parser (Board, [Pattern])
parseInput = do
  _ <- string "initial state: "
  board <- some parseElem
  _ <- eol >> eol
  patterns <- some (parsePattern <* eol)
  pure (board, patterns)

prepareBoard :: Board -> Board
prepareBoard board = reverse . ((++) fiveFalse) . (dropWhile (not.id)) . reverse . dropWhile (not . id) $ board

fiveFalse :: [Bool]
fiveFalse = [False, False,False,False,False]

applyRules :: M.Map (S.Seq Bool) Bool -> (Int, Board) -> (Int, Board)
applyRules rules (offset, board) =
  let
    (_,ret) = foldl (applyRules' rules) (S.fromList fiveFalse, []) (prepareBoard board)
    (empty, rest) = break id ret
  in
    (offset + length empty - 2, rest)


applyRules' :: M.Map (S.Seq Bool) Bool -> (S.Seq Bool, Board) -> Bool -> (S.Seq Bool, Board)
applyRules' rules (_:<|window, boardRet) endElem =
  let
    newWindow = window S.|> endElem
    newElem = fromMaybe False $ (M.lookup newWindow rules)
    out = (newWindow, boardRet ++ [newElem])
  in
    -- T.trace (showBoard (toList newWindow) ++ " => " ++ [b2c newElem]) out
    out


b2c :: Bool -> Char
b2c v = if v then '#' else '.'

showBoard :: (Int, Board) -> String
showBoard (offset, board) =
  fmap b2c board ++ "  =>  " ++ show  (scoreBoard offset board)


ingest :: IO (Board, [Pattern])
ingest = do
  file <- readFile "data/12"
  case parse parseInput "" file of
    Left err                -> error (errorBundlePretty err)
    Right (board, patterns) -> pure (board, patterns)

scoreBoard :: Int -> Board -> Int
scoreBoard startVal board =
  sum $ fst <$> (filter snd $ zip [startVal..] board)

type Repetition = (Board, (Int, Int), (Int, Int))

cachedIterate :: M.Map (S.Seq Bool) Bool -> Board -> Repetition
cachedIterate rules board = cachedIterate' rules M.empty 0 (0, board)

cachedIterate' :: M.Map (S.Seq Bool) Bool -> M.Map Board (Int, Int) -> Int -> (Int, Board) -> Repetition
cachedIterate' rules cache generation (offset, board) =
  let nextBoard = applyRules rules (offset, board) in
  case M.lookup board cache of
    Just (prevGeneration, prevOffset) -> (board, (generation, offset), (prevGeneration, prevOffset))
    Nothing -> cachedIterate' rules (M.insert board (generation, offset) cache) (generation + 1) nextBoard

main :: IO ()
main = do
  putStrLn "Part 1:"
  (newBoard, patterns) <- ingest
  let rules = M.fromList (fmap (\(pats, out) -> (S.fromList pats, out)) patterns)
  let boards = take 21 $ iterate (applyRules rules) (0, newBoard)
  _ <- sequence $ putStrLn . showBoard <$> boards

  putStrLn "Part 2:"
  let (repeatedBoard, (g2, o2), (g1, o1)) = cachedIterate rules newBoard
  print ("Repetition", g1, g2, o1, o2)
  let s1 = scoreBoard o1 repeatedBoard
  let s2 = scoreBoard o2 repeatedBoard
  let scoreDiff = s2 - s1
  let gdiff = g2 - g1
  print $ (scoreDiff * (50000000000 - g1) `div` gdiff) + s1
  pure ()
