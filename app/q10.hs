{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import qualified Control.Applicative.Combinators as C
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer      as L
import           Text.Megaparsec.Error
import           Util

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

data Particle = Particle { position :: (Int, Int)
                         , velocity :: (Int, Int)
                         }

type Board = [ Particle ]


boardMinMax :: Board -> ((Int, Int), (Int, Int))
boardMinMax board =
  (
    ( minimum $ (fst . position) <$> board
    , minimum $ (snd . position) <$> board
    ),
    ( maximum $ (fst . position) <$> board
    , maximum $ (snd . position) <$> board
    )
  )

boardArea :: Board -> Int
boardArea board = let ((x, y), (bx, by)) = boardMinMax board
  in
  (bx - x + 1) * (by - y + 1)

renderBoard :: Board -> [[Char]]
renderBoard board =
  let (min, max) = boardMinMax board
      emptyRow = replicate (fst max - fst min + 1) '.'
      emptyGrid = replicate (snd max - snd min + 1) emptyRow
  in
    foldl (\grid p ->
             let (px, py) = position p
                 px' = px - fst min
                 py' = py - snd min
             in
               replaceNth py' (replaceNth px' '#' (grid !! py')) grid
          ) emptyGrid board

printBoard :: Board -> IO ()
printBoard board =
  putStrLn $ unlines . renderBoard $ board


parseLine :: Parser Particle
parseLine = do
  string "position=<"

  space
  pxmul <- C.option 1 $ char '-' *> pure (-1)
  px <- L.decimal

  string ", "
  space
  pymul <- C.option 1 $ char '-' *> pure (-1)
  py <- L.decimal

  string "> velocity=<"
  space
  vxmul <- C.option 1 $ char '-' *> pure (-1)
  vx <- L.decimal

  string ", "
  space
  vymul <- C.option 1 $ char '-' *> pure (-1)
  vy <- L.decimal
  string ">"

  pure $ Particle (pxmul * px, pymul * py) (vxmul * vx, vymul * vy)


parseAll :: Parser Board
parseAll =
  C.many (parseLine <* C.optional newline) <* eof


ingest :: IO Board
ingest = do
  file <- readFile "data/10"
  case parse parseAll "" file of
    Left err  -> error (errorBundlePretty err)
    Right out -> pure out


step :: Board -> Board
step board =
  fmap (\p -> let (px, py) = position p
                  (vx, vy) = velocity p
              in
                p { position = (px + vx, py + vy) }
       ) board


findMinBoardArea :: Board -> (Int, Board)
findMinBoardArea board = findMinBoardArea' board 0 (boardArea board)

findMinBoardArea' :: Board -> Int -> Int -> (Int, Board)
findMinBoardArea' board stepCt prevArea =
  let board' = step board
      newArea = boardArea board'
  in
  if newArea > prevArea then
    (stepCt, board)
  else
    findMinBoardArea' board' (stepCt + 1) newArea


main :: IO ()
main = do
  board <- ingest
  -- printBoard board
  let (stepCt, board') = findMinBoardArea board
  putStrLn $ "Steps: " ++ show stepCt
  printBoard board'
  pure ()
