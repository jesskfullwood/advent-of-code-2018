{-# LANGUAGE BangPatterns #-}

module Main where

import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import           Data.Sequence   (Seq (..), (|>))
import qualified Data.Sequence   as S
import           Debug.Trace

data Turn = RightTurn | LeftTurn | StraightAhead deriving Show
data Direction = Up | Down | LeftD | RightD deriving Show
data Elf = Elf (Int, Int) Direction Turn deriving Show

nextTurn :: Turn -> Turn
nextTurn LeftTurn      = StraightAhead
nextTurn StraightAhead = RightTurn
nextTurn RightTurn     = LeftTurn

nextDirection :: Direction -> Turn -> (Direction, Turn)
nextDirection dirn turn =
  let next = nextTurn turn
      dirn' = case (dirn, turn) of
        (Up, RightTurn)     -> RightD
        (Up, LeftTurn)      -> LeftD
        (Down, RightTurn)   -> LeftD
        (Down, LeftTurn)    -> RightD
        (RightD, RightTurn) -> Down
        (RightD, LeftTurn)  -> Up
        (LeftD, RightTurn)  -> Up
        (LeftD, LeftTurn)   -> Down
        (d, StraightAhead)  -> d
  in
    (dirn', next)

type Square = (Route, Maybe Elf)

type Board = Seq (Seq Route)
type Elves = M.Map (Int, Int) Elf

newElf :: (Int, Int) -> Direction -> Elf
newElf posn dirn = Elf posn dirn LeftTurn

data Route =
  Horizontal
  | Vertical
  | UpRight
  | UpLeft
  | Intersection
  | None

charToSquare :: Char -> (Int, Int) -> Square
charToSquare c posn =
  case c of
    ' '  -> (None, Nothing)
    '-'  -> (Horizontal, Nothing)
    '|'  -> (Vertical, Nothing)
    '/'  -> (UpRight, Nothing)
    '\\'-> (UpLeft, Nothing)
    '+'  -> (Intersection, Nothing)
    '>'  -> (Horizontal, Just $ newElf posn RightD)
    '<'  -> (Horizontal, Just $ newElf posn LeftD)
    '^'  -> (Vertical, Just $ newElf posn Up)
    'v'  -> (Vertical, Just $ newElf posn Down)
    _    -> error $ "Bad char " ++ [c]


squareToChar :: Route -> Maybe Elf -> Char
squareToChar route Nothing =
    case route of
      Horizontal   -> '-'
      Vertical     -> '|'
      UpRight      -> '/'
      UpLeft       -> '\\'
      Intersection -> '+'
      None         -> ' '
squareToChar _ (Just (Elf _ dirn _)) =
  case dirn of
    Up     -> '^'
    Down   -> 'v'
    RightD -> '>'
    LeftD  -> '<'


foldlWithIndex :: (Int -> b -> a -> b) -> b -> [a] -> b
foldlWithIndex f initial list = foldl (\b (ix, a) -> f ix b a) initial (zip [0..] list)

parseBoard :: String -> (Board, Elves)
parseBoard input =
  foldlWithIndex (\rowIx (board, elves) line ->
            let (boardrow, elves') = foldlWithIndex (\colIx (boardrow', elves'') char ->
                      let (row, mbelf) = charToSquare char (rowIx, colIx) in
                        (boardrow' |> row, fromMaybe elves'' $ fmap (\elf -> M.insert (rowIx, colIx) elf elves'') mbelf)
                  ) (S.empty, elves) line
            in
             (board |> boardrow, elves')
        ) (S.empty, M.empty) $ lines input

get :: Seq (Seq a) -> Int -> Int -> a
get s rowIx colIx = (s `S.index` rowIx) `S.index` colIx

updateElf :: Board -> Elves -> Elf -> Elves
updateElf board elves elf@(Elf (rowIx, colIx) dirn turn) =
  let
    unMaybe mbElf elf' = case mbElf of
      Just (Elf posn' _ _) -> trace ("Removed elves at " ++ (show posn')) Nothing
      Nothing -> Just elf'
    route = get board rowIx colIx
    rmElf (Elf posn _ _) = M.delete posn elves
    up     = ((rowIx - 1), colIx)
    down   = ((rowIx + 1), colIx)
    right  = (rowIx, colIx + 1)
    left   = (rowIx, colIx - 1)
    mvElf elfFrom elfTo@(Elf posn _ _) = M.alter (\mbElf -> unMaybe mbElf elfTo) posn (rmElf elfFrom)
    mvTo = mvElf elf
  in
    case (route, dirn) of
      (None, _)                  -> error "Elf on invalid square"

      (Horizontal, RightD) -> mvTo (Elf right dirn turn)
      (Horizontal, LeftD)  -> mvTo (Elf left dirn turn)
      (Horizontal, _)      -> error "Horizontal error"

      (Vertical, Up)     -> mvTo (Elf up dirn turn)
      (Vertical, Down)   -> mvTo (Elf down dirn turn)
      (Vertical, _) -> error $ "Vertical error: " ++ show elf

      (UpRight, Up    ) -> mvTo $ Elf right RightD turn
      (UpRight, Down  ) -> mvTo $ Elf left  LeftD  turn
      (UpRight, RightD) -> mvTo $ Elf up    Up     turn
      (UpRight, LeftD ) -> mvTo $ Elf down  Down   turn

      (UpLeft, Up)     -> mvTo $ Elf left  LeftD  turn
      (UpLeft, Down)   -> mvTo $ Elf right RightD turn
      (UpLeft, RightD) -> mvTo $ Elf down  Down   turn
      (UpLeft, LeftD)  -> mvTo $ Elf up    Up     turn

      (Intersection, _) ->
        let (dirn', turn') = nextDirection dirn turn in
          case dirn' of
            Up     -> mvTo $ Elf up dirn' turn'
            Down   -> mvTo $ Elf down dirn' turn'
            RightD -> mvTo $ Elf right dirn' turn'
            LeftD  -> mvTo $ Elf left dirn' turn'


tick :: Board -> Elves -> Elves
tick board elves =
    foldl (\elves' posn -> fromMaybe elves' $ fmap (updateElf board elves') (M.lookup posn elves')) elves (M.keys elves)

showBoard :: Board -> Elves -> String
showBoard board elves =
  let toChar rowIx colIx = squareToChar (get board rowIx colIx) (M.lookup (rowIx, colIx) elves)
      nrow = length board
      ncol = length (board `S.index` 0)
      lines' = map (\rowIx -> map (\colIx -> toChar rowIx colIx) [0..ncol-1]) [0..nrow-1]
  in
    unlines lines'

iterToEnd :: Board -> Elves -> (Int, Elves)
iterToEnd board elves = iter' board elves 0

iter' :: Board -> Elves -> Int -> (Int, Elves)
iter' board elves iterCt =
  let
    elves' = tick board elves
  in
    if length elves' == 1 then
      (iterCt + 1, elves')
    else if length elves' == 0 then
      error "No elves remain!"
    else
      iter' board elves' (iterCt + 1)

ingest :: IO (Board, Elves)
ingest = do
  input <- readFile "data/13"
  pure $ parseBoard input

main :: IO ()
main = do
  (board, elves) <- ingest
  putStrLn $ "Starting elves: " ++ (show . length) elves
  let (iterEndCt, elvesEnd) = iterToEnd board elves
  putStrLn $ show (iterEndCt, elvesEnd)
  pure ()
