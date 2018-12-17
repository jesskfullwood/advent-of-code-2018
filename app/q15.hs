{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List.Split             (chunksOf)
import qualified Data.Map.Strict             as S
import           Data.Sequence               (Seq (..), empty, (|>))
import           Data.Vector.Unboxed         ((!))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Debug.Trace                 as T
import           Util


data Entity = Elf | Goblin deriving Show

type Grid = (V.Vector Bool, Int, Int)
type Coord = (Int, Int)
type Entities = S.Map Coord (Entity, Int)
type Bfs = M.IOVector Int

initHealth :: Int
initHealth = 300

parseFile :: String -> (Grid, Entities)
parseFile str =
  let rows = lines str
      (gridList, entities) = foldrWithIndex (\rowIx row (grid, entities') ->
                    foldrWithIndex (\colIx char (grid', entities'') ->
                                      let coord = (colIx, rowIx) in
                                      case char of
                                          '#' -> (False:grid', entities'')
                                          '.' -> (True:grid', entities'')
                                          'E' -> (True:grid', S.insert coord (Elf, initHealth) entities'')
                                          'G' -> (True:grid', S.insert coord (Goblin, initHealth) entities'')
                                          _  -> error ("Bad char" ++ [char])
                                   ) (grid, entities') row
                    ) ([], S.empty) rows
      nrows = length rows
      ncols = length (rows !! 0)
  in
    ((V.fromList gridList, ncols, nrows), entities)

ingest :: IO (Grid, Entities)
ingest = parseFile <$> readFile "data/15"

up :: Coord -> Coord
up (x, y) = (x, y + 1)

down :: Coord -> Coord
down (x, y) = (x, y - 1)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

get :: Grid -> Coord -> Bool
get (grid, xs, _) (x, y) =
  grid ! (xs * y + x)

readBfs :: Bfs -> Int -> Coord -> IO (Maybe Int)
readBfs bfs xs (x, y) =
    (\v -> if v == 0 then Nothing else Just v) <$> M.read bfs (xs * y + x)

setBfs :: Bfs -> Int -> Coord -> Int -> IO ()
setBfs bfs xs (x, y) val =
    M.write bfs (xs * y + x) val

boolToChar :: Bool -> Char
boolToChar False = '#'
boolToChar True  = '.'

intToChar :: Int -> Char
intToChar i =
  if i == 0 then
    '#'
  else if i < 10 then
    (head . show) i
  else if i == sourceInt then
    '*'
  else
    '+'

showGrid :: Grid -> String
showGrid (grid, xs, _) =
  unlines . (chunksOf xs) $ boolToChar <$> V.toList grid


showDijkstra :: V.Vector Int -> Int -> String
showDijkstra bfs xs =
  unlines . (chunksOf xs) $ intToChar <$> V.toList bfs

sourceInt = 9999

dijkstra :: Grid -> Coord -> IO (V.Vector Int)
dijkstra grid@(grid', _, _) source = do
  let
    -- set initial positions
    posns = empty |> (source, sourceInt) |> (up source, 1) |> (down source, 1) |> (left source, 1) |> (right source, 1)
    bfs = M.new (V.length grid')
  bfs' <- dijkstra' grid source bfs posns
  V.freeze bfs'

dijkstra' :: Grid -> Coord -> IO Bfs -> Seq (Coord, Int) -> IO Bfs
dijkstra' _ _ bfs Empty = bfs  -- No more positions to visit
dijkstra' grid@(_, xs, _) source bfs ((nextPosn,dist):<|posns) =
  if (T.traceShow nextPosn) get grid nextPosn then
    -- next position is reachable
    do
      bfs' <- bfs
      alreadyReached <- readBfs bfs' xs nextPosn
      case alreadyReached of
        Just _ -> dijkstra' grid source bfs posns
        Nothing -> do
          -- We have found a new reachable position
          let nextDist = dist + 1
          let posns' = posns
                |> (up nextPosn, nextDist)
                |> (down nextPosn, nextDist)
                |> (left nextPosn, nextDist)
                |> (right nextPosn, nextDist)
          () <- setBfs bfs' xs nextPosn dist
          dijkstra' grid source (pure bfs') posns'
  else
    dijkstra' grid source bfs posns


main :: IO ()
main = do
  (grid@(_, xs, _), entities) <- ingest
  bfs <- dijkstra grid (22, 22)
  putStrLn $ showGrid grid
  putStrLn $ showDijkstra bfs xs
  pure ()
