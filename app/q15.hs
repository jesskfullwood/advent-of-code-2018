{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Monad               (foldM)
import           Data.List                   (find, sortOn)
import           Data.List.Split             (chunksOf)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isNothing, listToMaybe, mapMaybe)
import           Data.Sequence               (Seq (..), empty, (|>))
import           Data.Vector.Unboxed         ((!))
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Debug.Trace                 as T
import           Util


data Creature = Elf | Goblin deriving (Show, Eq)
type Entity = (Creature, Coord, Int)

entityToChar :: Entity -> Char
entityToChar (Elf, _, _)    = 'E'
entityToChar (Goblin, _, _) = 'G'

type Grid = (V.Vector Bool, Int, Int)
type Coord = (Int, Int)
type Entities = Map.Map Coord Entity
type Bfs = M.IOVector Int
type Distances = (V.Vector Int, Int)

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
                                          'E' -> (True:grid', Map.insert coord (Elf, coord, initHealth) entities'')
                                          'G' -> (True:grid', Map.insert coord (Goblin, coord, initHealth) entities'')
                                          _  -> error ("Bad char" ++ [char])
                                   ) (grid, entities') row
                    ) ([], Map.empty) rows
      nrows = length rows
      ncols = length (rows !! 0)
  in
    ((V.fromList gridList, ncols, nrows), entities)

ingest :: IO (Grid, Entities)
ingest = parseFile <$> readFile "data/15-test"

up :: Coord -> Coord
up (x, y) = (x, y + 1)

down :: Coord -> Coord
down (x, y) = (x, y - 1)

right :: Coord -> Coord
right (x, y) = (x + 1, y)

left :: Coord -> Coord
left (x, y) = (x - 1, y)

isValidSquare :: Grid -> Coord -> Bool
isValidSquare (grid, xs, _) (x, y) =
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

update :: [a] -> Int -> a -> [a]
update list posn val =
  take posn list ++ val:(drop (posn + 1) list)

showGrid :: Grid -> Entities -> String
showGrid (grid, xs, _) entities =
  let chars = boolToChar <$> V.toList grid
      chars'' = foldl (\chars' ((x, y), entity) ->
                        update chars' (y * xs + x) (entityToChar entity)) chars (Map.toList entities)
  in
  unlines (chunksOf xs chars'')


showDijkstra :: V.Vector Int -> Int -> String
showDijkstra bfs xs =
  unlines . (chunksOf xs) $ intToChar <$> V.toList bfs

sourceInt :: Int
sourceInt = 9999

entityTurn :: Grid -> Entities -> Entity -> IO Entities
entityTurn grid entities entity =
  case willAttack (trace "Entity turn: " entity) entities of
    -- do the attack
    Just (_, coord, _) -> pure $ Map.update (\(c, _, hp) -> if hp > 3 then Just (c, coord, hp - 3) else Nothing) coord entities
    Nothing -> moveEntity grid entities (trace "Move entity: " entity)

willAttack :: Entity -> Entities -> Maybe Entity
willAttack (c, coord, _) entities =
  let lookup' f = Map.lookup (f coord) entities >>= (\e2@(c2, _, _) -> if c /= c2 then Just e2 else Nothing)
  in listToMaybe . (mapMaybe lookup') $ [up, left, right, down]  -- attack order

getDist :: Distances -> Coord -> Maybe Int
getDist (vec, xs) (x, y) = case vec ! (xs * y + x) of
  0   -> Nothing  -- This location is unreachable
  val -> Just val

closestOfGivenPositions :: Distances -> [Coord] -> Maybe (Int, Coord)
closestOfGivenPositions dist targets =
  let sorter (dist', (x, y)) = (dist', y, x)
  in
  trace "Chosen closest: " $ (listToMaybe . (trace "sorted options") . (sortOn sorter)) $ mapMaybe (\t -> (,t) <$> getDist dist t ) ( trace "Targets :" targets)

surroundingCoords :: Coord -> [Coord]
surroundingCoords coord = [up, left, right, down] <*> [coord]

traceRoute :: Distances -> Coord -> Coord -> [Coord]
traceRoute dists fromPosn target =
  let traceRoute' fromPosn' route =
        let surrounds = surroundingCoords fromPosn'
        in
          case find (\pos -> pos == trace "home: " target) (trace "move options: " surrounds) of
            Just found -> (found:route)  -- you have reached your destination
            Nothing -> case closestOfGivenPositions dists surrounds of
              Just (_dist, closest) -> traceRoute' closest (closest:route)
              Nothing               -> error "No route found"
  in
    trace "Chosen route: " $ traceRoute' fromPosn [fromPosn]

moveEntity :: Grid -> Entities -> Entity -> IO Entities
moveEntity grid@(_, xs, _) entities (c, coord, hp) = do
  bfs <- dijkstra grid entities coord
  let dists = (bfs, xs)
      enemies = Map.filter (\(c2, _, _) -> c /= c2) entities
      positionsAdjacentToEnemies :: [Coord] =  concat $ map surroundingCoords (Map.keys enemies)
  case closestOfGivenPositions dists positionsAdjacentToEnemies of
    Just (_dist, closest) ->
      let (_origPosn:nextStep:_rest) = traceRoute dists closest coord
          addedEntity = trace "added new posn " $ Map.insert nextStep (c, nextStep, hp) entities
      in
        pure (Map.delete coord addedEntity)
    Nothing -> pure entities  -- There is no enemy which can be reached


dijkstra :: Grid -> Entities -> Coord -> IO (V.Vector Int)
dijkstra grid@(grid', _, _) entities source = do
  let
    -- set initial positions
    posns = empty |> (source, sourceInt) |> (up source, 1) |> (left source, 1) |> (right source, 1) |> (down source, 1)
    bfs = M.new (V.length grid')
  bfs' <- dijkstra' grid entities source bfs posns
  V.freeze bfs'

dijkstra' :: Grid -> Entities -> Coord -> IO Bfs -> Seq (Coord, Int) -> IO Bfs
dijkstra' _ _ _ bfs Empty = bfs  -- No more positions to visit
dijkstra' grid@(_, xs, _) entities source bfs ((nextPosn,dist):<|posns) =
  if (isValidSquare grid nextPosn) && (isNothing $ Map.lookup nextPosn entities) then
    -- next position is reachable
    do
      bfs' <- bfs
      alreadyReached <- readBfs bfs' xs nextPosn
      case alreadyReached of
        Just _ -> dijkstra' grid entities source bfs posns
        Nothing -> do
          -- We have found a new reachable position
          let nextDist = dist + 1
          let posns' = posns
                |> (up nextPosn, nextDist)
                |> (left nextPosn, nextDist)
                |> (right nextPosn, nextDist)
                |> (down nextPosn, nextDist)
          () <- setBfs bfs' xs nextPosn dist
          dijkstra' grid entities source (pure bfs') posns'
  else
    dijkstra' grid entities source bfs posns

stepRound :: Grid -> Entities -> IO Entities
stepRound grid entities =
  foldM (\ioentities entity -> entityTurn grid ioentities entity) entities (Map.elems entities)

runToEnd :: Int -> Grid -> Entities -> IO Entities
runToEnd tick grid entities = do
  putStrLn ("Tick: " ++ show tick)
  entities' <- stepRound grid entities
  if entities /= entities' then
    putStrLn $ showGrid grid entities'
    else
    pure ()
  if (length $ filter (\(c, _, _) -> c == Elf) (Map.elems entities')) == 0 then
    pure entities'
  else
    runToEnd (tick + 1) grid entities'

main :: IO ()
main = do
  (grid, entities) <- ingest
  print entities
  putStrLn $ showGrid grid entities
  _ <- runToEnd 0 grid entities
  pure ()
