{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Control.Monad               (foldM)
import           Data.List                   (find, sort, sortOn)
import           Data.List.Split             (chunksOf)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (isNothing, isJust, listToMaybe, mapMaybe)
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
initHealth = 200

parseFile :: String -> (Grid, Entities)
parseFile str =
  let rows = lines str
      (gridList, entities) = foldrWithIndex (\rowIx row (grid, entities') ->
                    foldrWithIndex (\colIx char (grid', entities'') ->
                                      let coord = (rowIx, colIx) in
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
    ((V.fromList gridList, nrows, ncols), entities)

ingest :: IO (Grid, Entities)
ingest = parseFile <$> readFile "data/15"

up :: Coord -> Coord
up (y, x) = (y + 1, x)

down :: Coord -> Coord
down (y, x) = (y - 1, x)

right :: Coord -> Coord
right (y, x) = (y, x + 1)

left :: Coord -> Coord
left (y, x) = (y, x - 1)

isValidSquare :: Grid -> Coord -> Bool
isValidSquare (grid, _, xs) (y, x) =
  grid ! (xs * y + x)

readBfs :: Bfs -> Int -> Coord -> IO (Maybe Int)
readBfs bfs xs (y, x) =
    (\v -> if v == 0 then Nothing else Just v) <$> M.read bfs (xs * y + x)

setBfs :: Bfs -> Int -> Coord -> Int -> IO ()
setBfs bfs xs (y, x) val =
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
showGrid (grid, _ys, xs) entities =
  let chars = boolToChar <$> V.toList grid
      chars'' = foldl (\chars' ((y, x), entity) ->
                        update chars' (y * xs + x) (entityToChar entity)) chars (Map.toList entities)
  in
  unlines (chunksOf xs chars'')


showDijkstra :: V.Vector Int -> Int -> String
showDijkstra bfs xs =
  unlines . (chunksOf xs) $ intToChar <$> V.toList bfs

sourceInt :: Int
sourceInt = 9999

entityTurn :: Grid -> Entities -> Entity -> IO Entities
entityTurn grid entities entity@(c, coord, hp) = do
  let couldAttack = isJust (willAttack (trace "attacking: " entity) entities)
  mbNextStep <- if couldAttack then
                 pure Nothing -- we can attack, so don't try to move
               else
                 moveEntity grid entities entity
  let (entity', updatedEntities) = case mbNextStep of
        Nothing -> (entity, entities)  -- nothing to be done
        Just newCoord ->
          let
            -- update the entites map with new coords
            updatedEntity = (c, newCoord, hp)
            removedOldEntity = Map.delete coord entities
          in
            (updatedEntity, Map.insert newCoord updatedEntity removedOldEntity)
  case willAttack entity' updatedEntities of
    -- do the attack
    Just (Goblin, coord', _) -> pure $ Map.update (\(c', _, hp') ->
                                               if hp' > elfPower then Just (c', coord', hp' - elfPower) else Nothing
                                            ) coord' updatedEntities
    Just (Elf, coord', _) -> pure $ Map.update (\(c', _, hp') ->
                                               if hp' > 3 then Just (c', coord', hp' - 3) else Nothing
                                            ) coord' updatedEntities
    Nothing -> pure updatedEntities

willAttack :: Entity -> Entities -> Maybe Entity
willAttack (c, coord, _) entities =
  let lookup' f = Map.lookup (f coord) entities >>= (\e2@(c2, _, _) -> if c /= c2 then Just e2 else Nothing)
  in listToMaybe . (sortOn (\(_, coord', hp) -> (hp, coord'))) . (mapMaybe lookup') $ [up, left, right, down]  -- attack order

getDist :: Distances -> Coord -> Maybe Int
getDist (vec, xs) (y, x) = case vec ! (xs * y + x) of
  0   -> Nothing  -- This location is unreachable
  val -> Just val

closestOfGivenPositions :: Distances -> [Coord] -> Maybe (Int, Coord)
closestOfGivenPositions dist targets =
  (listToMaybe . sort) $ mapMaybe (\t -> (,t) <$> getDist dist t ) targets

surroundingCoords :: Coord -> [Coord]
surroundingCoords coord = [up, left, right, down] <*> [coord]

traceRoute :: Distances -> Coord -> Coord -> [Coord]
traceRoute dists fromPosn target =
  let traceRoute' fromPosn' route =
        let surrounds = surroundingCoords fromPosn'
        in
          case find (\pos -> pos == target) surrounds of
            Just found -> (found:route)  -- you have reached your destination
            Nothing -> case closestOfGivenPositions dists surrounds of
              Just (_dist, closest) -> traceRoute' closest (closest:route)
              Nothing               -> error "No route found"
  in
    traceRoute' fromPosn [fromPosn]

moveEntity :: Grid -> Entities -> Entity -> IO (Maybe Coord)
moveEntity grid@(_, xs, _) entities (c, coord, _) = do
  bfs <- dijkstra grid entities coord
  let dists = (bfs, xs)
      enemies = Map.filter (\(c2, _, _) -> c /= c2) entities
      positionsAdjacentToEnemies :: [Coord] =  concat $ map surroundingCoords (Map.keys enemies)
  case closestOfGivenPositions dists positionsAdjacentToEnemies of
    Just (_dist, closest) ->
      let (_origPosn:nextStep:_rest) = traceRoute dists closest coord
      in
        pure (Just nextStep)
    Nothing -> pure Nothing  -- There is no enemy which can be reached


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
  foldM (\entities' (_, coord, _) ->
           case Map.lookup coord entities' of
             Nothing -> pure entities'
             Just entity -> entityTurn grid entities' entity
        ) entities (Map.elems entities)

runToEnd :: Int -> Grid -> Entities -> IO (Int, Entities)
runToEnd tick grid entities = do
  entities' <- stepRound grid entities
  putStrLn ("Tick: " ++ show tick)
  putStrLn . unlines $ show <$> (Map.elems entities')
  if (Map.keys entities) /= (Map.keys entities') then putStrLn $ showGrid grid entities' else pure ()
  if (length $ filter (\(c, _, _) -> c == Elf) (Map.elems entities')) == 0
    || (length $ filter (\(c, _, _) -> c == Goblin) (Map.elems entities')) == 0 then
    pure (tick, entities')
  else
    runToEnd (tick + 1) grid entities'

score :: Int -> Entities -> Int
score rounds entities =
  rounds * (sum $ map (\(_, _, hp) -> hp) (Map.elems entities))

elfPower :: Int
elfPower = 23 -- set to 3 to win part 1

main :: IO ()
main = do
  (grid, entities) <- ingest
  print entities
  print . length $ filter (\(c, _, _) -> c == Elf) (Map.elems entities)
  putStrLn $ showGrid grid entities
  (rounds, entities') <- runToEnd 0 grid entities
  putStrLn $ "Score: " ++ (show $ score rounds entities')

  print . length $ filter (\(c, _, _) -> c == Elf) (Map.elems entities')
  pure ()
