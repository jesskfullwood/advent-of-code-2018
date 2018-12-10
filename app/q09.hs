{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Primitive     (PrimState)
import           Data.List.Index
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Vector.Unboxed         (freeze, toList)
import qualified Data.Vector.Unboxed.Mutable as V
import           Util


data Game = Game
  { ring            :: [Int]
  , scores          :: M.Map Int Int
  , lastIndex       :: Int
  , lastPlayer      :: Int
  , numPlayers      :: Int
  , lastMarbleScore :: Int
  } deriving Show


newGame :: Int -> Game
newGame numPlayers =
  Game { ring = [0]
       , scores = M.empty
       , lastIndex = 0
       , lastPlayer = numPlayers - 1
       , numPlayers = numPlayers
       , lastMarbleScore = 0
       }


step :: Game -> Game
step game=
  if (lastMarbleScore game + 1) `mod` 23 == 0 then
    removeStep game
  else
    insertStep game


insertStep :: Game -> Game
insertStep game =
  game { ring = insertAt ix val (ring game)
       , lastIndex = ix
       , lastPlayer = (lastPlayer game + 1) `mod` numPlayers game
       , lastMarbleScore = val
       }
  where
    ix = (lastIndex game + 2) `mod` length (ring game)
    val = lastMarbleScore game + 1


removeStep :: Game -> Game
removeStep game =
  game { ring = deleteAt ix (ring game)
       , scores = M.insertWith (+) thisPlayer (val + ring game !! ix) (scores game)
       , lastIndex = ix
       , lastPlayer = thisPlayer
       , lastMarbleScore = val
       }
  where
    thisPlayer = (lastPlayer game + 1) `mod` numPlayers game
    val = lastMarbleScore game + 1
    ix = (lastIndex game + len - 7) `mod` len
    len = length . ring $ game


runGame :: Game -> Int -> Game
runGame game 0 = game
runGame game n = runGame (step game) (n - 1)


nrounds = 71975
nplayers = 416


-- PART 2

data Game' = Game'
  { ring'         :: IO (V.MVector (PrimState IO) (Int, Bool))
  , scores'       :: M.Map Int Int
  , lastInsertIx' :: Int
  , lastPlayer'   :: Int
  , numPlayers'   :: Int
  , roundCt       :: Int
  , totRounds     :: Int
  , length'       :: Int
  }

newGame' :: Int -> Int -> IO Game'
newGame' totRounds numPlayers = do
  ring'' <- V.new (totRounds + 1)
  () <- V.write ring'' 0 (0, True)
  let game = Game' { ring' = pure ring''
                   , scores' = M.empty
                   , lastInsertIx' = 0
                   , lastPlayer' = numPlayers - 1
                   , numPlayers' = numPlayers
                   , roundCt = 0
                   , totRounds = totRounds
                   , length' = 1
                   }
  pure game

step' :: Game' -> IO Game'
step' game =
  if (roundCt game + 1) `mod` 23 == 0 then
    removeStep' game
  else
    do
      g <- insertStep' game
      -- ring' g >>= printVec
      -- putStrLn . show $ (lastInsertIx' g, lastPlayer' g, roundCt g)
      pure g

insertStep' :: Game' -> IO Game'
insertStep' game =
  let
    ixToInsert = (lastInsertIx' game + 2) `mod` (roundCt game + 1)
    posn = roundCt game + 1

    modifyEntry :: (Int, Bool) -> (Int, Bool)
    modifyEntry (ix, isLive) =
        if isLive then
          case compare ix ixToInsert of
            LT -> (ix, isLive)
            EQ -> (ix + 1, isLive)
            GT -> (ix + 1, isLive)
        else
          (ix, isLive)

  in
    do
      ring <- ring' game
      empties:: [()] <- sequence $ map (
        \posn -> V.modify ring modifyEntry posn
        ) [0..roundCt game]
      () <- V.write ring posn (ixToInsert, True)
      -- printVec ring
      pure game {
        ring' = pure ring
        , lastInsertIx' = ixToInsert
        , lastPlayer' = (lastPlayer' game + 1) `mod` numPlayers' game
        , roundCt = posn
        , length' = length' game + 1
        }

removeStep' :: Game' -> IO Game'
removeStep' game =
  let
    thisPlayer = (lastPlayer' game + 1) `mod` numPlayers' game
    val = roundCt game + 1
    len = length' game
    ixToRemove = (lastInsertIx' game + len - 7) `mod` len

    modifyEntry :: (Int, Bool) -> (Int, Bool)
    modifyEntry (ix, isLive) =
        if isLive then
          case compare ix ixToRemove of
            LT -> (ix, isLive)
            EQ -> (ix, False)
            GT -> (ix - 1, isLive)
        else
          (ix, isLive)

    findJust :: [Maybe Int] -> Int
    findJust (Just val:rest) = val
    findJust (Nothing:rest)  = findJust rest

  in
    do
      ring <- ring' game
      empties:: [Maybe Int] <- sequence $ map (
        \posn -> do
          (ix, isLive) <- V.read ring posn
          () <- V.modify ring modifyEntry posn
          pure $ if isLive && ix == ixToRemove then Just posn else Nothing
        ) [0..roundCt game]
      let posn = findJust empties
      pure $ game {
        ring' = pure ring
        , scores' = M.insertWith (+) thisPlayer (val + posn) (scores' game)
        , lastInsertIx' = ixToRemove
        , lastPlayer' = thisPlayer
        , roundCt = val
        , length' = length' game - 1
        }


nrounds' = 25
nplayers' = 9

runGame' :: Game' -> Int -> IO Game'
runGame' game 0 = pure game
runGame' game n = do
  g <- step' game
  ring' g >>= printVec
  runGame' g (n - 1)


printVec :: V.MVector (PrimState IO) (Int, Bool) -> IO ()
printVec ring = do
  ivector <- freeze ring
  let l = zip [0..] (toList ivector)
  let l2 = fst <$> (filter (snd . snd) $ sortOn (fst . snd) l)
  print l2


main :: IO ()
main = do
  let game = newGame nplayers
  let finishedGame = runGame game nrounds
  putStrLn . show $ (maximumByKey snd) . M.toList $ scores finishedGame
  -- game' <- newGame' nrounds' nplayers'
  -- finishedGame <- runGame' game' nrounds'
  -- ring' finishedGame >>= printVec
  -- putStrLn . show $ (maximumByKey snd) . M.toList $ scores' finishedGame
  pure ()
