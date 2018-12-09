module Main where

import           Data.List.Index
import qualified Data.Map.Strict as M
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


main :: IO ()
main = do
  let game = newGame nplayers
  let finishedGame = runGame game nrounds
  putStrLn . show $ (maximumByKey snd) . M.toList $ scores finishedGame
