{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Data.List
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Time
import           Data.Time.Clock
import           Data.Void
import           Debug.Trace
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Error
import           Util

type Parser = Parsec Void String
type Error = ParseErrorBundle String Void

data Event = BeginShift Int
  | FallsAsleep
  | WakesUp
  deriving Show

type Record = (UTCTime, Event)

type Map = M.Map Int [Int]

ingest :: IO [String]
ingest = lines <$> readFile "data/04"


parseLine :: String -> Either Error Record
parseLine line = parse parseRecord "" line


parseDateTime :: Parser UTCTime
parseDateTime = do
  year <- L.decimal
  char '-'
  month <- L.decimal
  char '-'
  day <- L.decimal
  space
  hour <- L.decimal
  char ':'
  minute <- L.decimal
  pure $ UTCTime (fromGregorian year month day) $ timeOfDayToTime $ TimeOfDay hour minute 0


parseGuardBegin :: Parser Event
parseGuardBegin = do
  string "Guard #"
  guardNo <- L.decimal
  string " begins shift"
  pure $ BeginShift guardNo


parseFallsAsleep :: Parser Event
parseFallsAsleep =
  string "falls asleep" >> pure FallsAsleep


parseWakes :: Parser Event
parseWakes =
  string "wakes up" >> pure WakesUp


parseRecord :: Parser Record
parseRecord = do
  char '['
  dateTime <- parseDateTime
  string "] "
  event <- parseGuardBegin <|> parseFallsAsleep <|> parseWakes
  pure (dateTime, event)


organizeRecords :: [Record] -> Map
organizeRecords records =
  let minutes utcTime =
        let TimeOfDay hours mins secs = timeToTimeOfDay ( utctDayTime utcTime)
        in mins

      minuteList :: UTCTime -> UTCTime -> [Int]
      minuteList start end = [minutes start .. minutes end - 1]

      organized = organizeRecords' records []

      diffs :: [(Int, [Int])] = fmap (\(gid, pairs) ->
                                        (gid, foldl (\list pair -> list ++ (uncurry minuteList) pair) [] pairs)
                                     ) organized
  in
    foldl (\map rcd -> insertOrUpdate (fst rcd) (\list -> list ++ snd rcd) [] map) M.empty diffs


organizeRecords' :: [Record] -> [(Int, [(UTCTime, UTCTime)])] -> [(Int, [(UTCTime, UTCTime)])]
organizeRecords' [] rtn = rtn
organizeRecords' ((t1, event1):(t2, event2):rest) shifts =
  case (event1, event2, shifts) of
    -- New shift - create a 'null' list to go with it
    (BeginShift newId, _, _) -> organizeRecords' ((t2, event2):rest) ((newId, []):shifts)
    -- sleep + wake within current shift
    (FallsAsleep, WakesUp, (curGuardId, sleepPairs):prevShifts) ->
          let updatedShift = (curGuardId, (t1, t2):sleepPairs)
          in organizeRecords' rest (updatedShift:prevShifts)
    -- any other match is invalid
    _ -> error "Unexpected pair"
organizeRecords' (one:rest) rtn = error . show $ one


runLength :: Ord a => [a] -> [(a, Int)]
runLength list = fmap (\x -> (head x, length x)) $ (group . sort) list


maxOfRunLengths :: [(a, Int)] -> (a, Int)
maxOfRunLengths list = maximumByKey snd list


main = do
  lines <- ingest
  let parsed :: Either Error [Record] = sequence $ parseLine <$> lines
  case parsed of
    Left err      -> putStrLn (errorBundlePretty err)
    Right records -> do
      let ordRecs = sortOn fst records
          shiftAgg = filter (\(git, mins) -> length mins > 0) $ M.toList $ organizeRecords ordRecs
          bestGuard = maximumByKey (length . snd) shiftAgg
          runLengthOfTimes = runLength $ snd bestGuard
          mostCommonTime = maxOfRunLengths runLengthOfTimes
      putStrLn $ "Best guard * best minute: " ++ (show $ (fst bestGuard * fst mostCommonTime))
      let bestMinuteList :: [(Int, (Int, Int))] = fmap (\(gid, minutesList) ->
                                   (gid, maxOfRunLengths $ runLength minutesList)) shiftAgg
          bestGuardMinute = maximumByKey (snd . snd) bestMinuteList
          res = fst bestGuardMinute * (fst . snd) bestGuardMinute
      putStrLn $ "Best minute for any guard * guard Id: " ++ show res
