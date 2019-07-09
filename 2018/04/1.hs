import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Time as Time

main = do
  contents <- readFile "input.txt"
  print
    . List.sortOn fst
    . Maybe.catMaybes
    . fmap buildTimeToEventTuple
    . fmap words
    . fmap (fmap (\char -> if char == '[' || char == ']' then ' ' else char))
    $ lines contents

buildTimeToEventTuple :: [String] -> Maybe (Time.UTCTime, Action)
buildTimeToEventTuple list = case list of
  ymd : time : rest -> do
    utcTime <- Time.parseTimeM True Time.defaultTimeLocale "%0Y-%-m-%-d %H:%M" (unwords [ymd, time])
    Just (utcTime, parseAction utcTime $ unwords rest)
  _ -> Nothing

parseAction time action = case action of
  "falls asleep" -> GuardSleeps $ toMinutes time
  "wakes up" -> GuardAwakes $ toMinutes time
  _ -> GuardOnDuty . read $ filter Char.isNumber action

data Action = GuardOnDuty Int | GuardSleeps Time.DiffTime | GuardAwakes Time.DiffTime
  deriving (Eq, Show)

toMinutes = (/ 60) . Time.utctDayTime
