import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Time as Time

main = do
  contents <- readFile "input.txt"
  print
    . findSleepiestGuardsFrequencies
    . last
    . List.sortOn (length . snd)
    . Map.toList
    . fmap (calculateTimeAsleep [])
    . buildGuardToActionMap [] Nothing
    . fmap snd
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

data Action = GuardOnDuty Int | GuardSleeps Int | GuardAwakes Int
  deriving (Eq, Show)

toMinutes = fromInteger . (flip div 60) . (flip div 1000000000000) .  Time.diffTimeToPicoseconds . Time.utctDayTime

buildGuardToActionMap :: [(Int, [Action])] -> Maybe Int -> [Action] -> Map.Map Int [Action]
buildGuardToActionMap accumulationList maybeGuardNumber actions = case (actions, maybeGuardNumber) of
  ([], _) -> Map.fromListWith (<>) accumulationList
  (x : xs, Just guardNumber) -> case x of
    GuardOnDuty currentGuardNumber ->
      buildGuardToActionMap accumulationList (Just currentGuardNumber) xs
    _ -> buildGuardToActionMap ((guardNumber, [x]) : accumulationList) maybeGuardNumber xs
  (x : xs, Nothing) -> case x of
    GuardOnDuty currentGuardNumber ->
      buildGuardToActionMap accumulationList (Just currentGuardNumber) xs
    _ -> buildGuardToActionMap accumulationList maybeGuardNumber xs

calculateTimeAsleep :: [Int] -> [Action] -> [Int]
calculateTimeAsleep accumulationList actions = case actions of
  [] -> accumulationList
  GuardSleeps diff1 : GuardAwakes diff2 : rest -> calculateTimeAsleep ([diff1 .. diff2 - 1] <> accumulationList) rest

findSleepiestGuardsFrequencies :: (Int, [Int]) -> Int
findSleepiestGuardsFrequencies (g, minutes) =
  (\((g, minute), _) -> g * minute)
  . last
  . List.sortOn snd
  . Map.toList
  . Map.fromListWith (+)
  $ fmap (\minute -> ((g, minute), 1)) minutes
