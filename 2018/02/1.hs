-- stack --resolver lts-13.20 script

{-
Late at night, you sneak to the warehouse - who knows what kinds of paradoxes you could cause if you were discovered - and use your fancy wrist device to quickly scan every box and produce a list of the likely candidates (your puzzle input).

To make sure you didn't miss any, you scan the likely candidate boxes again, counting the number that have an ID containing exactly two of any letter and then separately counting those with exactly three of any letter. You can multiply those two counts together to get a rudimentary checksum and compare it to what your device predicts.

For example, if you see the following box IDs:

abcdef contains no letters that appear exactly two or three times.
bababc contains two a and three b, so it counts for both.
abbcde contains two b, but no letter appears exactly three times.
abcccd contains three c, but no letter appears exactly two times.
aabcdd contains two a and two d, but it only counts once.
abcdee contains two e.
ababab contains three a and three b, but it only counts once.
Of these box IDs, four of them contain a letter which appears exactly twice, and three of them contain a letter which appears exactly three times. Multiplying these together produces a checksum of 4 * 3 = 12.
-}

import qualified Data.List as List
import qualified Data.Set as Set

main = interact (show . checksum . lines)

checksum :: [String] -> Int
checksum strs = let
    tupes = fmap getTuple strs
  in (sum $ fmap fst tupes) * (sum $ fmap snd tupes)

getTuple :: String -> (Int, Int)
getTuple str = let
  numberSet = Set.fromList $ fmap (\char -> count char str) str
  in (boolToInt $ Set.member 2 numberSet, boolToInt $ Set.member 3 numberSet)

boolToInt :: Bool -> Int
boolToInt True = 1
boolToInt False = 0

count :: Char -> String -> Int
count char str = length $ List.elemIndices char str
