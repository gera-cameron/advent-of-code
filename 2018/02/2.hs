-- stack --resolver lts-13.20 script

{-
--- Part Two ---
Confident that your list of box IDs is complete, you're ready to find the boxes full of prototype fabric.

The boxes will have IDs which differ by exactly one character at the same position in both strings. For example, given the following box IDs:

abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz
The IDs abcde and axcye are close, but they differ by two characters (the second and fourth). However, the IDs fghij and fguij differ by exactly one character, the third (h and u). Those must be the correct boxes.

What letters are common between the two correct box IDs? (In the example above, this is found by removing the differing character from either ID, producing fgij.)-}

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set

main = interact (show . findCorrectBox . lines)

findCorrectBox :: [String] -> String
findCorrectBox strs =
  let indexedMaps = map (zip [1 ..]) strs
  in  case searchBoxes indexedMaps $ head indexedMaps of
        Just x  -> x
        Nothing -> findCorrectBox $ tail strs

searchBoxes :: [[(Int, Char)]] -> [(Int, Char)] -> Maybe String
searchBoxes indexedMaps currentIndexedStr = case indexedMaps of
  []     -> Nothing
  x : xs -> case findIntersection x currentIndexedStr of
    intersection_ | length x - 1 == length intersection_ ->
      Just $ fmap snd intersection_
    _ -> searchBoxes xs currentIndexedStr

findIntersection :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
findIntersection l1 l2 = List.intersect l1 l2
