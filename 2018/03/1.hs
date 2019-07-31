import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.Time as Time

main = do
  contents <- readFile "input.txt"
  print
    . _
    . fmap (buildClaimTuples . fmap read)
    . fmap words
    . fmap (fmap replaceNonNumericWithSpace)
    $ lines contents

replaceNonNumericWithSpace :: Char -> Char
replaceNonNumericWithSpace charUnderTest = if charUnderTest >= '0' && charUnderTest <= '9' then charUnderTest else ' '

buildClaimTuples :: [Int] -> ((Int, Int), (Int, Int))
buildClaimTuples [_, x, y, h, w] = ((x, y), (h, w))
