import Data.Char
import qualified Data.Map as Map
main = do
  contents <- readFile "sample.txt"
  print
    . Map.fromList
    . flip zip [(1 :: Int) .. ]
    . parseInput
    $ contents

parseInput :: String -> [(Int, Int)]
parseInput =
  fmap ((\ [x, y] -> (x, y)) . fmap read . words)
  . lines
  . filter (/= ',')

foo :: Map.Map (Int, Int) Int -> Maybe Int
foo _ = Nothing
