import qualified Data.Maybe as Maybe
import qualified Data.Set as Set

main = do
  contents <- readFile "input.txt"
  print . concat . solve . fmap (parse . words) $ lines contents

parse :: [String] -> (String, String)
parse [_, x, _, _, _, _, _, y, _, _] = (x, y)

solve :: [(String, String)] -> [String]
solve input = case input of
  [(x, y)] -> [x, y]
  _ -> case step input of
    Nothing -> []
    Just string -> string : solve (filter (\(x, y) -> x /= string) input)

step :: [(String, String)] -> Maybe String
step input =
  Maybe.listToMaybe . Set.toAscList . uncurry Set.difference . both Set.fromList $ unzip input

both :: ([String] -> Set.Set String) -> ([String], [String]) -> (Set.Set String, Set.Set String)
both fn (firsts, seconds) = (fn firsts, fn seconds)
