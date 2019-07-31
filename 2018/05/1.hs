import qualified Data.Char as Char
main = do
  contents <- readFile "input.txt"
  print . length . foldr cleanOutReactions "" $ filter Char.isAlpha contents

cleanOutReactions :: Char -> String -> String
cleanOutReactions currentChar polymer = case polymer of
  "" -> [currentChar]
  x1 : xs
    | Char.toLower currentChar == Char.toLower x1 && currentChar /= x1
    -> xs
    | otherwise
    -> currentChar : polymer
