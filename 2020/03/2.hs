-- stack --resolver lts-16.24 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as Text
import qualified Data.List as List

main =
    putStrLn 
      . show
      . product
      . (\txts -> fmap (\(rs, cs) -> stepThrough rs cs rs cs 0 txts) [(1,1), (1, 3), (1, 5), (1, 7), (2, 1)])
      . lines
        =<< readFile "input.txt"

stepThrough :: Int -> Int ->  Int -> Int -> Int -> [String] -> Int
stepThrough rowSkip colSkip currentRow currentColumn treeCount txts = 
  case List.drop currentRow txts of
    [] -> treeCount
    x:_ -> case drop currentColumn (cycle x) of
              [] -> treeCount
              x:_ | x == '#' -> stepThrough rowSkip colSkip (currentRow + rowSkip) (currentColumn + colSkip) (treeCount + 1) txts
                  | otherwise -> stepThrough rowSkip colSkip (currentRow + rowSkip) (currentColumn + colSkip) treeCount txts