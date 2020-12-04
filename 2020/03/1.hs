-- stack --resolver lts-16.24 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as Text
import qualified Data.List as List

main =
    putStrLn 
      . show
      . stepThrough 1 3 0
      . lines
        =<< readFile "input.txt"

stepThrough :: Int -> Int -> Int -> [String] -> Int
stepThrough currentRow currentColumn treeCount txts = 
  case List.drop currentRow txts of
    [] -> treeCount
    x:_ -> case drop currentColumn (cycle x) of
              [] -> treeCount
              x:_ | x == '#' -> stepThrough (currentRow + 1) (currentColumn + 3) (treeCount + 1) txts
                  | otherwise -> stepThrough (currentRow + 1) (currentColumn + 3) treeCount txts