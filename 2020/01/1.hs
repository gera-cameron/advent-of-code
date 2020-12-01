-- stack --resolver lts-16.24 script

import Data.List

main =
    putStrLn 
        . show 
        . product
        . fmap read
        . head
        . filter (\l -> length l == 2 && sum (fmap read l) == 2020) 
        . findPairs
        . lines 
        =<< readFile "input.txt"

findPairs :: [String] -> [[String]]
findPairs [] = []
findPairs (x:xs) = fmap (\a -> [x, a]) xs ++ findPairs xs