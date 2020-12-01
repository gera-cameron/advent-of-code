-- stack --resolver lts-16.24 script

import Combinatorics
import Data.List

main = 
    putStrLn 
        . show 
        . product
        . fmap read
        . head
        . filter (\l -> length l == 3 && sum (fmap read l) == 2020) 
        . variate 3
        . lines 
        =<< readFile "input.txt"
