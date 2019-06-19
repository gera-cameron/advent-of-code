-- stack --resolver lts-13.20 script

import qualified Data.Maybe as Maybe
import qualified Text.Read as Read

main = putStrLn . show . sum . parseNumbers =<< getContents

parseNumbers :: String -> [Int]
parseNumbers = Maybe.mapMaybe (Read.readMaybe . removePlus) . lines

removePlus :: String -> String
removePlus str = case splitAt 1 str of
  ("+", restOfStr) -> restOfStr
  _ -> str
