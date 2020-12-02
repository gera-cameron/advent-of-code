-- stack --resolver lts-16.24 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as Text

main =
    putStrLn 
      . show
      . length
      . filter (== True)
      . fmap (isValid . Text.splitOn " ")
      . Text.lines
      . Text.pack
        =<< readFile "input.txt"

isValid :: [Text.Text] -> Bool
isValid txts = case txts of
  [numbs, letterWithColon, password] -> case Text.splitOn "-" numbs of
    [low, high] -> 
      let
        lowNum = read (Text.unpack low)
        highNum = read (Text.unpack high)
        letter = Text.filter (/= ':') letterWithColon
        c = Text.count letter password
      in c >= lowNum && c <= highNum
    _ -> False
  _ -> False