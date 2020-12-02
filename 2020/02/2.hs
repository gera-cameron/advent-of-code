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
        letter = Text.unpack $ Text.filter (/= ':') letterWithColon
        lowLetter = Text.index password (lowNum - 1) : []
        highLetter = Text.index password (highNum - 1) : []
      in case (lowLetter == letter, highLetter == letter) of
        (True, True) -> False
        (True, False) -> True
        (False, True) -> True
        (False, False) -> False
    _ -> False
  _ -> False