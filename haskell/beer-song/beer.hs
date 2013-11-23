-- This is a silly problem :|

module Beer where

import Data.Char (toLower)
import Data.List (intercalate)

verse :: Integer -> String
verse i = (bottlesString i) ++ " of beer on the wall, " ++
          (map toLower (bottlesString i)) ++ " of beer.\n" ++
          (secondLine i) ++ "\n"

bottlesString :: Integer -> String
bottlesString 0 = "No more bottles"
bottlesString 1 = "1 bottle"
bottlesString i = (show i) ++ " bottles"

secondLine :: Integer -> String
secondLine 0 = "Go to the store and buy some more," ++
               " 99 bottles of beer on the wall."
secondLine i = "Take " ++ (nextBottlePronoun i) ++
               " down and pass it around, " ++
               (map toLower (bottlesString (i - 1))) ++
               " of beer on the wall."

nextBottlePronoun :: Integer -> String
nextBottlePronoun 0 = ""
nextBottlePronoun 1 = "it"
nextBottlePronoun i = "one"

sing :: Integer -> Integer -> String
sing start end = unlines $ map verse [start,start-1..end]
