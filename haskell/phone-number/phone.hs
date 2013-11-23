module Phone where

import Data.Char (isDigit)

number :: String -> String
number s
    | 10 == l                         = digits
    | 11 == l && (head digits) == '1' = tail digits
    | otherwise                       = badNumber
    where digits    = filter isDigit s
          badNumber = "0000000000"
          l         = length digits

areaCode :: String -> String
areaCode = take 3 . number

localNumber :: String -> String
localNumber = drop 3 . number

prettyPrint :: String -> String
prettyPrint s = "(" ++ x ++ ") " ++ y ++ "-" ++ z
    where x      = areaCode s
          (y, z) = splitAt 3 $ localNumber s
