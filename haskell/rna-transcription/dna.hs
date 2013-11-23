module DNA where

import Data.String.Utils (replace) -- from MissingH package

toRNA :: String -> String
toRNA = replace "T" "U"
