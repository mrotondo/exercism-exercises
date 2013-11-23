module Anagram where

import Data.List (sort)
import Data.Char (toLower)
import Data.Traversable (sequenceA)

anagramsFor :: String -> [String] -> [String]
anagramsFor w = filter (and . sequenceA predicates)
    where canonical = sort . map toLower
	  predicates = [(canonical w ==) . canonical, not . (w ==)]
