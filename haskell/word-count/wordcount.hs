{-# LANGUAGE TupleSections #-}

module WordCount where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (isAlphaNum, toLower)
import Data.List.Split (wordsBy)

wordCount :: String -> Map String Int
wordCount = Map.fromListWith (+) . map (,1) . (wordsBy (not . isAlphaNum)) . map toLower