{-# LANGUAGE TupleSections #-}

module DNA where

import Data.Map (Map)
import qualified Data.Map as M

count :: Char -> [Char] -> Int
count c cs
      | c `elem` ['A', 'C', 'G', 'T', 'U'] = length $ filter (c==) cs
      | otherwise = error $ "invalid nucleotide " ++ (show c)

nucleotideCounts :: [Char] -> Map Char Int
nucleotideCounts = M.unionWith (+) emptyMap . M.fromListWith (+) . map (,1)
    where emptyMap = M.fromList $ map (,0) ['A', 'C', 'G', 'T']
