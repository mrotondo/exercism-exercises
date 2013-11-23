module DNA where

-- Thanks to http://stackoverflow.com/questions/3123762 for helping me
-- begin to understand why I need to use the function composition section here.
-- Though I still don't completely intuitively get it.

hammingDistance :: String -> String -> Int
hammingDistance = (length .) . (filter not .) . zipWith (==)
