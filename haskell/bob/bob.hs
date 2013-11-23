module Bob where

import Data.Char (isAsciiLower, isAsciiUpper, isSpace)
import Data.List (isSuffixOf)
import Control.Arrow ((&&&))
import Control.Applicative (Applicative, (<$>), (<*>), liftA2, pure)

responseFor :: String -> String
responseFor x
    | isWhitespace x = "Fine. Be that way!"
    | isQuestion x && not (allCaps x) = "Sure."
    | allCaps x = "Woah, chill out!"
    | otherwise = "Whatever."

allCaps :: String -> Bool
allCaps = and . sequenceA [noLowerCase, atLeastOneUpperCase]
--allCaps = (&&) <$> noLowerCase <*> atLeastOneUpperCase
--allcaps = uncurry (&&) . (noLowerCase &&& atLeastOneUpperCase)
--allCaps s = noLowerCase s && atLeastOneUpperCase s

-- From http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

noLowerCase :: String -> Bool
noLowerCase = not . any isAsciiLower

atLeastOneUpperCase :: String -> Bool
atLeastOneUpperCase = any isAsciiUpper

isQuestion :: String -> Bool
isQuestion = isSuffixOf "?"

isWhitespace :: String -> Bool
isWhitespace = all isSpace
