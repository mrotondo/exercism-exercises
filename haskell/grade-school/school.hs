module School (School, sorted, grade, add, empty) where

import Data.List (sortBy, find)
import Data.Ord (comparing)
import Data.Maybe (fromJust, isNothing)
import qualified Data.Map as M
import Data.Map (Map)
import Control.Applicative ((<$>), (<*>))

type Student = String
newtype School = School { getSchool :: Map Int [Student] }

sorted :: School -> [(Int, [Student])]
sorted = sortBy (comparing fst) . map sortedStudents . M.toList . getSchool

grade :: Int -> School -> [Student]
grade gradeNum (School school) = M.findWithDefault [] gradeNum school

add :: Int -> Student -> School -> School
add gradeNum student (School school)
    | M.null school = School $ M.singleton gradeNum [student]
    | otherwise     = School $ M.alter (addToStudents student) gradeNum school

addToStudents :: Student -> Maybe [Student] -> Maybe [Student]
addToStudents student Nothing         = Just [student]
addToStudents student (Just students) = Just $ student:students

empty :: School
empty = School M.empty

sortedStudents :: (Int, [Student]) -> (Int, [Student])
sortedStudents = fmap $ sortBy (comparing head)
