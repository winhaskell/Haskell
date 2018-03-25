-- Informatics 1 - Functional Programming 
-- Class Exam
--
-- Solutions

import Data.Char
import Test.QuickCheck

-- Problem 1.

--a
f :: Char -> Int
f x = firstScore * upperScore
  where
  y          = toLower x
  firstScore = if 'a' <= y && y <= 'm' then 1 else 2
  upperScore = if isLower x then 1 else 3

test1a = f 'a' == 1 && f 'A' == 3 && f 'z' == 2 && f 'Z' == 6

--b
g :: String -> Int
g xs = sum [ f x | x <- xs, isAlpha x ]

test1b = g "aAzZ" == 12 && g "a2m&n2z" == 6

--c
h :: String -> Int
h [] = 0
h (x:xs) | isAlpha x = f x + h xs
         | otherwise = h xs

test1c = h "aAzZ" == 12 && h "a2m&n2z" == 6
prop_1 xs = g xs == h xs

-- Problem 2.

--a
c :: [Int] -> Bool
c xs = and [ x > y | (x,y) <- zip xs (tail xs) ]

test2a = c [4,3,2,1] && c [8,4,2,1,0] && c [2] &&
         not (c [4,2,3,1] && c [0,1,2] && c [2,2,2])

--b
d :: [Int] -> Bool
d [x] = True
d (x:y:zs) = x > y && d (y:zs)

test2b = c [4,3,2,1] && c [8,4,2,1,0] && c [2] &&
         not (c [4,2,3,1] && c [0,1,2] && c [2,2,2])

--c
prop_cd :: [Int] -> Property
prop_cd xs = not (null xs) ==> c xs == d xs
