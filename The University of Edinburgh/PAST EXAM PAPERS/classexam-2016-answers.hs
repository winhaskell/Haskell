-- Informatics 1 - Functional Programming 
-- Class Test 2016
--
-- Sample solutions

-- I would not expect you to include the tests and use of QuickCheck
-- shown below in this exam, except for question 2(c) which explicitly
-- asks for a QuickCheck property, since you have no access to Haskell.
-- But this style of testing is *highly recommended* when you do have
-- Haskell access, for instance for tutorial exercises and the final exam.

import Data.Char
import Test.QuickCheck

-- Problem 1

-- a

vowelly :: Int -> Bool
vowelly 1 = True
vowelly 8 = True
vowelly 11 = True
vowelly 18 = True
vowelly x | x>=80 && x<=89 = True
          | otherwise = False

test1a =
  vowelly 6 == False &&
  vowelly 11 == True &&
  vowelly 15 == False &&
  vowelly 83 == True

-- b

count :: [Int] -> Int
count ns = length [ n | n <- ns, vowelly n]

test1b =
  count [22,11,34,17,52,26,13,40] == 1 &&
  count [] == 0 &&
  count [21,64,32,11,4] == 1 &&
  count [8,83,4,8] == 3

-- c

countRec :: [Int] -> Int
countRec [] = 0
countRec (n:ns) | vowelly n = 1 + countRec ns
                | otherwise = countRec ns 

test1c =
  countRec [22,11,34,17,52,26,13,40] == 1 &&
  countRec [] == 0 &&
  countRec [21,64,32,11,4] == 1 &&
  countRec [8,83,4,8] == 3

prop1 ns = count ns == countRec ns
test1 = quickCheck prop1

-- Problem 2

-- a

c :: Char -> String -> String
c y xs = [ if even i then y else x | (i,x) <- zip [0..] xs ]

test2a =
    c '.' "abcdefg"    ==  ".b.d.f."
 && c '.' "abcd"       ==  ".b.d"
 && c '.' []           ==  []
 && c '.' "a"          ==  "."

-- b

d :: Char -> String -> String
d y []        =  []
d y [x]       =  [y]
d y (_:x:xs)  =  y : x : d y xs

test2b =
    d '.' "abcdefg"    ==  ".b.d.f."
 && d '.' "abcd"       ==  ".b.d"
 && d '.' []           ==  []
 && d '.' "a"          ==  "."

-- c

prop_cd :: Char -> String -> Bool
prop_cd y xs = c y xs == d y xs

test2c = quickCheck prop_cd



