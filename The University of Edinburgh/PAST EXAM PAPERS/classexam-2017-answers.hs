-- Informatics 1 - Functional Programming 
-- Class Test 2017
--
-- Sample solutions

-- I would not expect you to include the tests and use of QuickCheck
-- shown below in this exam, except for question 1(c) which explicitly
-- asks for a QuickCheck property, since you have no access to Haskell.
-- But this style of testing is *highly recommended* when you do have
-- Haskell access, for instance for tutorial exercises and the final exam.

import Test.QuickCheck

-- Problem 1

-- a

f :: [Int] -> Int
f ns = sum [ n*n | n <- ns, n `mod` 3 == 0, n `mod` 5 /= 0 ]

test1a =
    f [] == 0
 && f [9,-3] == 90
 && f [0,30,2,7] == 0
 && f [-6,15,2,1,3] == 45

-- b

g :: [Int] -> Int
g [] = 0
g (n:ns) | n `mod` 3 == 0 && n `mod` 5 /= 0 = n*n + g ns
         | otherwise                        = g ns

test1b =
    g [] == 0
 && g [9,-3] == 90
 && g [0,30,2,7] == 0
 && g [-6,15,2,1,3] == 45

-- c

prop_fg :: [Int] -> Bool
prop_fg ns = f ns == g ns

-- Problem 2

-- a

mst :: Int -> Int -> Bool
mst x y | x < 0     = y > x `div` 2
        | otherwise = y > x * 2

test2a =
    mst (-10) (-5) == False
 && mst (-10) (-4) == True
 && mst (-2) 3 == True
 && mst 7 14 == False
 && mst 7 15 == True

-- b

ordered :: [Int] -> Bool
ordered [] = True
ordered (n:ns) = and [ mst x y | (x,y) <- zip (n:ns) ns ]

test2b =
    ordered [] == True
 && ordered [-4,-1,3,1,9] == False
 && ordered [-4,-1,1,3,9] == True
 && ordered [-4,-1,1,2,9] == False

-- c

ordered' :: [Int] -> Bool
ordered' [] = True
ordered' [x] = True
ordered' (x:y:ns) = mst x y && ordered' (y:ns)

test2c =
    ordered' [] == True
 && ordered' [-4,-1,3,1,9] == False
 && ordered' [-4,-1,1,3,9] == True
 && ordered' [-4,-1,1,2,9] == False

prop_ordered :: [Int] -> Bool
prop_ordered ns = ordered ns == ordered' ns
