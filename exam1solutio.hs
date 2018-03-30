type Position = (Int, Int)
type Dir = Position -> Position
type Snake = (Dir, [Position], Int)

west, east, north, south :: Dir
west  (x,y) = (x-1,y)
east  (x,y) = (x+1,y)
north (x,y) = (x,y+1)
south (x,y) = (x,y-1)

testDirs :: [Bool]
testDirs = [ south (0,0) == (0,-1)
           , east  (3,3) == (4,3)
           , north (3,2) == (3,3)
           , west  (2,2) == (1,2)
           , east (west (0,0)) == (0,0)
           , north (west (0,0)) == (-1,1)
           ]

		   
isWest, isEast, isNorth, isSouth :: Dir -> Bool
isWest  d = fst (d (0,0)) == (-1) && snd (d (0,0)) == 0
isEast  d = fst (d (0,0)) == 1 && snd (d (0,0)) == 0
isNorth d = fst (d (0,0)) == 0 && snd (d (0,0)) == 1
isSouth d = fst (d (0,0)) == 0 && snd (d (0,0)) == (-1)

testIsDirs :: [Bool]
testIsDirs = [ isWest  west
             , isEast  east
             , isNorth north
             , isSouth south
             , not (isWest south)
             , not (isNorth west)
             , not (isEast west)
             , not (isSouth east)
             , not (isSouth west)
             , not (isSouth north)
             ]
			 
--eqDir :: Dir -> Dir -> Bool
eqDir a b
 |(isEast a)  && (isEast b)   = True
 |(isNorth a) && (isNorth b)  = True
 |(isWest a)  && (isWest b)   = True
 |(isSouth a) && (isSouth b)  = True
 | otherwise                  = False

testEqDir :: [Bool]
testEqDir = [ west  `eqDir` west
            , east  `eqDir` east
            , north `eqDir` north
            , south `eqDir` south
            , not (west  `eqDir` east)
            , not (west  `eqDir` north)
            , not (west  `eqDir` south)
            , not (east  `eqDir` west)
            , not (east  `eqDir` north)
            , not (east  `eqDir` south)
            , not (north `eqDir` west)
            , not (north `eqDir` east)
            , not (north `eqDir` south)
            , not (south `eqDir` east)
            , not (south `eqDir` west)
            , not (south `eqDir` north)
            ]             


turnRight :: Dir -> Dir
turnRight m 
 |(isWest m)  = north 
 |(isNorth m) = east 
 |(isEast m)  = south  
 |(isSouth m) = west

testTurnRight :: [Bool]
testTurnRight = [ turnRight west  `eqDir` north
                , turnRight east  `eqDir` south
                , turnRight north `eqDir` east
                , turnRight south `eqDir` west
                ]
		  


oppositeDir :: Dir -> Dir -> Bool
oppositeDir p q
 |(isWest p) && (isEast q)  = True
 |(isEast p) && (isWest q)  = True
 |(isNorth p) &&(isSouth q) = True
 |(isSouth p) && (isNorth q) = True
 | otherwise = False
 
 
testOppositeDir :: [Bool]
testOppositeDir = [ oppositeDir west east
                  , oppositeDir east west
                  , oppositeDir north south
                  , oppositeDir south north
                  , not (oppositeDir south west)
                  , not (oppositeDir south east)
                  , not (oppositeDir south south)
                  , not (oppositeDir east north)
                  , not (oppositeDir east east)
                  ]

aSnake :: Snake
aSnake = (east, [(0,0)], 4)

snakeBody :: Snake -> [Position]
snakeBody (d, ps, l) = ps

snakeDir :: Snake -> Dir
snakeDir(d, ps, l) = d
fst3(x,y,z)=x
              
snakeLength :: Snake -> Int
snakeLength (d, ps, l) = l


turnTo :: Dir -> Snake -> Snake
turnTo d snakeDirection
       | oppositeDir d (snakeDir snakeDirection) = snakeDirection
       | otherwise                               = (d,snakeBody snakeDirection, snakeLength snakeDirection)
	   
testTurnTo :: [Bool]
testTurnTo = [ -- turnTo does not change the position of the snake
               snakeBody (turnTo west aSnake) == snakeBody aSnake
             , snakeBody (turnTo north aSnake) == snakeBody aSnake

               -- turnTo does not change the length of the snake
             , snakeLength (turnTo north aSnake) == snakeLength aSnake

               -- turning to opposite direction is not allowed, it does not change the direction
             , snakeDir (turnTo west aSnake) `eqDir` snakeDir aSnake

               -- turning to north from east is allowed
             , snakeDir (turnTo north aSnake) `eqDir` north
             ]
			 

step :: Snake -> Snake	 
step px = (snakeDir px,take (snakeLength px) ((fst(snakeBody px!!0)+1,snd(snakeBody px!!0)):snakeBody px), snakeLength px)

testStep :: [Bool]
testStep = [ snakeBody (step aSnake) == [(1,0),(0,0)]
           , snakeBody (step (step aSnake)) == [(2,0),(1,0),(0,0)]
           , snakeBody (step (step (step (step (step aSnake))))) == [(5,0),(4,0),(3,0),(2,0)]
           , snakeBody (step (step (step (step (step (step aSnake)))))) == [(6,0),(5,0),(4,0),(3,0)]
           , snakeBody (step (turnTo north (step aSnake))) == [(1,1),(1,0),(0,0)]
           , snakeDir (step aSnake) `eqDir` snakeDir aSnake
           , length (snakeBody (step (step (step (step (step (step aSnake))))))) == snakeLength aSnake
           , snakeLength aSnake == snakeLength (step (step aSnake)) 
           ]


Ok, modules loaded: Main.

*Main> testOppositeDir
[True,True,True,True,True,True,True,True,True]
*Main> testTurnRight
[True,True,True,True]
*Main> testEqDir
[True,True,True,True,True,True,True,True,True,True,True,True,True,True,True,True]
*Main> testIsDirs
[True,True,True,True,True,True,True,True,True,True]
*Main> testDirs
[True,True,True,True,True,True]
*Main> testTurnTo
[True,True,True,True,True]
*Main> testStep
[True,True,True,True,False,True,True,True]
*Main>

20/24 考试后做得分
13/24 考试得分重考 



