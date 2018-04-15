module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, nub)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return (lines dict)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 10

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return (filter gameLength aw)
  where gameLength w =
          let l = length (w :: String)
          in  l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word discovered guessed
              where discovered = map (const Nothing) word
                    guessed = []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word discovered guessed) c =
  Puzzle word newDiscovered (c : guessed)
  where zipper guess wordChar discoveredChar =
          if wordChar == guess
          then Just wordChar
          else discoveredChar
        newDiscovered =
          zipWith (zipper c) word discovered

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Great guess! That's in, filling it accordingly."
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "Too bad! This isn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle word discovered guessed) =
  if (length guessed - (length $ nub $ filter isJust discovered) > 7) then
    do putStrLn "You lost!"
       putStrLn $ "The answer was: " ++ word
       exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle word discovered _) =
  if all isJust discovered then
    do putStrLn "You win!"
       putStrLn $ "The answer was: " ++ word
       exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  hSetBuffering stdout NoBuffering
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _   -> putStrLn "Your guess must be a single character."

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
  
  -- https://gist.github.com/rajadain/8be169a2e9e34e2a8d3c5f2a6f0fc5dd
  -- https://gist.github.com/raheelahmad/885a8fde15e36b51e95e602fc36d4f49

--------------------------------------------------------------------------------------
import Data.Char (toUpper, toLower)
import Data.List (sort)

type ABC = [Char]

abc :: ABC
abc = ['A'..'Z']

type Riddle       = String
type RightGuesses = [Char]
type WrongGuesses = [Char]
type State        = (Riddle, RightGuesses, WrongGuesses)

isValidLetter :: Char -> ABC -> Bool
isValidLetter c l = elem (toUpper c) l || elem (toLower c) l

test_isValidLetter =
  [ isValidLetter 'a' abc        == True
  , isValidLetter 'X' abc        == True
  , isValidLetter ' ' abc        == False
  , isValidLetter '$' "*]-$><"   == True
  , isValidLetter 'E' ['a'..'z'] == True
  ]

startState :: ABC -> String -> State
startState abc str = if null [c | c <- str, not (c == ' ' || isValidLetter c abc)]
                     then ([toUpper c | c <- str], [], [])
                     else undefined

test_startState =
  [ startState abc ""                == ("","","")
  , startState abc "SOS"             == ("SOS","","")
  , startState abc "Save Our Souls"  == ("SAVE OUR SOULS","","")
  ]

guessLetter :: ABC -> Char -> State -> State
guessLetter abc c (r, rg, wg)
  | c' `elem` rg || c' `elem` wg = (r, rg, wg)
  | c' `elem` r                  = (r, c':rg, wg)
  | isValidLetter c abc          = (r, rg, c':wg)
  | otherwise                    = undefined
  where
    c' = toUpper c

test_guessLetter =
  [ guessLetter abc 'a' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'A' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'k' (startState abc "Save Our Souls") == ("SAVE OUR SOULS","","K")
  , guessLetter abc 'a' (guessLetter abc 'a' (startState abc "Save Our Souls")) == ("SAVE OUR SOULS","A","")
  , guessLetter abc 'K' (guessLetter abc 'k' (startState abc "Save Our Souls")) == ("SAVE OUR SOULS","","K")
  , guessLetter abc 'v' ("SAVE OUR SOULS", "A", [])       == ("SAVE OUR SOULS","VA","")
  , guessLetter abc 'k' ("SAVE OUR SOULS", "VA", [])      == ("SAVE OUR SOULS","VA","K")
  ]

showRiddle :: State -> String
showRiddle (r, rg, _) = [ if c `elem` rg || c == ' ' then c else '_' | c <- r ]

test_showRiddle =
  [ showRiddle ("SAVE OUR SOULS", [], [])     == "____ ___ _____"
  , showRiddle ("SAVE OUR SOULS", "AL", [])   == "_A__ ___ ___L_"
  , showRiddle ("SAVE OUR SOULS", "SAL", [])  == "SA__ ___ S__LS"
  , showRiddle ("SAVE OUR SOULS", "SALO", []) == "SA__ O__ SO_LS"
  , showRiddle ("SOS", "SO", "AL")            == "SOS"
  ]

showState :: State -> State
showState (r, rg, wg) = (showRiddle (r, rg, wg), rg, wg)

test_showState =
  [ showState ("SAVE OUR SOULS", [], [])     == ("____ ___ _____", "", "")
  , showState ("SAVE OUR SOULS", "SAL", [])  == ("SA__ ___ S__LS", "SAL", "")
  , showState ("SAVE OUR SOULS", "SALO", []) == ("SA__ O__ SO_LS", "SALO", "")
  ]

isRiddleComplete :: State -> Bool
isRiddleComplete s = null [ 1 | '_' <- showRiddle s ]

test_isRiddleComplete =
  [ isRiddleComplete ("SOS", [], [])                        == False
  , isRiddleComplete ("SOS", "SALO", [])                    == True
  , isRiddleComplete ("SOS", "ALO", [])                     == False
  , isRiddleComplete ("SOS", [], "LKHJIG")                  == False
  , isRiddleComplete ("SAVE OUR SOULS", "SAVEOURL", "KZTW") == True
  ]

isGameOver :: State -> Bool
isGameOver (r, rg, wg) = length wg > 5 || isRiddleComplete (r, rg, wg)

test_isGameOver =
  [ isGameOver ("SOS", [], [])                          == False
  , isGameOver ("SOS", [], "LKHJIG")                    == True
  , isGameOver ("SAVE OUR SOULS", "SAVEOURL", "KZTW")   == True
  , isGameOver ("SAVE OUR SOULS", "SAVEOURL", "KZTWM")  == True
  , isGameOver ("SAVE OUR SOULS", "SAVEOURL", "KZTWMB") == True
  ]

getAvailableLetters :: ABC -> State -> [Char]
getAvailableLetters abc (_, rg, wg) = [l | l <- abc, not (l `elem` (rg ++ wg))]

test_getAvailableLetters =
  [ sort (getAvailableLetters abc ("SAVE OUR SOULS", "SAVEOURL", "KZTW")) == "BCDFGHIJMNPQXY"
  , sort (getAvailableLetters abc ("SOS", [], "LKHJIG"))                  == "ABCDEFMNOPQRSTUVWXYZ"
  , sort (getAvailableLetters abc ("SOS", [], []))                        == "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  ]

allTests = (and (concatMap snd tests), tests)

tests =
  [ ("test_isValidLetter", test_isValidLetter)
  , ("test_startState", test_startState)
  , ("test_guessLetter", test_guessLetter)
  , ("test_showRiddle", test_showRiddle)
  , ("test_showState", test_showState)
  , ("test_isRiddleComplete", test_isRiddleComplete)
  , ("test_isGameOver", test_isGameOver)
  , ("test_getAvailableLetters", test_getAvailableLetters)
  ]
