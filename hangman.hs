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
