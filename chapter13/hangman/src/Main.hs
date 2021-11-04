module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse, find)
import System.Exit (exitSuccess)
import System.IO ( BufferMode(NoBuffering)
                 , hSetBuffering
                 , stdout)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]

instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $
         fmap renderPuzzleChar discovered)
         ++ " Guessed so far: " ++ guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar Nothing = '_'

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str [] []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle str _ _) c = c `elem` str


alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c =  c `elem` guessed


allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return $ concatMap words (lines dict)

minWordLength = 5
maxWordLength = 9

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
  where
    gameLength w = 
        let l = length (w :: String)
        in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, length wl)
    return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInfSoFar s) c =
    Puzzle word newFilledInSoFar (c:s)
  where
    zipper guessed wordChar guessChar =
        if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInfSoFar

main :: IO ()
main = do
  randomWord' >>= putStrLn
