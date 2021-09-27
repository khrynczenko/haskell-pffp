module Exercises where

-- Building function


-- 1 (and 2)
-- a)
appendExclamationMark :: String -> String
appendExclamationMark str = str ++ "!"

-- b)
getFifthLetter :: String -> Char
getFifthLetter str = str !! 5

-- c)
dropEightLetters :: String -> String
dropEightLetters = drop 8


-- 3
thirdLetter :: String -> Char
thirdLetter str = str !! 3

-- 4
letterIndex :: Int -> Char
letterIndex n = "Curry is awesome" !! n

-- 5
rvrs :: String -> String
rvrs [] = []
rvrs (x:xs) = rvrs xs ++ [x]
