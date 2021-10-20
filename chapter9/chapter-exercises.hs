import Data.Char
import Data.List
import Data.Maybe

-- Data.Char
-- 2.
onlyUpper :: [Char] -> [Char]
onlyUpper = filter isUpper

-- 3.
capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (c:cs) = toUpper c : cs

-- 4.
capitalize :: String -> String
capitalize [] = []
capitalize (c:cs) = toUpper c : capitalize cs

-- 5/6.
extractFirstLetterCapitalized = toUpper . head


-- Ciphers

lettersStream = cycle ['a'..'z']

caesar :: String -> Int -> String
caesar str shift = map (flip encryptLetter shift) str

unCaesar :: String -> Int -> String
unCaesar str shift = caesar str (-shift)

encryptLetter :: Char -> Int -> Char
encryptLetter letter shift = lettersStream !! (fromJust index + shift)
  where
    index = elemIndex letter lettersStream

-- Standard functions

myOr :: [Bool] -> Bool
myOr [] = False
myOr (True:_) = True
myOr (False:rest) = myOr rest

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs)
    | f x = True
    | otherwise = myAny f xs

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem needle (x:xs)
    | needle == x = True
    | otherwise = myElem needle xs

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' needle = any (== needle)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (l:ls) = l ++ squish ls

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

maximumBy' :: (a -> a -> Ordering) -> [a] -> a
maximumBy' f [] = undefined
maximumBy' f (x:xs) = maximumBy'' f xs x

maximumBy'' :: (a -> a -> Ordering) -> [a] -> a -> a
maximumBy'' f [] acc = acc
maximumBy'' f (x:xs) acc = if f x acc == GT then maximumBy'' f xs x else maximumBy'' f xs acc

minimumBy' :: (a -> a -> Ordering) -> [a] -> a
minimumBy' f [] = undefined
minimumBy' f (x:xs) = minimumBy'' f xs x

minimumBy'' :: (a -> a -> Ordering) -> [a] -> a -> a
minimumBy'' f [] acc = acc
minimumBy'' f (x:xs) acc = if f x acc == LT then minimumBy'' f xs x else minimumBy'' f xs acc

maximum' :: Ord a => [a] -> a
maximum' = maximumBy' compare

minimum' :: Ord a => [a] -> a
minimum' = minimumBy' compare
