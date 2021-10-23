-- 1.
-- I don't understand the task xd


-- 2. What does the following function do, what is its type?
seekritFunc x =
    div (sum (map length (words x))) (length (words x))
-- It finds the average word length in a text
-- Its type is (Integral a) => String -> a

-- 3.
--
avgWordLength x =
    fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))


-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldl (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldl (\acc b -> acc || f b) False

myElem :: (Eq a) => a -> [a] -> Bool
myElem x = foldr ((||) . (== x)) False

myElem' :: (Eq a) => a -> [a] -> Bool
myElem' x = any (== x)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr ((++) . (\x -> if f x then [x] else [])) []

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id
