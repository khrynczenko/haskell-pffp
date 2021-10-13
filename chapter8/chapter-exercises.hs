import Data.List (intersperse)
-- Chapter exercises
--
-- Review of types
-- 1. What is the type of [[True, False], [True, True], [False, True]]?
--    d) [[Bool]]
-- 2. Which of the following has the same types as
--    [[True, False], [True, True], [False, True]]?
--    b) [[3 == 3], [6 > 5], [3 < 4]]
-- 3. For the function below which of the following statements are true?
--    func :: [a] -> [a] -> [a]
--    func x y = x ++ y
--    a) x and y must be of the sam type
--    b) x and y must both be lists
--    c) if x is String the y must also be String
--    d) all the above :)
--
-- 4. For the func code aboce, which is valid application of func to both
--    its arguments.
--    a) func "Hello world"
--    b) func "Hello" "World" <-
--    c) func [1, 2, 3] "a, b, c"
--    d) func ["Hello", "World"]

-- Review currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ "meow" ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frape = flippy "haha"

-- 1. What is the value of appedCatty "wohoo!".
answer1 = appedCatty "wohoo!" == "woopsmeowwohoo!"

-- 2. Write function that recursively sums all numbers from 1 to n, n
--    being the argument. So if n is 5, you add 1 + 2 + 3 + 4+ 5 to get 15.
--    The type should be (Eq a, Num a) -> a -> a
sum' :: (Eq a, Num a) => a -> a -- Why do we need Eq constraint on a? Pattern matching uses it to check if n is 0
sum' 0 = 0
sum' n = n + sum' (n - 1)


-- Tail call optimized version
sumTCO :: (Eq a, Num a) => a -> a
sumTCO n = go n 0
    where
        go 0 acc = acc
        go n acc = go (n - 1) (acc + n)

-- 3. Write a function that mutiplies two integral number using recursive
--    summation. The type should be (Integral a) => a -> a -> a
mul :: Integral a => a -> a -> a
mul 0 y = 0
mul x 0 = 0
mul 1 y = y
mul x 1 = x
mul x y = y + mul (x - 1) y


-- Fixing dividedBy
type Numerator = Integer
type Denumerator = Integer
type Quotient = Integer
type Remainder = Integer

data DividedResult =
    Result (Quotient, Remainder)
  | DividedByZero
  deriving (Show)

dividedBy :: Numerator -> Denumerator -> DividedResult
dividedBy num 0 = DividedByZero
dividedBy num denum = Result (go num denum 0 0)
  where
    go n d q r
        | d > n = (q, n)
        | otherwise = go (n - d) d (q + 1) 0

-- McCarthy 91 function
mc91 :: (Num a, Ord a) => a -> a
mc91 n
    | n > 100 = n - 10
    | otherwise = (mc91 . mc91) (n + 11)

-- Numbers into words
digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = undefined

digits :: Int -> [Int]
digits n = reverse $ go n
    where
    go n 
        | n < 10 = [n]
        | otherwise = n `mod` 10 : go (n `div` 10)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . fmap digitToWord . digits
