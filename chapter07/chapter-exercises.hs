-- Chapter exercises
--
-- Multiple choice
-- 1. A polymorphic function:
--    a) May resolve to values of different types depending on inputs
-- 2. Two functions named f and g have type Char -> string and String ->
--    [String], respecrively. The compose function g . f has type:
--    b) Char -> [String]
-- 3. A function f has the type of Ord a => a -> a -> Bool, and we apply it to
--    one numeric value. What is the type now.
--    d) (Ord a, Num a) => a -> Bool
-- 4. A function with type (a -> b) -> c:
--    b) Is a higher-order function
--
-- Let's write code.
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
    where (d, _) = x `divMod` 10
        
-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y chooseX =
    case chooseX of
        True -> x
        False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y chooseX
    | chooseX = x
    | not chooseX = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x, y)

-- 4, 5, 6.

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show

roundTrip' :: (Show a, Read b) => a -> b
roundTrip' a = read (show a)
-- roundTrip' 4 :: Int

