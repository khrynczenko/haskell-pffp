-- Exercises: Type matching
--
-- 1. a) not :: Bool -> Bool
--    b) length :: [a] -> Int
--    c) concat :: [[a]] -> [a]
--    d) head :: [a] -> a
--    e) (<) :: Ord a => a -> a -> Bool
--
--
-- Currying/Uncurrying
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurriedAdd :: Num a => (a, a) -> a
uncurriedAdd (x, y) = x + y

curriedAdd = curry uncurriedAdd

uncurriedAdd3 :: Num a => (a, a, a) -> a
uncurriedAdd3 (x, y, z) = x + y + z

curry3 :: ((a, b, c) -> d) -> (a -> (b -> (c -> d)))
curry3 f x y z = f (x, y, z)

-- Exercises: Type arguments
-- 1. f :: a -> a -> a -> a
--    f "c" :: Char -> Char -> Char
-- 2. g :: a -> b -> c -> b
--    g 0 'c' "woot" :: Char
-- 3. h :: (Num a, Num b) => a -> b -> b
--    h 1.0 2 :: Num b => b
-- 4. h :: (Num a, Num b) => a -> b -> b
--    h 1 (5.0 :: Double) :: Double
-- 5. jackal :: (Ord a, Eq b) => a -> b - > a
--    jackal "keyboard" "has the work jackal in it" :: [Char]
-- 6. jackal :: (Ord a, Eq b) => a -> b - > a
--    jackal "keyboard" :: Eq b => b -> String
-- 7. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel 1 2 :: (Ord a, Num a) => a
-- 8. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel 1 (2 :: Integer) :: (Ord a, Num a) => a
-- 9. kessel :: (Ord a, Num b) => a -> b -> a
--    kessel (1 :: Integer) 2 :: Integer
--
-- Exercises: Parametricity
-- 1. 
--   id' :: a -> a
--   id' x = x + x -- will not work because `a` can be any type and we cannot be
--                    sure it will be an instance of `Num`
-- 2. 
parametricf1 :: a -> a -> a
parametricf1 x y = x

parametricf2 :: a -> a -> a
parametricf2 x y = y
-- 3.
parametricg :: a -> b -> b
parametricg x y = y
-- ^ there is only one wayt of implementing this function dues to resriction
--   that the result type is bound to the type of second parameter
--
-- Exercises: Apply yourself
-- 1. (++) :: [a] -> [a] -> [a]
--    myConcat x = x ++ "yo"
--    myConcat :: [Char] -> [Char]
-- 2. (*) :: Num a => a -> a -> a
--    myMult x = (x / 3) * 5
--    myMult :: Fractional a => a -> a
-- 3. take :: Int -> [a] -> [a]
--    myTake x = take x "hey you"
--    myTake :: Int -> [Char]
-- 4. (>) :: Ord a => a -> a -> Bool
--    myCom x = x > (length [1..10])
--    myCom :: Int -> Bool
-- 5. (<) :: Ord a => a -> a -> Bool
--    myCom x = x < 'z'
--    myCom :: Char -> Bool
--
-- Chapter exercises
-- Multiple choice
-- 1. A value of type [a] is:
--    b) a list of lists
--    c) a list of elements that are all of some type a
-- 2. A function of type [[a]] -> [a] could:
--    a) take a list of strings as an argument
-- 3. A function of type [a] -> Int -> a:
--    a) takes one argument
--    b) returns one element of type a from a list
-- 4. A function of type (a, b) -> a:
--    c) takes a tuple argument and returnst the first value
--
-- Does it compile?
--bigNum = (^) 5 $ 10
--wahoo = bigNum $ 10 -- Does not compile, `$` expects a function on its left side
--
--x = print
--y = print "wohoo"
--z = x "hello world" -- compiles
-- ... (these tasks are to easy)
--
-- Write type signature

functionH :: [a] -> a -- head
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc f g c (a, x) = (a, (g . f) x)
