-- Chapter exrcises
--
-- 1. The Eq class
-- c) makes equality tests possible
-- 2. The typ class Ord
-- a) allows any two vales to be compared
-- b) is a subclass of Eq
-- 3. Suppose the type of class Ord has an operator >. What is the type of >?
-- a) Ord a => a -> a -> Bool
-- 4. In x = divmod 16 12
-- c) the type of x is a tuple
-- 5. Tyhe type class of Integral includes
-- a) Int and Integer numbers

-- Does this type check?
-- 1.
--data Person = Person Bool

--printPerson :: Person -> IO ()
--printPerson person = putStrLn (show person)
--
--This does not compile because there is no Show instance for the Person type.

-- 2.
--
--data Mood = Blah | Woot deriving Show

--settleDown x = if x == Woot
               --then Blah
               --else x
----
--This does not compile because there is no Eq instance for the Mood type.
--
--
--3. a) two values are acceptible Blah and Woot.
--   b) It will cause error because occurrence of Woot in the comparison
--      make compiler infer that x has to be also of type Mood since
--      (==) has Eq a => a -> a -> Bool type.
--   c) It will not work since Mood does not implement Ord.
--
--4.
--type Subject = String
--type Verb = String
--type Object = String

--data Sentence = Sentence Subject Verb Object deriving (Show, Eq)

--s1 = Sentence "dogs" "drool"
--s2 = Sentence "Julie" "loves" "dogs"
--
--This type checks but s1 is a function that will accept one argument to create
--value of type Sentence. s2 is already such value.
--
--
-- Given a datatype declaration, what can we do?
--data Rocks = Rocks String deriving (Eq, Show)
--data Yeah = Yeah Bool deriving (Eq, Show)
--data Papu = Papu Rocks Yeah deriving (Eq, Show)
--
--1. (does not typecheck)
--phew = Papu "chases" True
--phew = Papu (Rocks "chases") (Yeah True) -- correct
--
--2. (typechecks)
--truth = Papu (Rocks "chomskydoz") (Yeah True)
--
--3. (typechecks)
--
--4. (does not typecheck, needs instance of Ord)
--comparePapus :: Papu -> Papu -> Bool
--comparePapus p p' = p > p'
--
-- Match the types

--1.
--i :: Num a => a
--i :: a
--i = 1

--2.
--f :: Float
--f :: Num a => a
--f = 1.0

--3.
--f :: Float
--f :: Fractional a => a
--f = 1.0

--4.
--f :: Float
--f :: RealFrac a => a
--f = 1.0

--5.
--freud :: a -> a
--freud :: Ord a => a -> a
--freud x = x

--6.
--freud' :: a -> a
--freud' :: Int -> Int
--freud' x = x

--7.
--myX = 1 :: Int
--sigmund :: Int -> Int
--sigmund :: a -> a
--sigmund x = myX

