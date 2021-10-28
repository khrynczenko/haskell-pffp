-- Exercises: Mood Swing
data Mood = Blah | Woot deriving Show

-- 1. Mood is the type constructor and the type name
-- 2. We could use either Blah or Woot
-- 3. `Woot` is a value. Value cannot appear in type signature. It would not
--    make any sense.
--
-- 4.
changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- Exercises: Fint the mistakes
--
-- 1. not True && true
-- Answer: true is not a value of type Bool, the compiler will look for a value
-- under that name but it is not defined
--
-- 2. not (x = 6)
-- Answer: `=` is not an operator, `==` should be used here
--
-- 3. (1 * 2) > 4
-- Anwser: not mistakes here, result will be `False`
--
-- 4. [Merry] > [Happy]
-- Answer: Missing quotation marks
--
-- 5. [1, 2, 3] ++ "look at me!"
-- Answer: type of (++) is (++) :: [a] -> [a] -> [a] which means that both
-- arguments must be of the same type. Here we have `Int` and `Char` so this
-- does not typecheck.
--
-- Chapter exercises
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- 1. `length` would be of type [a] -> Int
--    Actually it is Foldable t => t a -> Int
-- 2. a) length [1, 2, 3, 4, 5] == 5
--    b) length [(1, 2), (2, 3), (3, 4)] == 3
--    c) length allAwesome == 2
--    d) length (concat allAwesome) == 5
-- 3. 6 / length [1, 2, 3] will not work because length returns Int and (/)
--    requries instance of Rational. Int is not instance of Rational.
-- 4. We could perform integral division using div
--    6 `div` length [1, 2, 3] 
--    6 will infer to Int also and `div` will work
-- 5. Result will be the value True of type Bool
-- 6. a) it is a declaration, which is a statement, which does not produce a
--       value
--    b) will evaluate to false
-- 7. a) works -> True
--    b) does not work -> list type must be homogenous
--    c) works -> Int
--    d) works -> Bool
--    e) does not work (&&) requires two bools, one int given

-- 8.
isPalindrome :: String -> Bool
isPalindrome text = text == reverse text

-- 9.
myAbs :: (Num a, Ord a) => a -> a
myAbs x = if x < 0 then x * (-1) else x

-- 10.
f :: (a, b) -> (c, d) -> ((b, d), (a ,c))
f (a ,b) (c, d) = ((b, d), (a, c))
