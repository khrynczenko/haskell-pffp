import Data.Bool
-- 1.
task1 = take 1 $ map (+1) [undefined, 2, 3]
-- bottom


-- 2.
task2 = take 1 $ map (+1) [1, undefined, 3]
-- returns value

-- 3.
task3 = take 2 $ map (+1) [1, undefined, 3]
-- bottom

-- 4.

itIsMystery xs = map (\x -> x `elem` "aeiou") xs

whichLettersAreVowels = itIsMystery

-- 5.
-- a)
x = map (^2) [1..10]
x' = x == [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]

-- b)
y = map minimum [[1..10], [10..20], [20..30]]
y' = y == [1, 10, 20]
-- c)
z = map sum [[1..5], [1..5], [1..5]]
z' = z == [15, 15, 15]

-- 7.
negateThrees = map (\x -> bool x (-x) (x == 3))
