-- Exercises: Guard duty
--
-- 3.
pal xs
    | xs == reverse xs = True
    | otherwise = False
-- True when xs is a palindrome

-- 4.
-- It can take any kind of list for which elements have instance of Eq
--
-- 5.
-- It is Eq a => [a] -> Bool
--
--6.
numbers :: (Eq a, Ord a, Num a) => a -> Int
numbers x
    | x < 0 = -1
    | x == 0 = 0
    | x > 0 = 1
    | otherwise = undefined
-- c) An indication of wherher the argument is postitive, negative or zero.
-- 7.
-- Anything that has instances of Eq and Ord and Num (Ord has Eq so it
-- suffices)
-- 8.
-- (Ord a, Num a, Num p) => a -> p
