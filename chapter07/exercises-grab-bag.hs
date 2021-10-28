-- Exercises: Grab bag
-- 1.
mTh1 x y z = x * y * z
mTh2 x y = (\z -> x * y * z)
mTh3 x = (\y -> (\z -> x * y * z))
mTh4 = (\x -> (\y -> (\z -> x * y * z)))
-- They are all equivalent
--
-- 2.
-- mTh3 3 :: Num a => a -> a -> a
-- answer d
--
-- 3.
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
  where
    f = (\x -> x + 1)

addFive = (\x y -> (if x > y then y else x) + 5)

mflip f x y = f y x
