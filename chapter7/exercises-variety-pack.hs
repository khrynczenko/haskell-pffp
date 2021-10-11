-- Exercises: Variety pack
--
-- 1.

k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k (4 - 1, 10)

k2 :: [Char]
k2 = k ("three", (1 + 2))

k3 :: Num a => a
k3 = k (3, True)

-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

