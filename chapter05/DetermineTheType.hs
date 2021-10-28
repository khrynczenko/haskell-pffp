{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where

example = 1 -- :: Num a => a

-- 1.
example1a = (* 9) 6 -- :: Num a => a
example1b = head [(0, "doge"), (1, "kitteh")] -- :: Num a => (a, [Char])
example1c = head [(0 :: Integer, "doge"), (1, "kitteh")] -- :: (Integer, [Char])
example1d = if False then True else False -- :: Bool
example1e = length [1, 2, 3, 4, 5] -- :: Int
example1f = length [1, 2, 3, 4] > (length "TACOCAT") -- :: Bool

-- 2.
--x = 5
--y = x + 5
--w = y * 10 -- :: Num a => a

-- 3.
--x = 5
--y = x + 5
--z y = y * 10 -- :: Num a => a -> a

-- 4.
--x = 5
--y = x + 5
--f = 4 / y -- :: Fractional a => a

-- 5.
--x = "Julie"
--y = " <3 "
--z = "Haskell"
--f = x ++ y ++ z -- :: [Char]
