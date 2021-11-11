module Addition where

import Test.Hspec
import Test.QuickCheck

divideBy :: Integral a => a -> a -> (a, a)
divideBy num denom = go num denom 0
  where
    go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

mult :: (Eq a, Num a) => a -> a -> a
mult 0 _ = 0
mult _ 0 = 0
mult a b = a + mult a (b - 1)

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "1 + 1 is greater than 1" $ do
            (1 + 1) > (1 :: Int) `shouldBe` True
        it "2 + 2 should be equal to 3" $ do
            (2 + 2) `shouldBe` 4
        it "15 divided by 3 is 5" $ do
            (divideBy 15 5) `shouldBe` (3, 0)
        it "3 multiplied by 6 is 18" $ do
            mult 3 6 `shouldBe` 18
        it "x + 1 is always > x" $ do
            property $ (\x -> x + 1 > (x :: Int))
