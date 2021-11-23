{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

newtype ZipList' a = ZipList' [a] deriving (Eq, Show)

instance Semigroup a => Semigroup (ZipList' a) where
    (<>) (ZipList' []) (ZipList' _) = ZipList' []
    (<>) (ZipList' _) (ZipList' []) = ZipList' []
    (<>) (ZipList' xs) (ZipList' ys) = ZipList' $ zipWith (<>) xs ys

instance Functor ZipList' where
    fmap :: (a -> b) -> ZipList' a -> ZipList' b
    fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
    pure :: a -> ZipList' a
    pure x = ZipList' (repeat x)

    (<*>) (ZipList' fs) (ZipList' xs) =
        ZipList' $ zipWith ($) fs xs

instance Arbitrary a => Arbitrary (ZipList' a) where
    arbitrary :: Gen (ZipList' a)
    arbitrary = fmap ZipList' arbitrary

instance (Eq a) => EqProp (ZipList' a) where
    (=-=) :: ZipList' a -> ZipList' a -> Property
    (=-=) xs ys = xs' `eq` ys'
      where
        xs' = let (ZipList' l) = xs
              in take 3000 l
        ys' = let (ZipList' l) = ys
              in take 3000 l


main :: IO ()
main = do
    quickBatch $ semigroup (ZipList' [Sum 1 :: Sum Int] , 1 :: Int)
    quickBatch $ applicative $ ZipList' [(1 :: Int, 2 :: Int, 3 :: Int)]
