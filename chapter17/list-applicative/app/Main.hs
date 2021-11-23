{-# LANGUAGE InstanceSigs #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
    (<>) Nil ys = ys
    (<>) (Cons x Nil) ys = Cons x ys
    (<>) (Cons x xs) ys = Cons x (xs <> ys)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons x rest) = Cons (f x) (fmap f rest)

instance Applicative List where
    pure :: a -> List a
    pure x = Cons x Nil

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)


instance Arbitrary a => Arbitrary (List a) where
    arbitrary :: Gen (List a)
    arbitrary = do
        values <- listOf arbitrary
        return $ foldr Cons Nil values

instance (Eq a) => EqProp (List a) where
    (=-=) :: List a -> List a -> Property
    (=-=) = eq



main :: IO ()
main = do
    quickBatch $ applicative (Cons (1 :: Int, 1 :: Int, 1 :: Int) Nil)
