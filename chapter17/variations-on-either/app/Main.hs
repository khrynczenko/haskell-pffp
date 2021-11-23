{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Monoid

import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Validation' e a =
      Failure' e
    | Success' a
    deriving (Eq, Show)

instance Functor (Validation' e) where
    fmap :: (a -> b) -> Validation' e a -> Validation' e b
    fmap f (Success' a) = Success' (f a)
    fmap _ (Failure' e) = Failure' e

instance (Monoid e) => Applicative (Validation' e) where
    pure :: a -> Validation' e a
    pure = Success'

    (<*>) :: Validation' e (a -> b) -> Validation' e a -> Validation' e b
    (<*>) (Failure' e1) (Failure' e2) = Failure' (e1 <> e2)
    (<*>) (Failure' e) _ = Failure' e
    (<*>) _ (Failure' e) = Failure' e
    (<*>) (Success' f) (Success' a) = Success' (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation' e a) where
    arbitrary :: Gen (Validation' e a)
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [Failure' e, Success' a]


instance (Eq e, Eq a) => EqProp (Validation' e a) where
    (=-=) = eq




main :: IO ()
main = do
    quickBatch $ applicative $ (Success' (1 :: Int, 2 :: Int, 3 :: Int) :: Validation' String (Int, Int, Int))
