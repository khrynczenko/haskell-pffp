{-# LANGUAGE InstanceSigs #-}
module Main where

import Data.Monoid
import Control.Applicative

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

-- 1.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure :: a -> Pair a
    pure x = Pair x x

    (<*>) :: Pair (a -> b) -> Pair a -> Pair b
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)

instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary :: Gen (Pair a)
    arbitrary = Pair <$> arbitrary <*> arbitrary

instance (Eq a) => EqProp (Pair a) where
    (=-=) = eq

-- 2.
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
    fmap :: (b -> c) -> Two a b -> Two a c
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure :: b -> Two a b
    pure = Two mempty
    (<*>) :: Two a (b -> c) -> Two a b -> Two a c
    (<*>) (Two a1 f) (Two a2 b) = Two (a1 <> a2) (f b)
    --(<*>) (Two a1 f) (Two a2 b) = Two a2 (f b)
    -- ^ this implementations does not hold to the *interchange* law

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary :: Gen (Two a b)
    arbitrary = Two <$> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Two a b) where
    (=-=) = eq

-- 3.
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure :: c -> Three a b c
    pure = Three mempty mempty

    (<*>) :: Three a b (c -> d) -> Three a b c -> Three a b d
    (<*>) (Three a1 b1 f) (Three a2 b2 c) = Three (a1 <> a2) (b1 <> b2) (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary :: Gen (Three a b c)
    arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq

-- My other applicative
data Maybe' a = Nothing' | Just' a deriving (Eq, Show)

instance Functor Maybe' where
    fmap :: (a -> b) -> Maybe' a -> Maybe' b
    fmap _ Nothing' = Nothing'
    fmap f (Just' a) = Just' (f a)

instance Applicative Maybe' where
    pure :: a -> Maybe' a
    pure = Just'

    (<*>) :: Maybe' (a -> b) -> Maybe' a -> Maybe' b
    (<*>) Nothing' Nothing' = Nothing'
    (<*>) Nothing' _' = Nothing'
    (<*>) _ Nothing' = Nothing'
    (<*>) (Just' f) (Just' a) = Just' (f a)

instance Arbitrary a => Arbitrary (Maybe' a) where
    arbitrary :: Gen (Maybe' a)
    arbitrary = fmap Just' arbitrary

instance (Eq a) => EqProp (Maybe' a) where
    (=-=) = eq

main :: IO ()
main = do
    quickBatch $ applicative $ Pair (1 :: Int, 2 :: Int, 3 :: Int) (1 :: Int, 2 :: Int, 3 :: Int)
    quickBatch $ applicative $ Two "String" (1 :: Int, 2 :: Int, 3 :: Int) 
    quickBatch $ applicative $ Three "String" "String" (1 :: Int, 2 :: Int, 3 :: Int) 
    quickBatch $ applicative $ Just' (1 :: Int, 2 :: Int, 3 :: Int) 
