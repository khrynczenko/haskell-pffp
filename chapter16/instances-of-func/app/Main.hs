{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where

import Test.QuickCheck

prop_functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
prop_functorIdentity f = fmap id f == id f

prop_functorAssociativity :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
prop_functorAssociativity f g x = fmap g (fmap f x) ==  fmap (g . f) x


-- 1.
newtype Identity a = Identity a deriving stock (Eq, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary :: Gen (Identity a)
    arbitrary = fmap Identity arbitrary

-- 2.
data Pair a = Pair a a deriving stock (Eq, Show)

instance Functor Pair where
    fmap :: (a -> b) -> Pair a -> Pair b
    fmap f (Pair x y) = Pair (f x) (f y)

instance (Arbitrary a) => Arbitrary (Pair a) where
    arbitrary :: Gen (Pair a)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Pair a b)

-- 3.
data Two a b = Two a b deriving stock (Eq, Show)

instance Functor (Two a) where
    fmap :: (b -> c) -> Two a b -> Two a c
    fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary :: Gen (Two a b)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

-- 4.
data Three a b c = Three a b c deriving stock (Eq, Show)

instance Functor (Three a b) where
    fmap :: (c -> d) -> Three a b c -> Three a b d
    fmap f (Three x y z) = Three x y (f z)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary :: Gen (Three a b c)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return (Three a b c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary :: Gen (Three' a b)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return (Three' a b b')


-- 5.
data Three' a b = Three' a b b deriving stock (Eq, Show)

instance Functor (Three' a) where
    fmap :: (b -> c) -> Three' a b -> Three' a c
    fmap f (Three' x y z) = Three' x (f y) (f z)

-- Okay tasks 6 7 8 are can be solved analogicaly


data Trivial = Trivial
-- We cannot implement Functor instance for Trivial because Functor due to
-- the signature of fmap requires a type constructor with one type argument.
-- So a type whose kind is (* -> *). Trivial is a type constant (kind *).


main :: IO ()
main = do
    quickCheck (prop_functorIdentity :: Identity Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Identity Int -> Bool)
    quickCheck (prop_functorIdentity :: Pair Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Pair Int -> Bool)
    quickCheck (prop_functorIdentity :: Two Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Two Int Int -> Bool)
    quickCheck (prop_functorIdentity :: Three Int Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Three Int Int Int -> Bool)
    quickCheck (prop_functorIdentity :: Three' Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Three' Int Int -> Bool)

