{-# LANGUAGE InstanceSigs #-}
module Main where


import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap :: (b -> c) -> Sum a b -> Sum a c
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
    pure :: b -> Sum a b
    pure = Second

    (<*>) :: Sum a (b -> c) -> Sum a b -> Sum a c
    (<*>) (First a) _ = First a
    (<*>) _ (First a) = First a
    (<*>) (Second f) (Second b) = Second (f b)


instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
    arbitrary :: Gen (Sum a b)
    arbitrary = do
        e <- arbitrary
        a <- arbitrary
        elements [First e, Second a]

instance Monad (Sum a) where
    (>>=) :: Sum a b -> (b -> Sum a c) -> Sum a c
    (>>=) (First a) _ = First a
    (>>=) (Second b) f = f b


instance (Eq a, Eq b) => EqProp (Sum a b) where
    (=-=) = eq


ap' :: Sum a (b -> c) -> Sum a b -> Sum a c
ap' (First a) _ = First a
ap' (Second f) x = x >>= (\x -> pure . f $ x)

main :: IO ()
main = do
    quickBatch $ monad $ (Second (1 :: Int, 2 :: Int, 3 :: Int) :: Sum String (Int, Int, Int))
