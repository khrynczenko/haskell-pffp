{-# LANGUAGE InstanceSigs #-}
module Main where


import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap :: (a -> b) -> Nope a -> Nope b
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure :: a -> Nope a
    pure _ = NopeDotJpg

    (<*>) :: Nope (a -> b) -> Nope a -> Nope b
    (<*>) _ _ = NopeDotJpg

instance Monad Nope where
    (>>=) :: Nope a -> (a -> Nope b) -> Nope b
    (>>=) _ _ = NopeDotJpg

instance Arbitrary (Nope a) where
    arbitrary :: Gen (Nope a)
    arbitrary = pure NopeDotJpg

instance Eq a => EqProp (Nope a) where
    (=-=) = eq

-- 2.
data BahEither b a = PLeft a | PRight b deriving (Eq, Show)

instance Functor (BahEither b) where
    fmap :: (a -> c) -> BahEither b a -> BahEither b c
    fmap _ (PRight b) = PRight b
    fmap f (PLeft a) = PLeft (f a)

instance Applicative (BahEither b) where
    pure :: a -> BahEither b a
    pure = PLeft

    (<*>) :: BahEither b (a -> c) -> BahEither b a -> BahEither b c
    (<*>) (PRight x) _ = PRight x
    (<*>) _ (PRight x) = PRight x
    (<*>) (PLeft f) (PLeft x) = PLeft (f x)

instance Monad (BahEither b) where
    (>>=) :: BahEither b a -> (a -> BahEither b c) -> BahEither b c
    PLeft x >>= f = f x
    (PRight v) >>= _ = PRight v

instance (Arbitrary a, Arbitrary b) => Arbitrary (BahEither b a) where
    arbitrary :: Gen (BahEither b a)
    arbitrary = do
        b <- arbitrary
        a <- arbitrary
        elements [PLeft a, PRight b]

instance (Eq a, Eq b) => EqProp (BahEither b a) where
    (=-=) = eq

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure :: a -> Identity a
    pure = Identity
    
    (<*>) :: Identity (a -> b) -> Identity a -> Identity b
    (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
    (>>=) :: Identity a -> (a -> Identity b) -> Identity b
    (Identity x) >>= f = f x

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary :: Gen (Identity a)
    arbitrary = fmap Identity arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

-- 4.

data List a = Nil | Cons a (List a) deriving (Eq, Show)

concatLists :: List a -> List a -> List a
concatLists Nil rhs = rhs
concatLists (Cons x xs) rhs = Cons x (concatLists xs rhs)

instance Semigroup (List a) where
    (<>) = concatLists

instance Monoid (List a) where
    mempty = Nil

instance Arbitrary a => Arbitrary (List a) where
    arbitrary :: Gen (List a)
    arbitrary = do
        values <- listOf arbitrary
        return $ foldr Cons Nil values

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure :: a -> List a
    pure = (`Cons` Nil)

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) rhs = fmap f rhs `concatLists` (fs <*> rhs)

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    Nil >>= _ = Nil
    (Cons x xs) >>= f =  f x `concatLists` (xs >>= f)

instance (Eq a) => EqProp (List a) where
    (=-=) :: List a -> List a -> Property
    (=-=) = eq

-- Write following functions
j :: Monad m => m (m a) -> m a
j m = m >>= id

test1 = j [[1, 2], [], [3]] == [1, 2, 3]
test2 = j (Just (Just 1)) == Just 1
test3 = j (Just Nothing :: Maybe (Maybe Int)) == Nothing

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f ma mb = f <$> ma <*> mb

a :: Monad m => m a -> m (a -> b) -> m b
a ma mf = mf <*> ma


main :: IO ()
main = do
    putStrLn "TEST"
    quickBatch $ monad (NopeDotJpg  :: Nope (Int, Int, Int))
    quickBatch $ monad (PLeft (1, 2, 3)  :: BahEither Int (Int, Int, Int))
    quickBatch $ monad (Identity (1, 2, 3)  :: Identity (Int, Int, Int))
    quickBatch $ monad (Cons (1, 2, 3) Nil  :: List (Int, Int, Int))
