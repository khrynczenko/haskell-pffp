{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Test.QuickCheck
import GHC.Arr

prop_functorIdentity :: (Eq (f a), Functor f) => f a -> Bool
prop_functorIdentity f = fmap id f == id f

prop_functorAssociativity :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
prop_functorAssociativity f g x = fmap g (fmap f x) ==  fmap (g . f) x

-- 1.
data Bool' = False | True
-- We cannot write functor instance for Bool' because it is a type constant

--2.
data BoolAndSomthingElse a = False' a | True' a deriving stock (Show, Eq)

instance Functor BoolAndSomthingElse where
    fmap :: (a -> b) -> BoolAndSomthingElse a -> BoolAndSomthingElse b
    fmap f (True' x) = True' (f x)
    fmap f (False' x) = False' (f x)

instance Arbitrary a => Arbitrary (BoolAndSomthingElse a) where
    arbitrary :: Gen (BoolAndSomthingElse a)
    arbitrary = do
        x <- arbitrary
        elements [False' x, True' x]

-- 3.

data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving stock (Show, Eq)
-- This is Maybe a

instance Functor BoolAndMaybeSomethingElse where
    fmap :: (a -> b) -> BoolAndMaybeSomethingElse a -> BoolAndMaybeSomethingElse b
    fmap f (Truish x) = Truish (f x)
    fmap _ Falsish = Falsish

instance Arbitrary a => Arbitrary (BoolAndMaybeSomethingElse a) where
    arbitrary :: Gen (BoolAndMaybeSomethingElse a)
    arbitrary = do
        x <- arbitrary
        elements [Falsish, Truish x]

-- 4.
newtype Mu f = InF { outF :: f (Mu f) }

--instance Functor Mu f where
    --fmap :: (a -> b) -> Mu f a -> Mu f b
    --fmap = undefined
-- It cannot be written because Mu hask kind (* -> *) -> * and if we apply to
-- f like Mu f it has a kind of *. Neither does work.

-- 5.
data D = D (Array Word Word) Int Int

-- Cannot be written because D has kind *.


-- Rearranging arguments
-- 1.
data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
    fmap f (First a) = First (f a)
    fmap f (Second b) = Second b

-- 2.
data Company a c b = DeepBlue a c | Something b

instance Functor (Company e e') where
    fmap f (Something b) = Something (f b)
    fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More b a =
    L a b a
  | R b a b
  deriving stock (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

-- Write a functor
-- 1.
data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
    fmap :: (b -> c) -> Quant a b -> Quant a c
    fmap _ Finance = Finance
    fmap _ (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
    arbitrary :: Gen (Quant a b)
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Finance, Desk a, Bloor b]

-- 2.
newtype K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap :: (b -> c) -> K a b -> K a c
    fmap _ (K a) = K a

instance Arbitrary a => Arbitrary (K a b) where
    arbitrary :: Gen (K a b)
    arbitrary = fmap K arbitrary

-- 3.
newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

instance Functor (Flip K a) where
    fmap :: (b -> c) -> Flip K a b -> Flip K a c
    fmap f (Flip (K x)) = Flip (K (f x))

instance Arbitrary b => Arbitrary (Flip K a b) where
    arbitrary :: Gen (Flip K a b)
    arbitrary = fmap Flip $ fmap K $ arbitrary

-- 4.
data EvilGoateeConst a b = GoatyConst b deriving stock (Eq, Show)

instance Functor (EvilGoateeConst a) where
    fmap :: (b -> c) -> EvilGoateeConst a b -> EvilGoateeConst a c
    fmap f (GoatyConst x) = GoatyConst (f x)


instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
    arbitrary :: Gen (EvilGoateeConst a b)
    arbitrary = fmap GoatyConst arbitrary


-- 5.
data LiftItOut f a = LiftItOut (f a) deriving stock (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
    fmap :: (a -> b) -> LiftItOut f a -> LiftItOut f b
    fmap f (LiftItOut x) = LiftItOut (fmap f x)

instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
    arbitrary :: Gen (LiftItOut f a)
    arbitrary = fmap LiftItOut arbitrary

-- 6.
data Parappa f g a = DaWrappa (f a) (g a) deriving stock (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap :: (a -> b) -> Parappa f g a -> Parappa f g b
    fmap f (DaWrappa x y) = DaWrappa (fmap f x) (fmap f y)

instance (Arbitrary (f a), Arbitrary (g a)) => Arbitrary (Parappa f g a) where
    arbitrary :: Gen (Parappa f g a)
    arbitrary = DaWrappa <$> arbitrary <*> arbitrary

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving stock (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap :: (b -> c) -> IgnoreOne f g a b -> IgnoreOne f g a c
    fmap f (IgnoringSomething x y) = IgnoringSomething x (fmap f y)

instance (Arbitrary (f a), Arbitrary (g b)) => Arbitrary (IgnoreOne f g a b) where
    arbitrary :: Gen (IgnoreOne f g a b)
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return (IgnoringSomething x y)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving stock (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
    fmap :: (t -> l) -> Notorious g o a t -> Notorious g o a l
    fmap f (Notorious x y z) = Notorious x y (fmap f z)

instance (Arbitrary (g o), Arbitrary (g a), Arbitrary (g t)) => Arbitrary (Notorious g o a t) where
    arbitrary :: Gen (Notorious g o a t)
    arbitrary = Notorious <$> arbitrary <*> arbitrary <*> arbitrary

-- 9.
data List a = Nil | Cons a (List a) deriving stock (Eq, Show)

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons x rest) = Cons (f x) (fmap f rest)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary :: Gen (List a)
    arbitrary = do
        x <- listOf arbitrary
        return $ foldr Cons Nil x

-- 10.
data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
  deriving (Eq, Show)

instance Functor GoatLord where
    fmap :: (a -> b) -> GoatLord a -> GoatLord b
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat x) = OneGoat (f x)
    fmap f (MoreGoats x y z) = MoreGoats (fmap f x) (fmap f y) (fmap f z)

main :: IO ()
main = do
    quickCheck (prop_functorIdentity :: BoolAndSomthingElse Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: BoolAndSomthingElse Int -> Bool)
    quickCheck (prop_functorIdentity :: BoolAndMaybeSomethingElse Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: BoolAndMaybeSomethingElse Int -> Bool)
    quickCheck (prop_functorIdentity :: Quant Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Quant Int Int -> Bool)
    quickCheck (prop_functorIdentity :: K Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: K Int Int -> Bool)
    quickCheck (prop_functorIdentity :: Flip K Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Flip K Int Int -> Bool)
    quickCheck (prop_functorIdentity :: EvilGoateeConst Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: EvilGoateeConst Int Int -> Bool)
    quickCheck (prop_functorIdentity :: LiftItOut Maybe Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: LiftItOut Maybe Int -> Bool)
    quickCheck (prop_functorIdentity :: Parappa Maybe Maybe Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Parappa Maybe Maybe Int -> Bool)
    quickCheck (prop_functorIdentity :: IgnoreOne Maybe Maybe Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: IgnoreOne Maybe Maybe Int Int -> Bool)
    quickCheck (prop_functorIdentity :: Notorious Maybe Int Int Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: Notorious Maybe Int Int Int -> Bool)
    quickCheck (prop_functorIdentity :: List Int -> Bool)
    quickCheck (prop_functorAssociativity (+1) (+1) :: List Int -> Bool)
