module Main where

import Test.QuickCheck

newtype First' a = First' { getFirst' :: Maybe a} deriving (Show, Eq)

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = fmap First' arbitrary

instance Semigroup (First' a) where
    (<>) (First' (Just a)) (First' (Just b)) = First' (Just a)
    (<>) (First' Nothing) (First' (Just b)) = First' (Just b)
    (<>) (First' (Just a)) (First' Nothing) = First' (Just a)
    (<>) (First' Nothing) (First' Nothing) = First' Nothing

instance Monoid (First' a) where
    mempty = First' Nothing

-- SEMIGROUP
-- 1.
data Trivial = Trivial deriving (Show, Eq)

instance Semigroup Trivial where
    (<>) _ _ = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

-- 2.
newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

-- 3.

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return (Two a b)

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Show, Eq)

instance Semigroup BoolDisj where
    (<>) (BoolDisj True) _ = BoolDisj True
    (<>) _ (BoolDisj True) = BoolDisj True
    (<>) _ _ = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

-- 8.
data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
    (<>) (Fst a) (Fst b) = Fst b
    (<>) (Fst a) (Snd b) = Snd b
    (<>) (Snd a) _ = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary =
        frequency [(1, fmap Fst arbitrary), (1, fmap Snd arbitrary)]

-- 9.
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (\x -> g x <> f x)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = fmap Combine arbitrary 


-- 10.
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    (<>) (Comp f) (Comp g) = Comp $ f . g

-- 11.
data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (<>) (Failure' a) (Failure' b) = Failure' $ a <> b
    (<>) (Failure' a) (Success' b) = Success' b
    (<>) (Success' a) (Failure' b) = Success' a
    (<>) (Success' a) (Success' b) = Success' a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Failure' a, Success' b]


prop_binaryAssociativity :: (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_binaryAssociativity a b c = a <> b <> c == (a <> b) <> c

prop_leftIdentity :: (Eq a, Monoid a) => a -> Bool
prop_leftIdentity x = mempty <> x == x

prop_rightIdentity :: (Eq a, Monoid a) => a -> Bool
prop_rightIdentity x = x <> mempty == x

main :: IO ()
main = do
    -- Semigroup
    -- 1.
    quickCheck (prop_binaryAssociativity :: Trivial -> Trivial -> Trivial -> Bool)
    -- 2.
    quickCheck (prop_binaryAssociativity :: Identity String -> Identity String -> Identity String -> Bool)
    -- 3.
    quickCheck (prop_binaryAssociativity :: Two String String -> Two String String -> Two String String -> Bool)
    -- 7.
    quickCheck (prop_binaryAssociativity :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    -- 8.
    quickCheck (prop_binaryAssociativity :: Or String String -> Or String String -> Or String String -> Bool)
    -- 9.
    --quickCheck (prop_binaryAssociativity :: Combine String String -> Combine String String -> Combine String String -> Bool)
     -- ^ cannot be tested as functions don't have instance of Eq and Show
    -- 11.
    quickCheck (prop_binaryAssociativity :: Validation String String -> Validation String String -> Validation String String -> Bool)
