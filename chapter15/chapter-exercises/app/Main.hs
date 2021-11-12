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

instance Monoid Trivial where
    mempty = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial

-- 2.
newtype Identity a = Identity a deriving (Show, Eq)

instance Semigroup a => Semigroup (Identity a) where
    (<>) (Identity a) (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
    mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = fmap Identity arbitrary

-- 3.

data Two a b = Two a b deriving (Show, Eq)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (<>) (Two a b) (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

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

instance Monoid BoolDisj where
    mempty = BoolDisj False

instance Arbitrary BoolDisj where
    arbitrary = BoolDisj <$> arbitrary

-- 8.
data Or a b = Fst a | Snd b deriving (Show, Eq)

instance Semigroup (Or a b) where
    (<>) (Fst a) (Fst b) = Fst b
    (<>) (Fst a) (Snd b) = Snd b
    (<>) (Snd a) _ = Snd a

--instance Monoid a => Monoid (Or a b) where
    --mempty = Fst mempty
-- ^ Identity law cannot be satisfied

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary =
        frequency [(1, fmap Fst arbitrary), (1, fmap Snd arbitrary)]

-- 9.
newtype Combine a b = Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
    (<>) (Combine f) (Combine g) = Combine (\x -> g x <> f x)

instance Monoid b => Monoid (Combine a b) where
    mempty = Combine (\_ -> mempty)


instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = fmap Combine arbitrary 


-- 10.
newtype Comp a = Comp { unComp :: a -> a }

instance Semigroup (Comp a) where
    (<>) (Comp f) (Comp g) = Comp $ f . g

instance Monoid (Comp a) where
    mempty = Comp id

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



newtype Mem s a =  Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
    (<>) mem1 mem2 =
        Mem (\s -> (\x ->
                let a1 = fst x
                    (a2, s2) = runMem mem2 (snd x)
                in
                    (a1 <> a2, s2)) $ (runMem mem1) s)

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem (\s -> (mempty, s))

f' = Mem (\s -> ("hi", s + 1))

runMemExample :: IO ()
runMemExample = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty 0) 0
        rmright = runMem (mempty <> f') 0
    print rmleft
    print rmright
    print (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0


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
    quickCheck (prop_leftIdentity :: Trivial -> Bool)
    quickCheck (prop_rightIdentity :: Trivial -> Bool)
    -- 2.
    quickCheck (prop_binaryAssociativity :: Identity String -> Identity String -> Identity String -> Bool)
    quickCheck (prop_leftIdentity :: Identity String -> Bool)
    quickCheck (prop_rightIdentity :: Identity String -> Bool)
    -- 3.
    quickCheck (prop_binaryAssociativity :: Two String String -> Two String String -> Two String String -> Bool)
    quickCheck (prop_leftIdentity :: Two String String -> Bool)
    quickCheck (prop_rightIdentity :: Two String String -> Bool)
    -- 7.
    quickCheck (prop_binaryAssociativity :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
    quickCheck (prop_leftIdentity :: BoolDisj -> Bool)
    quickCheck (prop_rightIdentity :: BoolDisj -> Bool)
    -- 8.
    quickCheck (prop_binaryAssociativity :: Or String String -> Or String String -> Or String String -> Bool)
    -- 9.
    --quickCheck (prop_binaryAssociativity :: Combine String String -> Combine String String -> Combine String String -> Bool)
     -- ^ cannot be tested as functions don't have instance of Eq and Show
    -- 11.
    quickCheck (prop_binaryAssociativity :: Validation String String -> Validation String String -> Validation String String -> Bool)

