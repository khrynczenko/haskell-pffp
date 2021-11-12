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




prop_associative :: (Eq a, Semigroup a) => a -> a -> a -> Bool
prop_associative a b c = a <> b <> c == (a <> b) <> c

prop_leftIdentity :: (Eq a, Monoid a) => a -> Bool
prop_leftIdentity x = mempty <> x == x

prop_rightIdentity :: (Eq a, Monoid a) => a -> Bool
prop_rightIdentity x = x <> mempty == x

main :: IO ()
main = do
    quickCheck (prop_associative :: First' Int -> First' Int -> First' Int -> Bool)
    quickCheck (prop_leftIdentity :: First' Int -> Bool)
    quickCheck (prop_rightIdentity :: First' Int -> Bool)
