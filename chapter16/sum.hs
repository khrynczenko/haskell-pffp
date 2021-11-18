{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

data Sum a b = First a | Second b deriving stock (Show, Eq)

instance Functor (Sum a) where
    fmap :: (b -> c) -> Sum a b -> Sum a c
    fmap f (Second x) = Second (f x)
    fmap _ (First x) = First x

-- Why we cannot have a functor that applies to the First? Because we cannot
-- specify that the second type argument should be the one that is part of
-- the structure.
