{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DerivingStrategies #-}

data Possibly a = LolNope | Yeppers a deriving stock (Show, Eq)

instance Functor Possibly where
    fmap :: (a -> b) -> Possibly a -> Possibly b
    fmap f (Yeppers x) = Yeppers (f x)
    fmap _ LolNope = LolNope
