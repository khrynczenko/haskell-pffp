{-# LANGUAGE InstanceSigs #-}
newtype Constant a b = Constant a deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap :: (b -> c) -> Constant a b -> Constant a c
    fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
    pure :: b -> Constant a b
    pure _ = Constant mempty
    (<*>) :: Constant a (b -> c) -> Constant a b -> Constant a c
    (<*>) (Constant x) (Constant y) = Constant (x <> y)
