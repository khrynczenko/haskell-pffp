{-# LANGUAGE InstanceSigs #-}
newtype F a b = F (a -> b)

apF (F f) x = f x

instance Semigroup b => Semigroup (F a b) where
    (<>) :: F a b -> F a b -> F a b
    (F f) <> (F g) = F (\x -> f x <> g x)

instance Functor (F a) where
    fmap :: (b -> c) -> F a b -> F a c
    fmap f (F g) = F (\x -> f (g x))

instance Applicative (F a) where
    pure :: b -> F a b
    pure x = F (const x)

    (<*>) :: F a (b -> c) -> F a b -> F a c
    (<*>) (F f) (F g) = F (\x -> let f'= f x in f' (g x))

instance Monad (F a) where
    (>>=) :: F a b -> (b -> F a c) -> F a c
    (F f) >>= g = F (\x ->  apF (g . f $ x) x)
