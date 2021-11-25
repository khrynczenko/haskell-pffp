{-# LANGUAGE InstanceSigs #-}

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEvenB :: [Integer] -> [Integer]
twiceWhenEvenB xs = xs >>= (\x -> if even x then [x*x, x*x] else [x*x])

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
    then [x*x, x*x]
    else []

data List a = Nil | Cons a (List a) deriving (Eq, Show)

listFrom :: [a] -> List a
listFrom [] = Nil
listFrom (x:xs) = Cons x (listFrom xs)

concatList :: List (List a) -> List a
concatList Nil = Nil
concatList (Cons l ls) = l <> concatList ls


instance Semigroup (List a) where
    (<>) :: List a -> List a -> List a
    (<>) Nil rhs = rhs
    (<>) lhs Nil = lhs
    (<>) (Cons x xs) rhs = Cons x (xs <> rhs)

instance Monoid (List a) where
    mempty = Nil

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure :: a -> List a
    pure a = Cons a Nil

    (<*>) :: List (a -> b) -> List a -> List b
    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Cons f fs) xs = fmap f xs <> (fs <*> xs)

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    xs >>= f =  concatList $ fmap f xs


listApplicative = [(+1), (^2)] <*> [1, 2, 3]
myListApplicative = listFrom [(+1), (^2)] <*> listFrom [1, 2, 3]
applicativeWorks = listFrom listApplicative == myListApplicative

listMonad = twiceWhenEven' [1,2,3]
myListMonad = listFrom [1, 2, 3] >>= (\x -> if even x then (Cons (x * x) (Cons (x * x) Nil)) else Nil)
monadWorks = listFrom listMonad == myListMonad
