comp' :: (b -> c) -> (a -> b) -> (a -> c)
comp' f g x =  f (g x)

liftA2' :: (Functor f) => (a -> b) -> f (f a) -> f (f b)
liftA2' f functor = (fmap . fmap ) f functor


lift2M :: Maybe (Maybe Int) -> Maybe (Maybe Int)
lift2M (Just (Just x)) = (Just (Just (x + 1)))
lift2M (Just Nothing) = (Just (Nothing))
lift2M Nothing = Nothing


myComp :: (b -> c) -> (a -> b) -> (a -> c)
myComp f g = (\x -> f (g x))

-- fmap1 :: Functor f => (m -> x) -> f m -> f x
-- fmap2 :: Functor g => (n -> z) -> g n -> g z
-- myComp fmap1 :: Functor f =>  (a -> m -> x) -> (a -> f m -> f x)
-- myComp fmap1 fmap2
-- (Functor f, Functor g) =>  ((n -> z) -> f (g n) -> f (g z))
finallyGotIt :: (Functor f, Functor g) =>  ((n -> z) -> f (g n) -> f (g z))
finallyGotIt = myComp fmap fmap

