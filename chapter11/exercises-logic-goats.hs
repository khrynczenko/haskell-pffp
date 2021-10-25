{-# LANGUAGE FlexibleInstances #-}

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany (Int, String) where
    tooMany (n, _) = n > 32

instance TooMany (Int, Int) where
    tooMany (x, y) = x + y > 32

--instance TooMany (Num a , TooMany a) => (a, a) where
    --tooMany (x, y) = x + y > 32
