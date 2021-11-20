import Data.List (elemIndex)
--lookup :: [(a, b)] -> a -> Maybe b
--lookup [] _ = Nothing
--lookup ((a, b):rest) c = if a == c then Just b else lookup rest a

-- 1.
added :: Maybe Integer
--added = (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y  <*> z

-- 3.
x1 :: Maybe Int
x1 = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x1 <*> y1
