-- 1.
--const <$> Just "Hello" <*> "World"
x = const <$> Just "Hello" <*> Just "World"

-- 2.
--(,,,) Just 90 <*> Just 10 Just "Tierness" [1, 2, 3]
y = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
