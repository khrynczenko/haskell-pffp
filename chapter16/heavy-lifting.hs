--a = (+1) $ read "[1]" :: [Int]
a = fmap (+1) $ read "[1]" :: [Int]

--b = (++ "lol") (Just ["Hi,", "Hello"])
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

--c = (* 2) (\x -> x - 2)
c = fmap (* 2) (\x -> x - 2)

fmapf :: (a -> b) -> (r -> a) -> (r -> b)
fmapf f g = (\x -> f (g x))

c' = fmap (* 2) (\x -> x - 2)

--d =
    --((return '1' ++) . show)
    --(\x -> [x, 1..3])

d =
    fmap ((return '1' ++) . show)
    (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap read $ fmap ("123"++) (fmap show ioi :: IO String)
    in  fmap (*3) changed
