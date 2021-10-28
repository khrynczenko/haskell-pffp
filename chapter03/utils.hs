module Utils where

take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

drop' :: Int -> [a] -> [a]
drop' n [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n - 1) xs
