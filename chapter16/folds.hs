-- foldl
myFoldl' :: (b -> a -> b) -> b -> [a] -> b
myFoldl' _ acc [] = acc
myFoldl' f acc (x:xs) = myFoldl' f (f acc x) xs

-- myFoldl' (+) 0 [1, 2, 3]
-- (((0 + 1) + 2) + 3)

myFoldr' :: (a -> b -> b) -> b -> [a] -> b
myFoldr' _ acc [] = acc
myFoldr' f acc (x:xs) = myFoldr' f (f x acc) xs

-- myFoldr' (+) 0 [1, 2, 3]
-- (3 + (2 + (1 + 0)))

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ acc [] = acc
myFoldl f acc (x:xs) = f (myFoldl f acc xs) x

-- myFoldl (+) 0 [1, 2, 3]
-- (((0 + 3) + 2) + 1)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ acc [] = acc
myFoldr f acc (x:xs) = f x (myFoldr f acc xs)

-- myFoldr (+) 0 [1, 2, 3]
-- (1 + (2 + (3 + 0)))

-- I got associativity for all of the above forls right I believe.
-- Unfortunately I cannot comprehend other differences between them. Also
-- myFoldl' and myFoldr' are supposed to be strict, but they are not as of now.
-- I thought that myFoldl' and myFoldr' are tail recursive (I still believe
-- they are) but the performance looks the same for their non TCO counterparts.
-- I suppose this has to do with laziness of Haskell.
