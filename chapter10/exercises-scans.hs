
fibs = 1 : scanl (+) 1 fibs

-- 1 : (1 : ((1 + 1):((1 + 1) + 1) : []))

-- 1. Modify your fibs function to only return the first 20 fibonacci numbers.
fibs1 = take 20 $ 1 : scanl (+) 1 fibs

-- 2. Modify fibonacci numbers that are less than 100
fibs2 = takeWhile (<100) $ 1 : scanl (+) 1 fibs

-- 3. Write factorial in terms of scan
factorial = scanl (*) 1 [2..]
