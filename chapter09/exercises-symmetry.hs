-- Exercises: Thy fearful symmetry
-- 1.
-- A assumed that I cannot use anything beside `takeWhile` and `dropWhile`
myWords :: String -> [String]
myWords "" = []
myWords str = takeWhile (/= ' ') str : myWords (drop 1 (dropWhile (/= ' ') str))

-- 2.
myLines :: String -> [String]
myLines "" = []
myLines str = takeWhile (/= '\n') str : myLines (drop 1 (dropWhile (/= '\n') str))

--3.
splitOn' :: Char -> String -> [String]
splitOn' delim "" = []
splitOn' delim str =  takeWhile (/= delim) str : splitOn' delim (drop 1 (dropWhile (/= delim) str))
