import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello World"
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

-- 1. Write a function that filters for DbDate values and returns a list
--    of Integer values inside them

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate ((DbDate time):items) = time : filterDbDate items
filterDbDate (item:items) = filterDbDate items


getDbDate :: DatabaseItem -> [UTCTime] -> [UTCTime]
getDbDate (DbDate time) acc = time : acc
getDbDate _ acc = acc

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' items = foldr getDbDate [] items

-- 2. Write a function that filters for DbNumber values and returns a list
--    of Integer values inside them

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber ((DbNumber number):items) = number : filterDbNumber items
filterDbNumber (item:items) = filterDbNumber items

getDbNumber :: DatabaseItem -> [Integer] -> [Integer]
getDbNumber (DbNumber number) acc = number : acc
getDbNumber _ acc = acc

filterDbNumber' :: [DatabaseItem] -> [Integer]
filterDbNumber' items = foldr getDbNumber [] items

-- 3. Write a function that gets most recent date

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent items = foldr (\date acc -> if date > acc then date else acc) firstDate restDates
    where
        dates = filterDbDate' items
        firstDate = head dates
        restDates = tail dates

-- 4. Write a function that sums all of the DbNumber values
sumDb :: [DatabaseItem] -> Integer
sumDb items = foldr (+) 0 numbersOnly
    where
        numbersOnly = filterDbNumber' items

-- 5. Write a function that gets the average of the DbNumber values
avgDb :: [DatabaseItem] -> Double
avgDb items = fromIntegral sum / fromIntegral values
    where
        (sum, values) = foldr (\number (s, n) -> (s + number, n + 1)) (0, 0) numbersOnly
        numbersOnly = filterDbNumber' items
