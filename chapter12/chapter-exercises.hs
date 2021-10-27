-- Determin the kinds
-- 1. Given:
-- id :: a -> a
-- What is the kind of a?
-- Answer: Kind of a is *
--
-- 2. r :: a -> f a
-- What are the kinds of f and a?
-- Answer:
-- f :: * -> *
-- a :: *

-- String processing
-- 1. Write a recursive function named replaceThe that takes text/stirng,
-- breaks it into words, and replaces each instance of "the" with "a".
-- It should replace exactly the word "the". notThe is a suggested helepr
-- function.

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe text = Just text

replaceThe :: String -> String
replaceThe [] = []
replaceThe text =
    case notThe firstWord of
        (Just word) -> word ++ " " ++ replaceThe rest
        Nothing -> "a " ++ replaceThe rest
  where
    firstWord = takeWhile (/= ' ') text
    rest = drop (length firstWord + 1) text


-- 2. Write a recursive function that takes string, breaks it into words,
-- counts the number of instances of "the" followed by a vowel initial word.

isAlphanumeric :: Char -> Bool
isAlphanumeric c = c >= '0' && c <= 'z'

extractWords :: String -> [String]
extractWords [] = []
extractWords text = firstWord : extractWords rest'
  where
    firstWord = takeWhile isAlphanumeric text
    rest = drop (length firstWord) text
    rest' = dropWhile (not . isAlphanumeric) rest

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:_) = x `elem` "aouiyeAOUIYE"

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = result
  where
    words = extractWords text
    wordsStartingWithVowel = map startsWithVowel words
    result = foldl (\acc x -> if x then acc + 1 else acc) 0 wordsStartingWithVowel

-- 3. Return the number of letters that are vowels in a word.
countVowels :: String -> Integer
countVowels [] = 0
countVowels word = if startsWithVowel word then 1 + countVowels (tail word) else countVowels (tail word)

-- Validate the word
newtype Word' =
    Word' String
    deriving (Show, Eq)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord str = if vowelsCount > consonantsCount then Nothing else Just (Word' str)
  where
    vowelsCount = sum $ map (\x -> if x then 1 else 0) $ map (`elem` vowels) str
    consonantsCount = length str - vowelsCount

-- It is only natural

data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
    | x < 0 = Nothing
    | x == 0 = Just Zero
    | otherwise = Just $ go x
  where
    go y = if y > 0 then Succ (go (y - 1)) else Zero


-- Small library for Maybe
-- 1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False



-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee _ f (Just a) = f a
mayybee x _ _ = x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe x Nothing = x

--4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]


-- 5.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes ((Just x):xs) = x : catMaybes xs
catMaybes (Nothing:xs) = catMaybes xs

-- 6.
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs = if any isNothing xs then Nothing else Just (catMaybes xs)

-- Small library for Either
--


-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left a) acc = a : acc
    f (Right _) acc = acc
