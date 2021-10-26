import Data.List
import Data.Maybe
-- Multiple choice
-- 1. Given the following datatype
data Weekday =
      Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
-- which of the following is true?
-- a) Weekday is a type with five data constructure
-- b) Weekday is a tree with five branches
-- c) Weekday is a product type
-- d) Weekday takes five arguments
-- Answer: a


-- 2. With the same datatype definition in mind, what is the type of the
--    following function f?
-- f Friday = "Miller Timne"
-- Answer: Its type is Weekday -> String so c)

-- 3. Type defined with the data keyword
-- a) Must have at least one argument.
-- b) Must begin with capital letter.
-- c) Must be polymorphic.
-- d) Cannot be imported from modules.
-- Answer: )

-- 4. The functions `g xs = xs !! (length xs - 1)`
-- a) Is recursive and may not terminate
-- b) Returns the head of xs
-- c) returns the final element of xs
-- d) Has the same type as xs
-- Answer: c

-- Ciphers
--
--

lettersStream = cycle ['a'..'z']

type Keyword = String

caesar :: String -> Keyword -> String
caesar str keyword = encode str keywordedCode
    where
        keywordedCode = makeKeywordCode str keyword

makeKeywordCode :: String -> Keyword -> String
makeKeywordCode code keyword = map snd $ zip code (cycle keyword)

encode :: String -> String -> String
encode str keywordedCode = map (\(letter, shift) -> encryptLetter letter shift) $ zip str shifts
    where
        shifts = map ((`div` 97) . fromEnum) keywordedCode

encryptLetter :: Char -> Int -> Char
encryptLetter letter shift = lettersStream !! (fromJust index + shift)
  where
    index = elemIndex letter lettersStream

