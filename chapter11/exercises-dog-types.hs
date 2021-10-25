-- Exercises: Dog types
data Doggies a = Husky a | Mustiff a deriving (Eq, Show)

-- 1. Is doggies a type constructor od data cnstructor?
-- It is a type constructor.
-- 2. What is the kind of Doggies?
-- Its kind is * -> * (must be applied to a type constant)
-- 3. What is the kind of Doggies String?
-- Its kind is *, it is a types constant at this point.
-- 4. What is the type of Husky 10?
-- Its type is Num a => Doggies a
-- 5. What is the type of Husky (10 :: Integer)?
-- Its type is Doggies Integer
-- 6. What is the type of Mastiff "Scooby Doo"?
-- Its type is Doggies String

