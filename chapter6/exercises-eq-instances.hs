-- Exercises: Eq instances

data TisAnInteger = 
    TisAn Integer

instance Eq TisAnInteger where
    (==) (TisAn x) (TisAn y) = x == y

data TwoIntegers =
    Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two x1 y1) (Two x2 y2) = x1 == x2 && y1 == y2

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt x) (TisAnInt y) = x == y
    (==) (TisAString x) (TisAString y) = x == y
    (==) _ _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair lhsX lhsY) (Pair rhsX rhsY) = lhsX == rhsX && lhsY == rhsY

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple lhsX lhsY) (Tuple rhsX rhsY) = lhsX == rhsX && lhsY == rhsY

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello x) (Hello y) = x == y
    (==) (Goodbye x) (Goodbye y) = x == y
    (==) _ _ = False
