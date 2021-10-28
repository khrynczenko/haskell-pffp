-- Exercise: EnumFromTo
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool True False = []
eftBool False True = [False, True]
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd GT _ = []
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd LT LT = [LT]
eftOrd EQ GT = [EQ, GT]
eftOrd EQ LT = []
eftOrd EQ EQ = [EQ]

eftInt :: Int -> Int -> [Int]
eftInt lhs rhs
    | rhs >= lhs = lhs : eftInt (lhs + 1) rhs
    | otherwise = []



eftChar :: Char -> Char -> [Char]
eftChar lhs rhs
    | rhs >= lhs = lhs : eftChar (succ lhs) rhs
    | otherwise = []
