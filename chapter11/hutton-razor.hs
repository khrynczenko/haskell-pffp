data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit v) = v
eval (Add lhs rhs) = eval lhs + eval rhs

printExpr :: Expr -> String
printExpr (Lit v) = show v
printExpr (Add lhs rhs) = "(" ++ printExpr lhs ++ "+" ++ printExpr rhs ++ ")"
