module Hutton where

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add r l) = eval r + eval l

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add r l) = concat ["(", printExpr r, " + ", printExpr l, ")"]

instance Show Expr where
    show = printExpr

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
