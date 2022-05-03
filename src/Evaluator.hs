module Evaluator
(   reduce
,   eval
) where

import Ast

-- type Step = Either Expression Integer

reduce0 :: Expression0 -> Expression
reduce0 (AssignmentExpr assignment) = value assignment
reduce0 (WrappedExpr expr) = reduce expr
reduce0 x = Expr0 x

reduce1 :: Expression1 -> Expression
reduce1 (AddExpr (BinOperation (LitExpr a) (LitExpr b))) =  Expr0 (LitExpr (a + b))
reduce1 (AddExpr (BinOperation a b)) = Expr1 (AddExpr (BinOperation (WrappedExpr (reduce0 a)) (WrappedExpr (reduce0 b))))

reduce :: Expression -> Expression
reduce (Expr0 e0) = reduce0 e0
reduce (Expr1 e1) = reduce1 e1

eval :: Expression -> Integer
eval (Expr0 (LitExpr lit)) = lit
eval expr = eval (reduce expr)