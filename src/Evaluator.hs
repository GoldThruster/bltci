module Evaluator
    (   reduce
    ,   eval
    ) where

import Ast

reduceOp :: Operation -> Expression
reduceOp (AddOp (LitExpr a) (LitExpr b)) = LitExpr (sumLit a b)
reduceOp (AddOp a b)                     = OpExpr $ AddOp (reduce a) (reduce b)
reduceOp (BindOp id val)                 = OpExpr $ BindOp id (reduce val)

reduce :: Expression -> Expression
reduce lit@(LitExpr _) = lit
reduce (OpExpr op)     = reduceOp op

eval :: Expression -> Expression
eval expr = if reduced == expr then expr else eval reduced
    where reduced = reduce expr


----- HELPERS -----
sumLit :: Literal -> Literal -> Literal
sumLit (IntLit a) (IntLit b) = IntLit (a + b)