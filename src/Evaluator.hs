module Evaluator
    (   reduce
    ,   eval
    ) where

import Ast

reduceOp :: Operation -> Expression
reduceOp (BindOp id val)                  = OpExpr $ BindOp id (reduce val)
reduceOp (AddOp (LitExpr a) (LitExpr b))  = LitExpr (sumLit a b)
reduceOp (AddOp a b)                      = OpExpr $ AddOp (reduce a) (reduce b)
reduceOp (RemvOp (LitExpr a) (LitExpr b)) = LitExpr (subLit a b)
reduceOp (RemvOp a b)                     = OpExpr $ RemvOp (reduce a) (reduce b)
reduceOp (NegOp (LitExpr x))              = LitExpr (negLit x)
reduceOp (NegOp x)                        = OpExpr $ NegOp (reduce x)

reduce :: Expression -> Expression
reduce lit@(LitExpr _) = lit
reduce (OpExpr op)     = reduceOp op

eval :: Expression -> Expression
eval expr = if reduced == expr then expr else eval reduced
    where reduced = reduce expr


----- HELPERS -----
sumLit :: Literal -> Literal -> Literal
sumLit (IntLit a) (IntLit b) = IntLit (a + b)

subLit :: Literal -> Literal -> Literal
subLit (IntLit a) (IntLit b) = IntLit (a - b)

negLit :: Literal -> Literal
negLit (IntLit x) = IntLit (-x)