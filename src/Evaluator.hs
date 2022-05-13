module Evaluator
    (   reduce
    ,   eval
    ) where

import Ast
import Data.List (stripPrefix)
import Data.Maybe

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
sumLit (StrLit a) (StrLit b) = StrLit (a ++ b)
sumLit _ _ = error "Type does not support 'add(_ + _)'"

subLit :: Literal -> Literal -> Literal
subLit (IntLit a) (IntLit b) = IntLit (a - b)
subLit (StrLit a) (StrLit b) = StrLit (fromMaybe a (stripPrefix b a))
subLit _ _ = error "Type does not support 'neg(_ - _)'"

negLit :: Literal -> Literal
negLit (IntLit x) = IntLit (-x)
negLit _  = error "Type does not support 'neg(- _)'"