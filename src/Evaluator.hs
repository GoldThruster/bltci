module Evaluator
    (   reduce
    ,   eval
    ) where

import Ast
import Data.List (stripPrefix)
import Data.Maybe

reduceOp :: Operation -> Maybe Expression
reduceOp (BindOp id val)                  = applyReduction (OpExpr . BindOp id) val
reduceOp (AddOp (LitExpr a) (LitExpr b))  = Just $ LitExpr (sumLit a b)
reduceOp (AddOp a b)                      = applyReduction2 ((OpExpr .) . AddOp) a b
reduceOp (RemvOp (LitExpr a) (LitExpr b)) = Just $ LitExpr (subLit a b)
reduceOp (RemvOp a b)                     = applyReduction2 ((OpExpr .) . RemvOp) a b
reduceOp (NegOp (LitExpr x))              = Just $ LitExpr (negLit x)
reduceOp (NegOp x)                        = applyReduction (OpExpr . NegOp) x
reduceOp (OrOp a b)
    | a == b = Just a
    | otherwise = applyReduction2 ((OpExpr .) . OrOp) a b


reduce :: Expression -> Maybe Expression
reduce lit@(LitExpr _)   = Nothing
reduce (OpExpr op)       = reduceOp op
reduce call@(CallExpr _) = Nothing

forceReduce :: Expression -> Expression
forceReduce expr = fromMaybe expr (reduce expr)

applyReduction :: (Expression -> Expression) -> Expression -> Maybe Expression
applyReduction func expr =
    case reduced of
        Nothing -> Nothing
        Just x -> Just (func x)
    where reduced = reduce expr

applyReduction2 :: (Expression -> Expression -> Expression) -> Expression -> Expression  -> Maybe Expression
applyReduction2 func expr0 expr1 =
        case (reduced0, reduced1) of
            (Nothing, Nothing) -> Nothing
            (a, b)             -> Just $ func (final0 a) (final1 b)
    where reduced0 = reduce expr0
          reduced1 = reduce expr1
          final0 = fromMaybe expr0
          final1 = fromMaybe expr1

eval :: Expression -> Expression
eval expr = maybe expr eval reduced
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