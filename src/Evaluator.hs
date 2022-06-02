module Evaluator
    (   reduce
    ,   eval
    ,   emptyState
    ,   updateState
    ,   Context (..)
    ) where

import Ast
import Data.List (stripPrefix)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

type Context = Map Ast.Id Ast.Expression

emptyState = Map.empty

updateState :: Context -> Ast.Expression -> Context
updateState previous (Ast.OpExpr (Ast.BindOp id val)) =
    Map.insert id val previous
updateState previus _ = previus

reduceOp :: Operation -> Context -> Maybe Expression
reduceOp (BindOp id val)                  ctx = applyReduction (OpExpr . BindOp id) val ctx
reduceOp (AddOp (LitExpr a) (LitExpr b))  _ = Just $ LitExpr (sumLit a b)
reduceOp (AddOp a b)                      ctx = applyReduction2 ((OpExpr .) . AddOp) a b ctx
reduceOp (RemvOp (LitExpr a) (LitExpr b)) _ = Just $ LitExpr (subLit a b)
reduceOp (RemvOp a b)                     ctx = applyReduction2 ((OpExpr .) . RemvOp) a b ctx
reduceOp (NegOp (LitExpr x))              _ = Just $ LitExpr (negLit x)
reduceOp (NegOp x)                        ctx = applyReduction (OpExpr . NegOp) x ctx
reduceOp (OrOp a b) ctx
    | a == b = Just a
    | otherwise = applyReduction2 ((OpExpr .) . OrOp) a b ctx


reduce :: Expression -> Context -> Maybe Expression
reduce lit@(LitExpr _)    _     = Nothing
reduce (OpExpr op)        ctx     = reduceOp op ctx
reduce call@(CallExpr id) ctx = Map.lookup id ctx

forceReduce :: Expression -> (Context -> Expression)
forceReduce expr = fromMaybe expr . reduce expr

applyReduction :: (Expression -> Expression) -> Expression -> Context -> Maybe Expression
applyReduction func expr state =
    case reduced of
        Nothing -> Nothing
        Just x -> Just (func x)
    where reduced = reduce expr state

applyReduction2 :: (Expression -> Expression -> Expression) -> Expression -> Expression -> Context -> Maybe Expression
applyReduction2 func expr0 expr1 ctx =
        case (reduced0, reduced1) of
            (Nothing, Nothing) -> Nothing
            (a, b)             -> Just $ func (final0 a) (final1 b)
    where reduced0 = reduce expr0  ctx
          reduced1 = reduce expr1  ctx
          final0 = fromMaybe expr0
          final1 = fromMaybe expr1

eval :: Expression -> Context -> Expression
eval expr ctx = maybe expr (`eval` ctx) reduced
    where reduced = reduce expr ctx

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