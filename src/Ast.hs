module Ast 
    (   Expression (..)
    ,   Literal (..)
    ,   Operation (..)
    ) where

newtype Literal = IntLit Integer deriving (Show, Eq)
data Operation 
    = AddOp  Expression Expression
    | RemvOp  Expression Expression
    | BindOp String     Expression
    | NegOp  Expression
    deriving (Show, Eq)

data Expression = LitExpr Literal | OpExpr Operation deriving (Show, Eq)