module Ast 
    (   Expression (..)
    ,   Literal (..)
    ,   Operation (..)
    ) where

newtype Literal = IntLit Integer deriving (Show, Eq)
data Operation 
    = AddOp Expression Expression
    | BindOp String Expression 
    deriving (Show, Eq)

data Expression = LitExpr Literal | OpExpr Operation deriving (Show, Eq)