module Ast 
    (   Expression (..)
    ,   Literal (..)
    ,   Operation (..)
    ) where


data Literal 
    = IntLit  Integer
    | BoolLit Bool
    | StrLit  String
    deriving (Show, Eq)
data Operation 
    = AddOp  Expression Expression
    | RemvOp Expression Expression
    | BindOp String     Expression
    | NegOp  Expression
    deriving (Show, Eq)

data Expression = LitExpr Literal | OpExpr Operation deriving (Show, Eq)