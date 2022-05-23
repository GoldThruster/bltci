module Ast 
    (   Expression (..)
    ,   Literal (..)
    ,   Operation (..)
    ,   Id (..)
    ) where


newtype Id = Id String deriving (Show, Eq)
data Literal 
    = IntLit     Integer
    | BoolLit    Bool
    | StrLit     String
    deriving (Show, Eq)
data Operation 
    = AddOp   Expression Expression
    | RemvOp  Expression Expression
    | BindOp  Id         Expression
    | NegOp   Expression
    deriving (Show, Eq)

data Type = Type Id (Expression -> Bool)

data Expression = LitExpr Literal | OpExpr Operation | CallExpr Id deriving (Show, Eq)