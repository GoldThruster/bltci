module Ast 
    ( Expression0 (..)
    , Expression1 (..)
    , Expression (..)
    , Assignment (..)
    , BinOperation (..)
    ) where

data Expression0
    = LitExpr Literal
    | AssignmentExpr Assignment
    | WrappedExpr Expression
    deriving Show

newtype Expression1
    = AddExpr (BinOperation Expression0) deriving Show

data Assignment = Assignment {id :: String, value :: Expression} deriving Show
type Literal = Integer

data BinOperation a = BinOperation {termA :: a, termB :: Expression} deriving Show

data Expression = Expr0 Expression0 | Expr1 Expression1 deriving Show