module Ast 
    ( Expression0 (..)
    , Assignment (..)
    ) where

data Expression0
    = LitExpr Literal
    | AssignmentExpr Assignment
    deriving Show

data Assignment = Assignment {id :: String, value :: Expression} deriving Show
type Expression = Expression0
type Literal = Integer