{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
    (
        assignment, 
        expr
    ) where

import Text.Parsec

import qualified Lexer as L
import Ast




assignment :: Parsec String u Assignment
assignment = do
    id <- L.identifier
    L.equalOp
    val <- expr
    return (Assignment id val)

expr0 = (AssignmentExpr <$> assignment) <|> (LitExpr <$> literal)

expr = expr0

literal = L.integer