{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
    (
        assignment, 
        expr
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Lexer as L
import Ast





assignment :: Parser Assignment
assignment = do
    id <- L.identifier
    L.equalOp
    val <- expr
    return (Assignment id val)

expr0 :: Parser Expression0
expr0 = (AssignmentExpr <$> assignment) <|> (LitExpr <$> literal)

expr :: Parser Expression0
expr = expr0

literal :: Parser Integer
literal = L.integer