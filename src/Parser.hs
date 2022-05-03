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

addition :: Parser (BinOperation Expression0)
addition = do
    termA <- expr0
    L.addOp
    termB <- expr0
    return (BinOperation termA termB)

expr0 :: Parser Expression0
expr0 = (AssignmentExpr <$> assignment) <|> (LitExpr <$> literal)

expr1 :: Parser Expression1
expr1 = AddExpr <$> addition

expr :: Parser Expression
expr = (Expr1 <$> try expr1) <|> (Expr0 <$> expr0)

literal :: Parser Integer
literal = L.integer