{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
    (   expr
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Lexer as L
import Ast hiding (value)


binding :: Parser Operation
binding = do
    id <- L.identifier
    L.equalOp
    val <- expr
    return $ BindOp id val

addition :: Parser Operation
addition = do
    termA <- expr0
    L.addOp
    termB <- expr
    return $ AddOp termA termB

expr0 :: Parser Expression
expr0 = (LitExpr <$> literal) <|> wrapped <|> (OpExpr <$> binding)

expr1 :: Parser Expression
expr1 = OpExpr <$> addition

expr :: Parser Expression
expr = try expr1 <|> expr0

literal :: Parser Literal
literal = IntLit <$> L.integer

wrapped :: Parser Expression
wrapped = L.parens expr