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
    BindOp id <$> expr

addition :: Parser Operation
addition = do
    termA <- expr0
    L.addOp
    AddOp termA <$> expr

removal :: Parser Operation
removal = do
    termA <- expr0
    L.removeOp
    RemvOp termA <$> expr

negation :: Parser Operation
negation = do
    L.negateOp
    NegOp <$> expr0

expr0 :: Parser Expression
expr0 = (LitExpr <$> literal) <|> wrapped <|> (OpExpr <$> (binding <|> negation))

expr1 :: Parser Expression
expr1 = OpExpr <$> (try addition <|> removal)

expr :: Parser Expression
expr = try expr1 <|> expr0

literal :: Parser Literal
literal = IntLit <$> L.integer

wrapped :: Parser Expression
wrapped = L.parens expr