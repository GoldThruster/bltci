module Parser
    (   expr
    ) where

import qualified Data.Functor.Identity
import Text.Parsec
import qualified Text.Parsec.Expr as E
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as T

import qualified Lexer as L
import Ast hiding (value)
import Lexer (boltLexer)


binding :: Parser Operation
binding = do
    id <- L.identifier
    L.equalOp
    BindOp (Id id) <$> expr

expr :: Parser Expression
expr = (OpExpr <$> binding) <|> operation

operation :: Parser Expression
operation = E.buildExpressionParser opTable term

literal :: Parser Expression
literal = LitExpr <$> (
            (IntLit  <$> L.integer)
        <|> (BoolLit <$> L.boolean)
        <|> (StrLit  <$> L.string')
        <|> fixed SelfLit L.self
    )

wrapped :: Parser Expression
wrapped = L.parens expr

term :: Parser Expression
term = literal <|> wrapped

opTable :: [[E.Operator String () Data.Functor.Identity.Identity Expression]]
opTable = [ [prefix "-" NegOp]
          , [binary "+" AddOp E.AssocLeft, binary "-" RemvOp E.AssocLeft]
          , [binary "|" OrOp  E.AssocLeft]
          ]


------ HELPERS ------
fixed what parser = do {parser; return what}

binary  name fun = E.Infix   (do{ T.reservedOp boltLexer name; return (wrapExprBin fun) })
prefix  name fun = E.Prefix  (do{ T.reservedOp boltLexer name; return (OpExpr . fun) })
postfix name fun = E.Postfix (do{ T.reservedOp boltLexer name; return (OpExpr . fun) })

wrapExprBin = ((OpExpr .) .)