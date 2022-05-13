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
    BindOp id <$> expr

expr :: Parser Expression
expr = (OpExpr <$> binding) <|> operation

operation :: Parser Expression
operation = E.buildExpressionParser opTable term

literal :: Parser Expression
literal = LitExpr <$> (
            (IntLit  <$> L.integer)
        <|> (BoolLit <$> L.boolean)
        <|> (StrLit  <$> L.string')
    )

wrapped :: Parser Expression
wrapped = L.parens expr

term :: Parser Expression
term = literal <|> wrapped

opTable :: [[E.Operator String () Data.Functor.Identity.Identity Expression]]
opTable = [ [prefix "-" makeNegation]
          , [binary "+" makeAddition E.AssocLeft, binary "-" makeRemoval E.AssocLeft]
          ]


------ HELPERS ------
makeAddition = (OpExpr .) . AddOp

makeRemoval = (OpExpr .) . RemvOp

makeNegation = OpExpr . NegOp

binary  name fun = E.Infix   (do{ T.reservedOp boltLexer name; return fun })
prefix  name fun = E.Prefix  (do{ T.reservedOp boltLexer name; return fun })
postfix name fun = E.Postfix (do{ T.reservedOp boltLexer name; return fun })