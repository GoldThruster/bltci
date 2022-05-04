module Lexer
    (   identifier
    ,   integer
    ,   equalOp
    ,   addOp
    ,   parens
    ) where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Text.Parsec.String (Parser)



------ WORDS ------
identifier :: Parser String
identifier = T.identifier boltLexer

------ LITERALS ------
integer :: Parser Integer
integer = T.integer boltLexer

------ OPERATORS ------
equalOp :: Parser ()
equalOp = T.reservedOp boltLexer "="

addOp :: Parser ()
addOp = T.reservedOp boltLexer "+"

------ SYMBOLS ------
parens :: Parsec String u a -> Parsec String u a
parens = T.parens boltLexer 

------ TOKEN GEN ------
boltDef :: LanguageDef u
boltDef = T.LanguageDef 
    { T.commentStart = "//"
    , T.commentEnd   = "//"
    , T.commentLine = ""
    , T.nestedComments = False
    , T.identStart = letter <|> char '_'
    , T.identLetter = letter <|> char '_' <|> digit
    , T.opStart = oneOf []
    , T.opLetter = oneOf "=+"
    , T.reservedOpNames = ["=", "+"]
    , T.reservedNames = []
    , T.caseSensitive = True
    }

boltLexer :: T.TokenParser u
boltLexer = T.makeTokenParser boltDef