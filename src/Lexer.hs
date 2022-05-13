module Lexer
    (   -- Words --
        identifier
        -- Literals --
    ,   integer
    ,   boolean
    ,   string'
        -- Operators --
    ,   equalOp
    ,   addOp
    ,   removeOp
        -- Symbols --
    ,   parens
    ,   boltLexer
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
integer = T.natural boltLexer

boolean :: Parser Bool
boolean = do{string "True"; return True} <|> do {string "False"; return False}

string' :: Parser String
string' = T.stringLiteral boltLexer

------ OPERATORS ------
equalOp :: Parser ()
equalOp = T.reservedOp boltLexer "="

addOp :: Parser ()
addOp = T.reservedOp boltLexer "+"

removeOp :: Parser ()
removeOp = T.reservedOp boltLexer "-"

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
    , T.reservedOpNames = ["=", "+", "-"]
    , T.reservedNames = ["True", "False"]
    , T.caseSensitive = True
    }

boltLexer :: T.TokenParser u
boltLexer = T.makeTokenParser boltDef