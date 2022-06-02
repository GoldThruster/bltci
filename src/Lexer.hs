module Lexer
    (   -- Words --
        identifier
    ,   call
        -- Literals --
    ,   integer
    ,   boolean
    ,   string'
    ,   self
        -- Operators --
    ,   equalOp
    ,   addOp
    ,   removeOp
    ,   orOp
        -- Symbols --
    ,   parens
    ,   boltLexer
    ) where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T
import Text.Parsec.String (Parser)
import Data.Char (toLower)



------ WORDS ------
identifier :: Parser String
identifier = do
    (start : body) <- T.identifier boltLexer
    return (toLower start : body)

call :: Parser String
call = do
    start <- lower <|> char '_'
    body <- many idLetter
    spaces
    return (start : body)

------ LITERALS ------
integer :: Parser Integer
integer = T.natural boltLexer

boolean :: Parser Bool
boolean = do{string "true"; return True} <|> do {string "false"; return False}

string' :: Parser String
string' = T.stringLiteral boltLexer

self :: Parser ()
self = do {T.symbol boltLexer "$"; return ()}

------ OPERATORS ------
equalOp :: Parser ()
equalOp = T.reservedOp boltLexer "="

addOp :: Parser ()
addOp = T.reservedOp boltLexer "+"

removeOp :: Parser ()
removeOp = T.reservedOp boltLexer "-"

orOp :: Parser ()
orOp = T.reservedOp boltLexer "|"

------ SYMBOLS ------
parens :: Parsec String u a -> Parsec String u a
parens = T.parens boltLexer

------ TOKEN GEN ------
boltDef :: LanguageDef u
boltDef = T.LanguageDef 
    { T.commentStart =    "//"
    , T.commentEnd   =    "//"
    , T.commentLine =     ""
    , T.nestedComments =  False
    , T.identStart =      upper <|> char '_'
    , T.identLetter =     idLetter
    , T.opStart =         oneOf []
    , T.opLetter =        oneOf []
    , T.reservedOpNames = ["=", "+", "-", "|"]
    , T.reservedNames =   ["true", "false"]
    , T.caseSensitive =   True
    }

boltLexer :: T.TokenParser u
boltLexer = T.makeTokenParser boltDef

idLetter = alphaNum <|> char '_'