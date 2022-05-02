module Lexer
    (   identifier
    ,   integer
    ,   equalOp
    ) where

import Text.Parsec
import Text.Parsec.Language
import qualified Text.Parsec.Token as T



------ WORDS ------
identifier :: Parsec String u String
identifier = T.identifier boltLexer

------ LITERALS ------
integer :: Parsec String u Integer
integer = T.integer boltLexer
equalOp :: Parsec String u ()

equalOp = T.reservedOp boltLexer "="

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
    , T.opLetter = oneOf "="
    , T.reservedOpNames = ["="]
    , T.reservedNames = []
    , T.caseSensitive = True
    }

boltLexer :: T.TokenParser u
boltLexer = T.makeTokenParser boltDef