{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module Parser
    (
        assignment
    ) where

import Text.Parsec

import qualified Lexer as L
import qualified Ast as A




assignment :: Parsec String u A.Assignment
assignment = do
    id <- L.identifier
    _ <- L.equalOp
    val <- value
    return (A.Assignment id val)

value = L.integer