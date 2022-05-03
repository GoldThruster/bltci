module Main where

import Text.Parsec
import Parser
import qualified Ast
import GHC.Base (IO(..))

main :: IO ()
main = do
    res <- parseLine
    case res of
        Left err -> print err
        Right x -> print x

parseLine :: IO (Either ParseError Ast.Expression)
parseLine = do
    Text.Parsec.parse expr "" <$> getLine