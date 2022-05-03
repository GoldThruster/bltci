module Main where

import Text.Parsec
import Parser
import Evaluator
import qualified Ast

main :: IO ()
main = do
    res <- parseLine
    case res of
        Left err -> print err
        Right x -> putStrLn ("AST: " ++ show x ++ "\n" ++ "EVAL: " ++ show (eval x))

parseLine :: IO (Either ParseError Ast.Expression)
parseLine = do
    Text.Parsec.parse expr "" <$> getLine