module Main where

import Text.Parsec ( parse, ParseError )
import Evaluator
import Parser
import Control.Monad
import qualified Data.Maybe as Maybe
import qualified Ast


main :: IO ()
main = loop emptyState


loop :: Context -> IO ()
loop state = do
        input <- getLine
        execute input state
            
execute :: String -> Context -> IO ()
execute ":quit" _ = return ()
execute ":list" state = do
    print state
    loop state
execute input state =
    let res = parseLine input
    in case res of
        Left err -> do
            print err
            loop state
        Right x -> do
            let evalued = eval x state
                newState = Evaluator.updateState state evalued
            putStrLn ("AST : " ++ show x ++ "\n" ++ "EVAL: " ++ show evalued)
            loop newState


parseLine :: String -> Either ParseError Ast.Expression
parseLine = do
    Text.Parsec.parse expr ""


