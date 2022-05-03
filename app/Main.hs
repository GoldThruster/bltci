module Main where

import Text.Parsec
import Parser

main :: IO ()
main = do
    line <- getLine
    case parse expr "" line of
            Left err  -> print err
            Right xs  -> print xs
