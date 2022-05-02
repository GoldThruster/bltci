module Ast 
    (
        Assignment (..)
    ) where

data Assignment = Assignment { identifier :: String, value :: Integer } deriving Show