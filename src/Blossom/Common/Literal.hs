module Blossom.Common.Literal (
    Literal(..),
) where

import Data.ByteString (ByteString)


data Literal
    = IntLit Integer
    | FloatLit Double
    | CharLit Char
    | StringLit ByteString
    deriving (Show, Eq)
