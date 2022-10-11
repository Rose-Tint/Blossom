module Blossom.Parsing.Token (
    Token(..),
) where

import Blossom.Common.Name (Ident)
import Data.ByteString.Lazy (ByteString)


data Token
    = TokInteger Integer
    | TokFloat Double
    | TokString ByteString
    | TokChar Char
    | TokOperator Ident
    | TokSmallId Ident
    | TokBigId Ident
    | TokSemi
    | TokColon
    | TokArrow
    | TokEquals
    | TokEqArrow
    | TokLParen
    | TokRParen
    | TokLBrace
    | TokRBrace
    | TokBackslash
    | TokImport
    | TokFunc
    | TokData
    | TokMatch
    | TokEnd
    deriving (Show, Eq)
