module Blossom.Parsing.Token (
    Token(..),
    mkSmallId,
    mkBigId,
    mkOperator,
) where

import Data.Int (Int64)
import Data.ByteString.Lazy (ByteString)

import Blossom.Common.Name (Name, mkName)


data Token
    = TokInteger Int64
    | TokFloat Double
    | TokString ByteString
    | TokChar Char
    | TokOperator Name
    | TokSmallId Name
    | TokBigId Name
    | TokSemi
    | TokColon
    | TokDoubleColon
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


-- | Creates a `@TokSmallId@` using a `@String@`
mkSmallId :: String -> Token
mkSmallId = TokSmallId . mkName

-- | Creates a `@TokBigId@` using a `@String@`
mkBigId :: String -> Token
mkBigId = TokBigId . mkName

-- | Creates a `@TokOperator@` using a `@String@`
mkOperator :: String -> Token
mkOperator = TokOperator . mkName
