module Blossom.Parsing.Token (
    Token(..),
    mkBigId,
    mkOperator,
    mkSmallId,
) where

import Blossom.Common.Name (Iden, mkIden)
import Data.ByteString.Lazy (ByteString)


data Token
    = TokInteger Integer
    | TokFloat Double
    | TokString ByteString
    | TokChar Char
    | TokOperator Iden
    | TokSmallId Iden
    | TokBigId Iden
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


-- | Creates a `@TokSmallId@` using a `@String@`
mkSmallId :: String -> Token
mkSmallId = TokSmallId . mkIden

-- | Creates a `@TokBigId@` using a `@String@`
mkBigId :: String -> Token
mkBigId = TokBigId . mkIden

-- | Creates a `@TokOperator@` using a `@String@`
mkOperator :: String -> Token
mkOperator = TokOperator . mkIden
