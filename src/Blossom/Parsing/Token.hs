module Blossom.Parsing.Token (
    Token(..),
    mkSmallId,
    mkBigId,
) where

import qualified Data.ByteString.Lazy as BS

import Blossom.Common.Name (Name, mkName)


data Token
    = TokInteger Int
    | TokFloat Double
    | TokString BS.ByteString
    | TokOperator Name
    | TokSmallId Name
    | TokBigId Name
    | TokSemi
    | TokColon
    | TokDoubleColon
    | TokArrow
    | TokEqArrow
    | TokLParen
    | TokRParen
    | TokLBrace
    | TokRBrace
    | TokImport
    | TokFunc
    | TokData
    | TokIf
    | TokThen
    | TokElse
    | TokEnd
    deriving (Show, Eq)


-- | Creates a `@TokSmallId@` using a `@String@`
mkSmallId :: String -> Token
mkSmallId = TokSmallId . mkName

-- | Creates a `@TokBigId@` using a `@String@`
mkBigId :: String -> Token
mkBigId = TokBigId . mkName

