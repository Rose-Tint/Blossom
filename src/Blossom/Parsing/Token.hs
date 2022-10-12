{-# LANGUAGE OverloadedStrings #-}

module Blossom.Parsing.Token (
    Token(..),
) where

import Blossom.Common.Name (Ident)
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import Prettyprinter (
    Pretty(pretty),
    unsafeViaShow,
    backslash,
    colon,
    equals,
    semi, lparen, rparen, lbrace, rbrace
    )


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

instance Pretty Token where
    pretty (TokInteger n) = pretty n
    pretty (TokFloat n) = pretty n
    pretty (TokString bs) = pretty (unpack bs)
    pretty (TokChar ch) = unsafeViaShow ch
    pretty (TokOperator ident) = pretty ident
    pretty (TokSmallId ident) = pretty ident
    pretty (TokBigId ident) = pretty ident
    pretty TokSemi = semi
    pretty TokColon = colon
    pretty TokArrow = "->"
    pretty TokEquals = equals
    pretty TokEqArrow = "=>"
    pretty TokLParen = lparen
    pretty TokRParen = rparen
    pretty TokLBrace = lbrace
    pretty TokRBrace = rbrace
    pretty TokBackslash = backslash
    pretty TokImport = "import"
    pretty TokFunc = "func"
    pretty TokData = "data"
    pretty TokMatch = "match"
    pretty TokEnd = "end-of-input"
