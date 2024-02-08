{-# LANGUAGE OverloadedStrings #-}

module Blossom.Package.Token (
    Token(..),
) where

import Data.ByteString (ByteString, unpack)
import Blossom.Package.Version (Version)
import Prettyprinter (Pretty(pretty), brackets)


data Token
    = TokExeHeader
    | TokLibHeader
    | TokString ByteString
    | TokVersion Version
    | TokComma
    | TokEq
    | TokLBracket
    | TokRBracket
    | KeySourceDirs
    | KeyTargets
    | KeyName
    | KeyVersion
    | KeyMain
    | TokEnd
    deriving (Show, Eq)

instance Pretty Token where
    pretty TokExeHeader = brackets "executable"
    pretty TokLibHeader = brackets "library"
    pretty (TokString str) = "\"" <> pretty (unpack str) <> "\""
    pretty (TokVersion vers) = pretty vers
    pretty TokComma = ","
    pretty TokEq = "="
    pretty TokLBracket = "["
    pretty TokRBracket = "]"
    pretty KeySourceDirs = "source-dirs"
    pretty KeyTargets = "modules"
    pretty KeyName = "name"
    pretty KeyVersion = "version"
    pretty KeyMain = "main"
    pretty TokEnd = "end-of-input"
