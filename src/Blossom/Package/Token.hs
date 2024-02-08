{-# LANGUAGE OverloadedStrings #-}

module Blossom.Package.Token (
    Token(..),
) where

import Blossom.Common.Name.Ident (Ident)
import Data.ByteString (ByteString, unpack)
import Blossom.Package.Version (Version)
import Prettyprinter (Pretty(pretty), brackets)


data Token
    = TokExeHeader Ident
    | TokLibHeader Ident
    | TokPkgHeader Ident
    | TokString ByteString
    | TokVersionVal Version
    | TokComma
    | TokEq
    | TokLBracket
    | TokRBracket
    | TokSourceDirs
    | TokModules
    | TokName
    | TokVersion
    | TokMain
    | TokPath
    | TokBool Bool
    | TokEnd
    deriving (Show, Eq)

instance Pretty Token where
    pretty (TokExeHeader ident) = brackets $ "executable." <> pretty ident
    pretty (TokLibHeader ident) = brackets $ "library." <> pretty ident
    pretty (TokPkgHeader ident) = brackets $ "subpackage." <> pretty ident
    pretty (TokString str) = "\"" <> pretty (unpack str) <> "\""
    pretty (TokVersionVal vers) = pretty vers
    pretty TokComma = ","
    pretty TokEq = "="
    pretty TokLBracket = "["
    pretty TokRBracket = "]"
    pretty TokSourceDirs = "source-dirs"
    pretty TokModules = "modules"
    pretty TokName = "name"
    pretty TokVersion = "version"
    pretty TokMain = "main"
    pretty TokPath = "path"
    pretty (TokBool True) = "true"
    pretty (TokBool False) = "false"
    pretty TokEnd = "end-of-input"
