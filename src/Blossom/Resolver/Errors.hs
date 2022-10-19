{-# LANGUAGE OverloadedStrings #-}

module Blossom.Resolver.Errors (
    ResolverError(..),
) where

import Blossom.Common.Name (Name, Ident)
import Blossom.Common.Source (SourceLoc, HasLoc(getLoc), unknownLoc)
import Blossom.Parsing.AbsSynTree (Params, AbsType)
import Prettyprinter (Pretty(pretty), (<+>), line, nest, dot)
import Blossom.Common.Arity (HasArity(arityOf))


data ResolverError
    = TODO SourceLoc String
    | DeclNoDef Name
    | MultipleDefs Name
    | TooManyParams (Params Ident) AbsType
    | InternalError String

instance HasLoc ResolverError where
    getLoc (TODO loc _msg) = loc
    getLoc (DeclNoDef name) = getLoc name
    getLoc (MultipleDefs name) = getLoc name
    getLoc (TooManyParams params _typ) = getLoc params
    getLoc InternalError{} = unknownLoc "INTERNAL ISSUE"

instance Pretty ResolverError where
    pretty err = pretty loc <+> "Error:" <> line <> nest 4 message
        where
            loc = getLoc err
            message = case err of
                TODO _loc msg -> "TODO:" <+> pretty msg
                DeclNoDef name -> pretty name <+> "declared but not defined"
                MultipleDefs name ->
                    "Multiple definitions of" <+> pretty name <> dot <> line
                    <> "This is not supported yet :("
                TooManyParams params typ ->
                    "Too many parameters for expected type" <> line <> nest 4 (
                    "Expected an arity of" <+> pretty (arityOf typ) <+>
                    ", but got" <+> pretty (length params) <+> "parameters"
                    )
                InternalError msg -> "[INTERNAL ERROR]" <+> pretty msg
