{-# LANGUAGE OverloadedStrings #-}

module Blossom.Resolver.Errors (
    ResolverError(..),
) where

import Blossom.Common.Name (Name)
import Blossom.Parsing.AbsSynTree (Params, Type, typeArity)
import Blossom.Common.Source (SourceLoc, HasLoc(getLoc), unknownLoc)
import Prettyprinter


data ResolverError
    = TODO SourceLoc String
    | DeclNoDef Name
    | MultipleDefs Name
    | TooManyParams Params Type
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
                    "Expected an arity of" <+> pretty (typeArity typ) <+>
                    ", but got" <+> pretty (length params) <+> "parameters"
                    )
                InternalError msg -> "[INTERNAL ERROR]" <+> pretty msg
