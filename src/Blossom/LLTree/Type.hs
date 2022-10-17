{-# LANGUAGE OverloadedStrings #-}

module Blossom.LLTree.Type (
    Type(..),
    Typed(..),
    Var,
    fromASTType,
) where

import Blossom.Common.Name (Ident(Ident), Name)
import qualified Blossom.Parsing.AbsSynTree as AST (Type(..))


data Type
    = I64
    | I32
    | I16
    | I8
    | F64
    | F32
    | Char
    | String
    | Unit
    | Pointer Type
    | Struct [Type]
    | FuncType Type [Type]
    deriving (Show, Eq)

data Typed a = Typed a Type

type Var = Typed Name


fromASTType :: AST.Type -> Type
fromASTType (AST.TypeCon (Ident "I64" _) args) = prim I64 args
fromASTType (AST.TypeCon (Ident "I32" _) args) = prim I32 args
fromASTType (AST.TypeCon (Ident "I16" _) args) = prim I16 args
fromASTType (AST.TypeCon (Ident "I8" _) args) = prim I8 args
fromASTType (AST.TypeCon (Ident "Boolean" _) args) = prim I8 args
fromASTType (AST.TypeCon (Ident "F64" _) args) = prim F64 args
fromASTType (AST.TypeCon (Ident "F32" _) args) = prim F32 args
fromASTType (AST.TypeCon (Ident "Char" _) args) = prim Char args
fromASTType (AST.TypeCon (Ident "String" _) args) = prim String args
fromASTType (AST.TypeCon (Ident "Unit" _) args) = prim Unit args
fromASTType (AST.TypeCon _name args) = Struct (map fromASTType args)
fromASTType (t1 AST.:-> t2) = case fromASTType t2 of
    FuncType ret pars -> FuncType ret (t1':pars)
    t2' -> FuncType t2' [t1']
    where
    t1' = fromASTType t1

prim :: Type -> [AST.Type] -> Type
prim typ args =
    if null args then
        typ
    else
        error "unexpected argument(s) to primitive type constructor"
