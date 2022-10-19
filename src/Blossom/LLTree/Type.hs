module Blossom.LLTree.Type (
    Type(..),
    Typed(..),
    Var,
) where

import Blossom.Common.Name (Name)


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
