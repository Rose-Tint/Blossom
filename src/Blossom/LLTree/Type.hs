module Blossom.LLTree.Type (
    Type(..),
) where


data Type
    = I64
    | I32
    | I16
    | I8
    | U64
    | U32
    | U16
    | U8
    | Char
    | String
    | Pointer Type
    | Struct [Type]
