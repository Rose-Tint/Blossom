module Blossom.Typing.Type (
    Type(..),
) where

import qualified LLVM.AST as LLVM


data Type
    = TypeCon {
        typeName :: LLVM.Name,
        typeArgs :: [Type]
    }
    -- TODO: Type variables
    | Type :-> Type
