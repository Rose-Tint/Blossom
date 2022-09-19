module Blossom.Typing.Type (
    Type(..),
) where

import Blossom.Common.Name (Name)


data Type
    = TypeCon {
        typeName :: Name,
        typeArgs :: [Type]
    }
    -- TODO: Type variables
    | Type :-> Type
