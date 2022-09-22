module Blossom.Typing.Type (
    Type(..),
) where

import Blossom.Common.Name (Name)

infixr 9 :->

data Type
    = TypeCon {
        typeName :: Name,
        typeArgs :: [Type]
    }
    -- TODO: Type variables
    | Type :-> Type
    deriving (Show, Eq)
