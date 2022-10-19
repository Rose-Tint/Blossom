module Blossom.Typing.Type (
    Type(..),
    Kind(..),
    HasType(..),
) where

import Blossom.Common.Arity (HasArity(..))
import Blossom.Common.Name (Name)
import Blossom.Common.Source (HasLoc(..), mergeLocs)


data Type
    -- | Type constructors (i.e. `I32`, `Map`, `String`)
    = TypeCon Name
    -- | A type-variable
    | TypeVar Name
    -- | Application of types (i.e. `Array I32`, `Map I32 v`)
    | TypeApp Type Type
    -- | Functional application of types (i.e. `String -> I32`)
    | Type :-> Type
    deriving (Show, Eq)

data Kind
    -- | "*"
    = Nullary
    | Kind ::-> Kind

class HasType a where
    typeOf :: a -> Type

instance HasLoc Type where
    getLoc (TypeCon name) = getLoc name
    getLoc (TypeVar name) = getLoc name
    getLoc (TypeApp con arg) = mergeLocs con arg
    getLoc (t1 :-> t2) = mergeLocs t1 t2

instance HasType Type where
    typeOf = id

instance HasArity Type where
    arityOf (_t1 :-> t2) = 1 + arityOf t2
    arityOf _ = 0

instance HasArity Kind where
    arityOf Nullary = 0
    arityOf (_k1 ::-> k2) = 1 + arityOf k2
