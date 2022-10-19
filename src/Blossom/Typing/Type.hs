module Blossom.Typing.Type (
    Type(..),
    Kind(..),
) where

import Blossom.Common.Arity (HasArity(..))
import Blossom.Common.Source (HasLoc(..), mergeLocs)
import Prettyprinter (Pretty(pretty), (<+>))

-- | Represents a type, parametric in the type used for names. This allows for
-- flexibility, such as using the this type for the syntax tree, both before
-- *and* after renaming.
data Type name
    -- | Type constructors (i.e. `I32`, `Map`, `String`)
    = TypeCon name
    -- | A type-variable
    | TypeVar name
    -- | Application of types (i.e. `Array I32`, `Map I32 v`)
    | TypeApp (Type name) (Type name)
    -- | Functional application of types (i.e. `String -> I32`)
    | (Type name) :-> (Type name)
    deriving (Show, Eq)

infixr 9 :->

data Kind
    -- | "*"
    = Nullary
    | Kind ::-> Kind

infixr 9 ::->

instance HasLoc name => HasLoc (Type name) where
    getLoc (TypeCon name) = getLoc name
    getLoc (TypeVar name) = getLoc name
    getLoc (TypeApp con arg) = mergeLocs con arg
    getLoc (t1 :-> t2) = mergeLocs t1 t2

instance HasArity (Type name) where
    arityOf (_t1 :-> t2) = 1 + arityOf t2
    arityOf _ = 0

instance HasArity Kind where
    arityOf Nullary = 0
    arityOf (_k1 ::-> k2) = 1 + arityOf k2

instance Pretty name => Pretty (Type name) where
    pretty (TypeCon name) = pretty name
    pretty (TypeVar name) = pretty name
    pretty (TypeApp con arg) = pretty con <+> pretty arg
    pretty (t1 :-> t2) = pretty t1 <+> pretty "->" <+> pretty t2
