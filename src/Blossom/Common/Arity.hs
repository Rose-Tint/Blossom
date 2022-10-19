module Blossom.Common.Arity (
    Arity,
    HasArity(arityOf),
) where


-- | Represents the number of arguments something can take.
type Arity = Word

class HasArity a where
    arityOf :: a -> Arity
