{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Blossom.Common.Arity (
    Arity(..),
    HasArity(arityOf),
) where


-- | Represents the number of arguments something can take.
newtype Arity = Arity Word
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

class HasArity a where
    arityOf :: a -> Arity
