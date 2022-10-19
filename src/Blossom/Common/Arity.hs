{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Blossom.Common.Arity (
    Arity(..),
    HasArity(arityOf),
) where

import Prettyprinter (Pretty(pretty))


-- | Represents the number of arguments something can take.
newtype Arity = Arity { unArity :: Word }
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

class HasArity a where
    arityOf :: a -> Arity

instance Pretty Arity where
    pretty = pretty . unArity
