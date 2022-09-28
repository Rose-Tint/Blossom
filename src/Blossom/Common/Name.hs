{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Name (
    Iden,
    Name(..),
    toDisplay,
    fromQualified,
    mkIden,
) where

import Data.String (fromString)
import Data.ByteString (ByteString, append)
import Data.ByteString.Char8 (spanEnd, dropWhileEnd)


type Iden = ByteString

data Name = Name {
    -- | The name of the module the identifier originates from.
    nameModule :: ByteString,
    -- | The un-qualified name of the identifier that appears in
    -- the source code
    nameIden :: ByteString
    -- -- | A number that is unique to the thing this name represents.
    -- nameUnique :: Word
    }
    deriving (Show, Eq, Ord)


fromQualified :: ByteString -> Name
fromQualified bs = Name mdl' iden
    where
        (mdl, iden) = spanEnd (/= ':') bs
        mdl' = dropWhileEnd (/= ':') mdl

toDisplay :: Name -> ByteString
toDisplay (Name mdl iden) = append mdl $ append "_" iden

mkIden :: String -> Iden
mkIden = fromString
