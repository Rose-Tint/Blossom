{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Name (
    Iden,
    Name(..),
    catIdent,
    display,
    fromQualified,
    mkIden,
) where

import Data.ByteString (ByteString, append)
import qualified Data.ByteString as BS (concat)
import Data.ByteString.Char8 (unpack)
import Data.String (fromString)


type Iden = ByteString

data Name = Name {
    -- | The name of the module the identifier originates from.
    nameModule :: Iden,
    -- | The un-qualified name of the identifier that appears in
    -- the source code
    nameIden :: Iden
    -- -- | A number that is unique to the thing this name represents.
    -- nameUnique :: Word
    }
    deriving (Show, Eq, Ord)


fromQualified :: ByteString -> Name
fromQualified = go (Name "" "") . unpack
    where
        go :: Name -> String -> Name
        go (Name _mdl "") "" = error "fromQualified: empty identifier"
        go name "" = name
        go (Name "" _iden) "::" = error
            "fromQualified: illegal trailing \"::\""
        -- go (Name "" _iden) (':':':':_) = error
        --     "fromQualified: illegal leading \"::\""
        go name@(Name mdl iden) (':':':':rest) = case go name rest of
            Name "" iden' -> Name mdl iden'
            Name mdl' "" -> Name mdl' iden
            name' -> name'
        go (Name "" iden) str =
            let (idenStr, rest) = span (/= ':') str
                iden' = fromString idenStr
            in go (Name iden iden') rest
        go (Name mdl iden) str =
            let (idenStr, rest) = span (/= ':') str
                mdl' = BS.concat [mdl, "_", iden]
                iden' = fromString idenStr
            in go (Name mdl' iden') rest

display :: Name -> ByteString
display (Name mdl iden) = BS.concat [mdl, "_", iden]

mkIden :: String -> Iden
mkIden = fromString

-- | `@catIdent@ name suffix` appends `suffix` to the identifier
-- part of `name`
catIdent :: Name -> ByteString -> Name
catIdent (Name mdl iden) = Name mdl . append iden
