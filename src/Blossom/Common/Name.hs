{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Blossom.Common.Name (
    Ident(..),
    ModuleName,
    Name(..),
    NameType(..),
    catIdent,
    display,
    external,
    internal,
    stripColons,
    testIdent,
) where

import Blossom.Common.Source (SourceLoc, HasLoc(getLoc), testLoc)
import Data.ByteString (ByteString, append)
import qualified Data.ByteString.Char8 as BS (concat, length, dropWhile)
import Data.ByteString.Char8 (unpack, spanEnd, dropWhileEnd)
import Data.Ord (comparing)
import Data.String (fromString)
import Prettyprinter (Pretty(pretty))


type ModuleName = ByteString

-- | The name as it appears in the source code, along with it's location
data Ident
    = Ident {
        identBS :: ByteString,
        identLoc :: SourceLoc
    }
    deriving (Show)

instance Eq Ident where
    Ident id1 _loc1 == Ident id2 _loc2 = id1 == id2

instance Ord Ident where
    compare = comparing identBS

data NameType
    -- | A name that has been determined to be from a different module, or is
    -- qualified.
    = External ModuleName
    -- | A name that the user has defined in the current module.
    | Internal
    deriving (Show, Eq)

data Name
    = Name {
        -- | The name of the module the identifier originates from.
        nameType :: NameType,
        nameBS :: ByteString,
        -- | The un-qualified name of the identifier that appears in
        -- the source code
        nameLoc :: SourceLoc
    }
    deriving (Show)

instance Eq Name where
    Name nt1 id1 _loc1 == Name nt2 id2 _loc2 = nt1 == nt2 && id1 == id2

-- | Creates an 'external' name (i.e. a name that was defined in a different
-- module). If the given `@Ident@` is qualified, the qualifier *must* equal the
-- given module name.
external :: ModuleName -> Ident -> Name
external mdl ident
    | BS.length qual > 0 && qual /= mdl = error $
        "Qualifier does not match expected module name.\n\
        \    Expected: " ++ unpack mdl ++ "\n\
        \     But got: " ++ unpack qual
    | otherwise = Name (External mdl) iden loc
    where
        (qual, Ident iden loc) = fromQualified ident

-- | Creates an 'internal' name (i.e. a name that was defined in the current
-- module). If the given `@Ident@` is qualified, `@internal@` will actually
-- create an external one, with the qualifier as the module name.
internal :: Ident -> Name
internal ident
    | BS.length qual == 0 = Name Internal iden loc
    | otherwise = Name (External qual) iden loc
    where
        (qual, Ident iden loc) = fromQualified ident

stripColons :: ModuleName -> ModuleName
stripColons = BS.dropWhile (== ':') . dropWhileEnd (== ':')

fromQualified :: Ident -> (ModuleName, Ident)
fromQualified (Ident name loc) =
    let (mdl, name') = spanEnd (/= ':') name
    in (stripColons mdl, Ident (stripColons name') loc)

display :: Name -> ByteString
display (Name (External qual) name _loc) = BS.concat [qual, "::", name]
display (Name Internal name _loc) = name

-- | `@catIdent@ name suffix` appends `suffix` to the identifier
-- part of `name`
catIdent :: Name -> ByteString -> Name
catIdent name@Name{nameBS=iden} sfx = name{ nameBS = append iden sfx }

testIdent :: String -> Ident
testIdent iden = Ident (fromString iden) testLoc

instance Pretty ModuleName where
    pretty = pretty . unpack

instance Pretty Ident where
    pretty (Ident iden _loc) = pretty (unpack iden :: String)

instance Pretty NameType where
    pretty (External mdl) = "external (" <> pretty (unpack mdl) <> ")"
    pretty Internal = "internal"

instance Pretty Name where
    pretty = pretty . unpack . display

instance HasLoc Ident where
    getLoc = identLoc

instance HasLoc Name where
    getLoc = nameLoc
