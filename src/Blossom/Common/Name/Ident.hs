module Blossom.Common.Name.Ident (
    Ident(..),
    fromQualified,
    testIdent,
) where

import Blossom.Common.Name.Module (ModuleName(MdlName))
import Blossom.Common.Source (SourceLoc, HasLoc(getLoc), testLoc)
import Data.ByteString.Char8 as BS (
    ByteString,
    unpack,
    dropWhile,
    dropWhileEnd,
    spanEnd,
    )
import Data.Ord (comparing)
import Data.String (fromString)
import Prettyprinter (Pretty(pretty))

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

instance Pretty Ident where
    pretty (Ident iden _loc) = pretty (unpack iden)

instance HasLoc Ident where
    getLoc = identLoc

-- | Separates the qualifier from the actual identifier.
fromQualified :: Ident -> (ModuleName, Ident)
fromQualified (Ident iden loc) =
    let stripColons = BS.dropWhile (== ':') . dropWhileEnd (== ':')
        (mdl, ident) = spanEnd (/= ':') iden
        mdl' = stripColons mdl
        ident' = stripColons ident
    in (MdlName mdl', Ident ident' loc)

testIdent :: String -> Ident
testIdent = (`Ident` testLoc) . fromString
