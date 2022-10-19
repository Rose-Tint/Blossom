module Blossom.Common.Name (
    module Blossom.Common.Name.Ident,
    module Blossom.Common.Name.Module,
    Name(..),
    NameType(..),
    affixName,
    display,
    external,
    internal,
) where

import Blossom.Common.Name.Ident
import Blossom.Common.Name.Module
import Blossom.Common.Source (SourceLoc, HasLoc(getLoc))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (concat)
import Data.ByteString.Char8 (unpack, pack)
import Prettyprinter (Pretty(pretty), (<+>), hardline, nest, parens)


-- | Describes where a name was defined
data NameType
    -- | A name that has been determined to be from a different module, or is
    -- qualified.
    = External ModuleName
    -- | A name that the user has defined in the current module.
    | Internal
    deriving (Show, Eq)

-- | A name for something. The raw name, as given by `@nameBS@`, may not
-- exactly resemble the identifier from the source code but may still give the
-- same location. This can happen because the thing that this name *represents*
-- may be *derived* from the source identifer.
data Name
    = Name {
        -- | The name of the module the identifier originates from.
        nameType :: NameType,
        -- | The un-qualified name of the identifier that appears in the source
        -- code.
        nameBS :: ByteString,
        -- | The location in the source code that this name originates from.
        nameLoc :: SourceLoc
    }
    deriving (Show)

instance Eq Name where
    Name nt1 id1 _loc1 == Name nt2 id2 _loc2 = nt1 == nt2 && id1 == id2

instance Pretty NameType where
    pretty (External mdl) = pretty "external" <+> parens (pretty mdl)
    pretty Internal = pretty "internal"

instance Pretty Name where
    pretty = pretty . unpack . display

instance HasLoc Name where
    getLoc = nameLoc

-- | Creates an 'external' name (i.e. a name that was defined in a different
-- module). If the given `@Ident@` is qualified, the qualifier *must* equal the
-- given module name.
external :: ModuleName -> Ident -> Name
external mdl ident
    | not (mdlIsEmpty qual) && qual /= mdl = error $ show $
        pretty "Qualifier does not match expected module name." <>
        nest 4 (hardline
            <> pretty "Expected:" <+> pretty mdl <> hardline
            <> pretty " But got:" <+> pretty qual
            )
    | otherwise = Name (External mdl) iden loc
    where
        (qual, Ident iden loc) = fromQualified ident

-- | Creates an 'internal' name (i.e. a name that was defined in the current
-- module). If the given `@Ident@` is qualified, `@internal@` will actually
-- create an external one, with the qualifier as the module name.
internal :: Ident -> Name
internal ident
    | mdlIsEmpty qual = Name Internal iden loc
    | otherwise = Name (External qual) iden loc
    where
        (qual, Ident iden loc) = fromQualified ident

-- | Converts a name to how it may have looked in the source code.
display :: Name -> ByteString
display (Name (External (MdlName qual)) name _loc) =
    BS.concat [qual, pack "::", name]
display (Name Internal name _loc) = name

-- | `@affixName@ name suffix` appends `suffix` to the identifier
-- part of `name`.
affixName :: Name -> ByteString -> Name
affixName name@Name{nameBS=iden} sfx = name{ nameBS = iden <> sfx }
