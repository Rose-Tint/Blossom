module Blossom.Common.Name.Module (
    ModuleName(..),
) where

import Data.ByteString (ByteString, unpack)
import Prettyprinter (Pretty(pretty))
import Data.String (IsString(fromString))


newtype ModuleName = MdlName { unMdlName :: ByteString }
    deriving (Show, Eq)

instance Pretty ModuleName where
    pretty = pretty . unpack . unMdlName

instance Semigroup ModuleName where
    MdlName mdl1 <> MdlName mdl2 = MdlName $ mdl1 <> fromString "::" <> mdl2

instance Monoid ModuleName where
    mempty = MdlName mempty

instance IsString ModuleName where
    fromString = MdlName . fromString
