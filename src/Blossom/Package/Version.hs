module Blossom.Package.Version (
    Version(..),
    newVersion,
) where
import Prettyprinter (Pretty(pretty), dot)

data Version
    = Version {
        versionMajor :: Int,
        versionMinor :: Int,
        versionPatch :: Int
    }
    deriving (Show, Eq, Ord)

newVersion :: Version
newVersion = Version 1 0 0

instance Pretty Version where
    pretty (Version major minor patch)
        = pretty major <> dot <> pretty minor <> dot <> pretty patch
