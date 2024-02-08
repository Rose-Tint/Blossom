module Blossom.Package.Package (
    Package(..),
    newPackage,
) where

import Blossom.Package.Component (Component)
import Blossom.Package.Version (Version(..), newVersion)
import Data.ByteString (ByteString)


data Package
    = Package {
        pkgName :: ByteString,
        pkgVersion :: Version,
        pkgComponents :: [Component]
    }

newPackage :: Package
newPackage = Package {
    pkgName = error "'name' is a required package-property",
    pkgVersion = newVersion,
    pkgComponents = []
}
