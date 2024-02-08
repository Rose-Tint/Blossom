module Blossom.Package (
    Package(..),
) where

import Blossom.Package.Component (Component)
import Blossom.Package.Properties (Properties)


data Package
    = Package {
        pkgProperties :: Properties,
        pkgComponents :: [Component]
    }
