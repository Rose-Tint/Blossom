module Blossom.Package (
    Package(..),
    getMainPackage
) where

import Blossom.Package.Package
-- import Blossom.Package.Parser (parsePackageFile)
-- import System.Directory (findFile)


getMainPackage :: IO Package
getMainPackage = do
    return undefined
