module Blossom.Package.Component.Subpackage (
    Subpackage(..),
    newSubpackage,
) where

import Blossom.Package.Component.GenOpts (GenOpts, IsCmpnt(..), newGenOpts)


data Subpackage
    = Sub {
        subGenOpts :: GenOpts,
        -- | Path to the directory containing the "<package-name>.toml" file.
        subPath :: FilePath
    }

newSubpackage :: Subpackage
newSubpackage = Sub {
    subGenOpts = newGenOpts,
    subPath = error "'path' is a required field"
    }

instance IsCmpnt Subpackage where
    getGenOpts = subGenOpts
    modifyGenOpts f sub = sub { subGenOpts = f (subGenOpts sub) }
