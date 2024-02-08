module Blossom.Package.Component.Library (
    Library(..),
    newLibrary,
) where

import Blossom.Package.Component.GenOpts (GenOpts, IsCmpnt(..), newGenOpts)


newtype Library
    = Lib {
        libGenOpts :: GenOpts
    }

newLibrary :: Library
newLibrary = Lib {
    libGenOpts = newGenOpts
    }

instance IsCmpnt Library where
    getGenOpts = libGenOpts
    modifyGenOpts f lib = lib { libGenOpts = f (libGenOpts lib) }
