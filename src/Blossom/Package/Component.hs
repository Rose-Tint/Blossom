module Blossom.Package.Component (
    Component(..),
    newLibrary,
    newExecutable,
) where

import Blossom.Package.Target (Target)
import Data.ByteString (ByteString)


-- | Represents a library, executable, or subpackage to build.
data Component
    = Library {
        cmpntName :: ByteString,
        cmpntTargets :: [Target],
        cmpntSourceDirs :: [FilePath],
        cmpntDepends :: [ByteString]
    }
    | Executable {
        cmpntName :: ByteString,
        cmpntTargets :: [Target],
        cmpntSourceDirs :: [FilePath],
        cmpntDepends :: [ByteString],
        -- | Path to a module that exports the `main` function that gets
        -- executed.
        exeMain :: FilePath
    }

newLibrary :: Component
newLibrary = Library {
    cmpntName = error "'name' is a required field",
    cmpntTargets = [],
    cmpntSourceDirs = [],
    cmpntDepends = []
    }

newExecutable :: Component
newExecutable = Executable {
    cmpntName = error "'name' is a required field",
    cmpntTargets = [],
    cmpntSourceDirs = [],
    cmpntDepends = [],
    exeMain = error "'main' is a required field"
    }
