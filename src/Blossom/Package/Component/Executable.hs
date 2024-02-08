module Blossom.Package.Component.Executable (
    Executable(..),
    newExecutable,
) where

import Blossom.Package.Component.GenOpts (GenOpts, IsCmpnt(..), newGenOpts)


data Executable
    = Exe {
        exeGenOpts :: GenOpts,
        -- | Path to a module that exports the `main` function that gets
        -- executed.
        exeMain :: FilePath
    }

newExecutable :: Executable
newExecutable = Exe {
    exeGenOpts = newGenOpts,
    exeMain = error "'main' is a required field"
    }

instance IsCmpnt Executable where
    getGenOpts = exeGenOpts
    modifyGenOpts f exe = exe { exeGenOpts = f (exeGenOpts exe) }
