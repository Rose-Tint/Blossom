module Blossom.Package.Component (
    module Cmpnt,
    Component(..),
) where

import Blossom.Package.Component.Executable as Cmpnt
import Blossom.Package.Component.GenOpts as Cmpnt
import Blossom.Package.Component.Library as Cmpnt
import Blossom.Package.Component.Subpackage as Cmpnt


-- | Represents a library, executable, or subpackage to build.
data Component
    = Library Library
    | Executable Executable
    | Subpackage Subpackage

instance IsCmpnt Component where
    getGenOpts (Library lib) = getGenOpts lib
    getGenOpts (Executable exe) = getGenOpts exe
    getGenOpts (Subpackage sub) = getGenOpts sub
    modifyGenOpts f (Library lib) = Library $ modifyGenOpts f lib
    modifyGenOpts f (Executable exe) = Executable $ modifyGenOpts f exe
    modifyGenOpts f (Subpackage sub) = Subpackage $ modifyGenOpts f sub
