module Blossom.Package.Target (
    Target(..),
) where

import Blossom.Common.Name.Module (ModuleName)


data Target
    = TargetModule ModuleName
    | TargetFile FilePath
