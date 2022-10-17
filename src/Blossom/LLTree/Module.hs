module Blossom.LLTree.Module (
    Import(..),
    ModuleLLT(..)
) where

import Blossom.Common.Name (ModuleName)
import Blossom.LLTree.Definition (Definition)


-- | Represents a lower-level version of the AST
-- for interfacing with the backend. Has renamed
-- variables, closures, ...
data ModuleLLT = ModuleLLT {
    moduleName :: ModuleName,
    modulePath :: FilePath,
    moduleImports :: [Import],
    moduleDefs :: [Definition]
    }

newtype Import = Import FilePath
