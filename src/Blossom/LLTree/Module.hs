module Blossom.LLTree.Module (
    Import(..),
    ModuleLLT(..),
    Name,
) where

import Blossom.Common.Name (Name)
import Blossom.LLTree.Definition (Definition)
import Data.ByteString (ByteString)


-- | Represents a lower-level version of the AST
-- for interfacing with the backend. Has renamed
-- variables, closures, ...
data ModuleLLT = ModuleLLT {
    moduleName :: ByteString,
    modulePath :: FilePath,
    moduleImports :: [Import],
    moduleDefs :: [Definition]
    }

newtype Import = Import FilePath
