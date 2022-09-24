module Blossom.LLTree.Module (
    Name,
    ModuleLLT(..),
    Import(..),
    Definition(..),
    CtorDef(..),
    ClosureType(..),
    Closure(..),
    FuncPtr(..),
    Type(..),
    Header(..),
    Info(..),
    Arity(..),
    FuncApp(..),
    PartialApp(..),
    Entry,
    Body,
) where

import Data.ByteString (ByteString)
import Blossom.LLTree.Name (Name)
import Blossom.LLTree.Closure (
    FuncPtr(..),
    Closure(..),
    ClosureType(..),
    )
import Blossom.LLTree.Type (Type(..))
import Blossom.LLTree.Info (
    Info(..),
    Header(..),
    Entry
    )


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

data Definition
    = FuncDef {
        funcClosure :: Closure,
        funcInfo :: Info,
        funcDef :: Body
    }
    | DataDef {
        dataName :: Name,
        dataCtors :: [CtorDef]
    }

data CtorDef = CtorDef {
    ctorName :: Name,
    ctorInfo :: Info
    }

newtype Arity = Arity Word

data FuncApp = FAP {
    fapHeader :: Header,
    fapArity :: Arity,
    fapClosure :: Closure,
    fapArgs :: [Name]
    }

data PartialApp = PAP {
    papHeader :: Header,
    papArity :: Arity,
    papClosure :: Closure,
    papArgs :: [Name] -- args that have been applied,
    }

data Body
