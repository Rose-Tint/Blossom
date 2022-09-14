module Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevel(..),
    Function(..),
    Param(..),
    Data(..),
    Constructor(..),
) where

import qualified LLVM.AST as LLVM


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import LLVM.Name

data TopLevel
    = FuncDef Function
    | DataDef Data

data Function = Function {
    funcName :: LLVM.Name,
    funcParams :: [Param],
    funcBody :: FunctionBody
    }

data Param = Param {
    paramName :: LLVM.Name
    -- paramType ::
    }

data Data = Data {
    dataName :: LLVM.Name,
    dataCtors :: [Constructor],
    }

data Constructor = Constructor {
    ctorName :: LLVM.Name,
    ctorTypes :: [LLVM.Name]
    }
