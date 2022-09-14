module Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevel(..),
    Function(..),
    Param(..),
    Expr(..),
    Data(..),
    Constructor(..),
) where

import qualified LLVM.AST as LLVM


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevel]
    }

newtype Import = Import LLVM.Name

data TopLevel
    = FuncDef Function
    | DataDef Data

data Function = Function {
    funcName :: LLVM.Name,
    funcParams :: [Param],
    funcBody :: Expr
    }

data Param = Param {
    paramName :: LLVM.Name
    -- paramType ::
    }

data Expr = Expr

data Data = Data {
    dataName :: LLVM.Name,
    dataCtors :: [Constructor]
    }

data Constructor = Constructor {
    ctorName :: LLVM.Name,
    ctorTypes :: [LLVM.Name]
    }
