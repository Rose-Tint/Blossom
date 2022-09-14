module Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevelExpr(..),
    Function(..),
    Param(..),
    Expr(..),
    Data(..),
    Constructor(..),
) where

import qualified LLVM.AST as LLVM


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import LLVM.Name

data TopLevelExpr
    = FuncDef Function
    | DataDef Data

data Function = Function {
    funcName :: LLVM.Name,
    funcParams :: [Param],
    funcBody :: Expr
    }

data Param = Param {
    paramName :: Maybe LLVM.Name
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
