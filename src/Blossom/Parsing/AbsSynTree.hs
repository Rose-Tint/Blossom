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

import Blossom.Typing.Type
import Data.Int (Int64)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import LLVM.Name

data TopLevelExpr
    = FuncDef Function
    | DataDef Data

data Function = Function {
    -- | Name of the function.
    funcName :: LLVM.Name,
    -- | Parameters that the function takes.
    funcParams :: [Param],
    -- | Return type of the function, written as the last
    -- type in a type signature
    funcReturn :: Type,
    -- | The body of the function.
    funcBody :: Expr
    }

data Param = Param {
    paramName :: LLVM.Name,
    paramType :: Type
    }

data Expr
    = VarExpr LLVM.Name
    | IntExpr Int64
    | FloatExpr Double
    | FuncApp Expr Expr
    | Lambda {
        exprParams :: [Param],
        exprReturn :: Type,
        exprBody :: Expr
    }
    | Match Expr [(Expr, Expr)]

data Data = Data {
    dataName :: LLVM.Name,
    dataCtors :: [Constructor]
    }

data Constructor = Constructor {
    ctorName :: LLVM.Name,
    ctorParams :: [Param]
    }
