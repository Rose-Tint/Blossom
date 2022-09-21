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

import qualified Data.ByteString.Lazy as BS

import Blossom.Typing.Type (Type)
import Blossom.Common.Name (Name)
import Data.Int (Int64)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import Name

data TopLevelExpr
    = FuncDef Function
    | DataDef Data

data Function = Function {
    -- | Name of the function.
    funcName :: Name,
    -- | Parameters that the function takes.
    funcParams :: [Param],
    -- | Return type of the function, written as the last
    -- type in a type signature
    funcReturn :: Type,
    -- | The body of the function.
    funcBody :: Expr
    }

data Param = Param {
    paramName :: Name,
    paramType :: Type
    }

data Expr
    = VarExpr Name
    | IntExpr Int64
    | FloatExpr Double
    | StringExpr BS.ByteString
    | FuncApp Expr Expr
    | Lambda {
        exprParams :: [Param],
        exprReturn :: Type,
        exprBody :: Expr
    }
    | Match Expr [(Expr, Expr)]

data Data = Data {
    dataName :: Name,
    dataCtors :: [Constructor]
    }

data Constructor = Constructor {
    ctorName :: Name,
    ctorParams :: [Param]
    }
