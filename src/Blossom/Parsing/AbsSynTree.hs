module Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevelExpr(..),
    Function(..),
    Param(..),
    Expr(..),
    Data(..),
    Constructor(..),
    mkLambda,
) where

import qualified LLVM.AST as LLVM

import Blossom.Typing.Type


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import LLVM.Name

data TopLevelExpr
    = FuncDef Function
    | DataDef Data

data Function = Function {
    -- | Name of the function. `@Nothing@` represents a lambda.
    funcName :: Maybe LLVM.Name,
    -- | Parameters that the function takes.
    funcParams :: [Param],
    -- | Return type of the function.
    funcReturn :: Type,
    -- | The body of the function.
    funcBody :: Expr
    }

data Param = Param {
    paramName :: Maybe LLVM.Name,
    paramType :: Type
    }

data Expr
    = VarExpr LLVM.Name
    | FuncApp Expr Expr
    | Lambda Function
    | IfElse Expr Expr Expr

data Data = Data {
    dataName :: LLVM.Name,
    dataCtors :: [Constructor]
    }

data Constructor = Constructor {
    ctorName :: LLVM.Name,
    ctorParams :: [Param]
    }


mkLambda :: [Param] -> Type -> Expr -> Expr
mkLambda params ret = Lambda . Function Nothing params ret
