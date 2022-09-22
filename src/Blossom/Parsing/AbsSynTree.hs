module Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevelExpr(..),
    Pattern(..),
    Expr(..),
    Constructor(..),
    Case(..),
    Params,
    Param,
) where

import Data.ByteString.Lazy (ByteString)

import Blossom.Typing.Type (Type)
import Blossom.Common.Name (Name)
import Data.Int (Int64)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }

newtype Import = Import Name

data TopLevelExpr
    = FuncDecl Name Type
    | FuncDef Name Params Expr
    | DataDef Name [Constructor]

data Pattern
    = Param Name
    | CtorPtrn Name [Pattern]

type Params = [Pattern]

type Param = Pattern

data Expr
    = VarExpr Name
    | IntExpr Int64
    | FloatExpr Double
    | CharExpr Char
    | StringExpr ByteString
    -- | SHOULD NOT BE EMPTY (but NonEmpty makes for ugly code).
    -- A list of function applications. It is done this
    -- way to allow for custom operator precedence. A
    -- Shunting-Yard algorithm will figure the application
    -- order after parsing.
    | FuncApp [Expr]
    | Lambda [Pattern] Expr
    | Match Expr [Case]
    | TypedExpr Expr Type

data Case = Case Pattern Expr

data Constructor
    = Constructor {
        ctorName :: Name,
        ctorParams :: Type
    }
    | Nullary {
        ctorName :: Name
    }
