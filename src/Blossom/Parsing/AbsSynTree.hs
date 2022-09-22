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

import Blossom.Typing.Type (Type)
import Blossom.Common.Name (Name)
import Data.ByteString.Lazy (ByteString)
import Data.Int (Int64)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }
    deriving (Show, Eq)

newtype Import = Import Name
    deriving (Show, Eq)

data TopLevelExpr
    = FuncDecl Name Type
    | FuncDef Name Params Expr
    | DataDef Name [Constructor]
    deriving (Show, Eq)

data Pattern
    = Param Name
    | CtorPtrn Name [Pattern]
    deriving (Show, Eq)

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
    deriving (Show, Eq)

data Case = Case Pattern Expr
    deriving (Show, Eq)

data Constructor
    = Constructor {
        ctorName :: Name,
        ctorParams :: Type
    }
    | Nullary {
        ctorName :: Name
    }
    deriving (Show, Eq)
