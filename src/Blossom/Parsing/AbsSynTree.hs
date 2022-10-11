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
    Type(..),
) where

import Blossom.Common.Name (Ident)
import Blossom.Common.Literal (Literal)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }
    deriving (Show, Eq)

newtype Import = Import Ident
    deriving (Show, Eq)

data TopLevelExpr
    = FuncDecl Ident Type
    | FuncDef Ident Params Expr
    | DataDef Ident [Constructor]
    deriving (Show, Eq)

data Pattern
    = Param Ident
    | CtorPtrn Ident [Pattern]
    deriving (Show, Eq)

type Params = [Pattern]

type Param = Pattern

data Expr
    = VarExpr Ident
    | LitExpr Literal
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
        ctorName :: Ident,
        ctorParams :: Type
    }
    | Nullary {
        ctorName :: Ident
    }
    deriving (Show, Eq)

infixr 9 :->

-- | !!!ONLY TO BE USED PRE-RENAMING!!!
data Type
    = TypeCon {
        typeName :: Ident,
        typeArgs :: [Type]
    }
    -- TODO: Type variables
    | Type :-> Type
    deriving (Show, Eq)
