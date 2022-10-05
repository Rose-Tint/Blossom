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

import Blossom.Common.Name (Iden)
import Blossom.Common.Literal (Literal)


data ModuleAST = ModuleAST {
    moduleImports :: [Import],
    moduleTopExprs :: [TopLevelExpr]
    }
    deriving (Show, Eq)

newtype Import = Import Iden
    deriving (Show, Eq)

data TopLevelExpr
    = FuncDecl Iden Type
    | FuncDef Iden Params Expr
    | DataDef Iden [Constructor]
    deriving (Show, Eq)

data Pattern
    = Param Iden
    | CtorPtrn Iden [Pattern]
    deriving (Show, Eq)

type Params = [Pattern]

type Param = Pattern

data Expr
    = VarExpr Iden
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
        ctorName :: Iden,
        ctorParams :: Type
    }
    | Nullary {
        ctorName :: Iden
    }
    deriving (Show, Eq)

infixr 9 :->

-- | !!!ONLY TO BE USED PRE-RENAMING!!!
data Type
    = TypeCon {
        typeName :: Iden,
        typeArgs :: [Type]
    }
    -- TODO: Type variables
    | Type :-> Type
    deriving (Show, Eq)
