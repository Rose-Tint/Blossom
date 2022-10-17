{-# LANGUAGE OverloadedStrings #-}

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
    typeArity,
) where

import Blossom.Common.Name (Ident)
import Blossom.Common.Literal (Literal)
import Blossom.Common.Source (HasLoc(..), mergeLocs)
import Prettyprinter (
    Pretty(pretty),
    PageWidth(..),
    (<+>),
    brackets,
    hsep,
    line,
    nest,
    sep,
    vsep,
    backslash,
    braces,
    colon,
    equals,
    parens,
    pageWidth,
    )


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
    | Lambda Params Expr
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

-- | The arity that a function with the given type should have.
typeArity :: Type -> Int
typeArity TypeCon{} = 0
typeArity (_ :-> typ) = 1 + typeArity typ

instance Pretty ModuleAST where
    pretty (ModuleAST imports exprs) =
        lineBreak <>
        vsep (map pretty imports) <> line <>
        vsep (map ((<> line) . pretty) exprs) <>
        lineBreak
        where
            lineBreak = pageWidth (\pw -> brackets $ case pw of
                AvailablePerLine w _ -> pretty (replicate (w - 2) '~')
                Unbounded -> pretty (replicate 78 '~')
                )

instance Pretty Import where
    pretty (Import mdl) = "import" <+> pretty mdl

instance Pretty TopLevelExpr where
    pretty (FuncDecl ident typ) =
        "func" <+> pretty ident <> colon <+> pretty typ
    pretty (FuncDef ident params body) =
        "func" <+> pretty ident <+> sep (map pretty params)
            <> nest 4 (line <> equals <+> pretty body)
    pretty (DataDef ident ctors) =
        "data" <+> pretty ident <+> braces (nest 4 (line <>
            vsep (map pretty ctors)
            ) <> line)

instance Pretty Pattern where
    pretty (Param ident) = pretty ident
    pretty (CtorPtrn ident []) = parens (pretty ident)
    pretty (CtorPtrn ident args) = parens $
        pretty ident <+> hsep (map pretty args)

instance Pretty Expr where
    pretty (VarExpr ident) = pretty ident
    pretty (LitExpr lit) = pretty lit
    pretty (FuncApp vals) = hsep (map (\val -> case val of
        FuncApp{} -> parens (pretty val)
        Lambda{} -> parens (pretty val)
        TypedExpr{} -> parens (pretty val)
        _ -> pretty val
        ) vals)
    pretty (Lambda params body) =
        backslash <> hsep (map pretty params) <+> "=>" <+> pretty body
    pretty (Match expr cases) = "match" <+> pretty expr <+> braces
        (nest 4 (line <> vsep (map pretty cases)))
    pretty (TypedExpr expr typ) = pretty expr <+> colon <+> pretty typ

instance Pretty Case where
    pretty (Case expr body) = pretty expr <+> "=>" <+> pretty body

instance Pretty Constructor where
    pretty (Constructor ident typ) = pretty ident <+> colon <+> pretty typ
    pretty (Nullary ident) = pretty ident

instance Pretty Type where
    pretty (TypeCon name []) = pretty name
    pretty (TypeCon name args) = pretty name <+> hsep (map pretty args)
    pretty (t1@(:->){} :-> t2) = parens (pretty t1) <+> "->" <+> pretty t2
    pretty (t1 :-> t2) = pretty t1 <+> "->" <+> pretty t2

instance HasLoc Pattern where
    getLoc (Param iden) = getLoc iden
    getLoc (CtorPtrn ctor []) = getLoc ctor
    getLoc (CtorPtrn ctor args) = mergeLocs ctor (last args)
