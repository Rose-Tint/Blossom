{-# LANGUAGE OverloadedStrings #-}

module Blossom.Parsing.SynTree (
    SynTree(..),
    Import(..),
    TopLevelExpr(..),
    Pattern(..),
    Expr(..),
    Constructor(..),
    Case(..),
    Params,
    Param,
) where

import Blossom.Typing.Type (Type(..))
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


data SynTree name = SynTree {
    moduleImports :: [Import name],
    moduleTopExprs :: [TopLevelExpr name]
    }
    deriving (Show, Eq)

newtype Import name = Import name
    deriving (Show, Eq)

data TopLevelExpr name
    = FuncDecl name (Type name)
    | FuncDef name (Params name) (Expr name)
    | DataDef name [name] [Constructor name]
    deriving (Show, Eq)

data Pattern name
    = Param name
    | CtorPtrn name [Pattern name]
    deriving (Show, Eq)

type Params name = [Param name]

type Param = Pattern

data Expr name
    = VarExpr name
    | LitExpr Literal
    -- | SHOULD NOT BE EMPTY (but NonEmpty makes for ugly code).
    -- A list of function applications. It is done this
    -- way to allow for custom operator precedence. A
    -- Shunting-Yard algorithm will figure the application
    -- order after parsing.
    | FuncApp [Expr name]
    | Lambda (Params name) (Expr name)
    | Match (Expr name) [Case name]
    | TypedExpr (Expr name) (Type name)
    deriving (Show, Eq)

data Case name = Case (Pattern name) (Expr name)
    deriving (Show, Eq)

data Constructor name
    = Constructor {
        ctorName :: name,
        ctorParams :: Type name
    }
    | Nullary {
        ctorName :: name
    }
    deriving (Show, Eq)

instance Pretty name => Pretty (SynTree name) where
    pretty (SynTree imports exprs) =
        lineBreak <>
        vsep (map pretty imports) <> line <>
        vsep (map ((<> line) . pretty) exprs) <>
        lineBreak
        where
            lineBreak = pageWidth (\pw -> brackets $ case pw of
                AvailablePerLine w _ -> pretty (replicate (w - 2) '~')
                Unbounded -> pretty (replicate 78 '~')
                )

instance Pretty name => Pretty (Import name) where
    pretty (Import mdl) = "import" <+> pretty mdl

instance Pretty name => Pretty (TopLevelExpr name) where
    pretty (FuncDecl ident typ) =
        "func" <+> pretty ident <> colon <+> pretty typ
    pretty (FuncDef ident params body) =
        "func" <+> pretty ident <+> sep (map pretty params)
            <> nest 4 (line <> equals <+> pretty body)
    pretty (DataDef ident tvs ctors) =
        "data" <+> pretty ident <+> hsep (map pretty tvs)
        <+> braces (nest 4 (line <>
            vsep (map pretty ctors)
            ) <> line)

instance Pretty name => Pretty (Pattern name) where
    pretty (Param ident) = pretty ident
    pretty (CtorPtrn ident []) = parens (pretty ident)
    pretty (CtorPtrn ident args) = parens $
        pretty ident <+> hsep (map pretty args)

instance Pretty name => Pretty (Expr name) where
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

instance Pretty name => Pretty (Case name) where
    pretty (Case expr body) = pretty expr <+> "=>" <+> pretty body

instance Pretty name => Pretty (Constructor name) where
    pretty (Constructor ident typ) = pretty ident <+> colon <+> pretty typ
    pretty (Nullary ident) = pretty ident

instance HasLoc name => HasLoc (Pattern name) where
    getLoc (Param iden) = getLoc iden
    getLoc (CtorPtrn ctor []) = getLoc ctor
    getLoc (CtorPtrn ctor args) = mergeLocs ctor (last args)
