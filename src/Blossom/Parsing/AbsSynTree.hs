module Blossom.Parsing.AbsSynTree (
    -- export modules for constructors
    module Blossom.Parsing.SynTree,
    module Blossom.Typing.Type,
    AbsSynTree,
    AbsType,
    AbsTopLevelExpr,
    AbsPattern,
    AbsExpr,
    AbsConstructor,
    AbsCase,
    AbsParams,
    AbsParam,
) where

import Blossom.Common.Name.Ident (Ident)
import Blossom.Typing.Type (Type(..))
import Blossom.Parsing.SynTree


type AbsSynTree = SynTree Ident

type AbsType = Type Ident

type AbsTopLevelExpr = TopLevelExpr Ident

type AbsPattern = Pattern Ident

type AbsExpr = Expr Ident

type AbsConstructor = Constructor Ident

type AbsCase = Case Ident

type AbsParams = Params Ident

type AbsParam = Param Ident
