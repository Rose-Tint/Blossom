module Blossom.LLTree.Definition (
    Definition(..),
    Param(..),
    CtorDef(..),
    Arity(..),
    FuncApp(..),
    PartialApp(..),
) where

import Blossom.LLTree.Closure (Closure)
import Blossom.LLTree.Info (Header, Info)
import Blossom.Common.Name (Name)
import Blossom.LLTree.Body (Body, Value)
import Blossom.LLTree.Type (Type)


data Definition
    = FuncDef {
        defnName :: Name,
        defnClosure :: Closure,
        defnParams :: [Param],
        defnRtnType :: Type,
        defnInfo :: Info,
        defnBody :: Body
    }
    | DataDef {
        defnName :: Name,
        dataCtors :: [CtorDef]
    }

data Param = Param Name Type

data CtorDef = CtorDef {
    ctorName :: Name,
    ctorInfo :: Info
    }

-- | The number of arguments that a function can take.
newtype Arity = Arity Word

data FuncApp = FAP {
    fapHeader :: Header,
    fapArity :: Arity,
    fapClosure :: Closure,
    fapArgs :: [Value]
    }

data PartialApp = PAP {
    papHeader :: Header,
    papArity :: Arity,
    papClosure :: Closure,
    papArgs :: [Value] -- args that have been applied,
    }
