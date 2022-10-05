module Blossom.LLTree.Closure (
    ClosureType(..),
    Closure(..),
    FuncPtr(..),
    isStaticClosType,
) where

import Blossom.Common.Name (Name)
import Blossom.LLTree.Type (Type, Var)


data ClosureType
    = DynamicFuncClosure
    | DynamicCtorClosure
    | StaticCtorClosure
    | StaticFuncClosure

data Closure = Closure {
    closFreeVars :: [Var],
    closFuncPtr :: FuncPtr
    }

data FuncPtr = FuncPtr Name Type

isStaticClosType :: ClosureType -> Bool
isStaticClosType StaticCtorClosure = True
isStaticClosType StaticFuncClosure = True
isStaticClosType _ = False
