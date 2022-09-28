module Blossom.LLTree.Closure (
    ClosureType(..),
    Closure(..),
    FuncPtr(..),
) where

import Blossom.LLTree.Type (Type)
import Blossom.Common.Name (Name)


data ClosureType
    = DynamicFuncClosure
    | DynamicCtorClosure
    | StaticCtorClosure
    | StaticFuncClosure

data Closure = Closure {
    closFreeVars :: [Name],
    closFuncPtr :: FuncPtr
    }

data FuncPtr = FuncPtr Name Type
