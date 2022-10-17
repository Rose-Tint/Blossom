module Blossom.LLTree.Body (
    Body(..),
    Instruction(..),
    Terminator(..),
    Value(..),
) where

import Blossom.Common.Literal (Literal)
import Blossom.LLTree.Type (Type, Var)


data Body = Body [Instruction] Terminator

data Terminator
    = Return (Maybe Value)
    | Branch {
        branchCond :: Value,
        branchTrue :: Body,
        branchFalse :: Body
    }

data Instruction
    = BitCast Value Type
    | AutoCast Value Type
    | Call Var [Value]
    | StackAlloc Type
    | GetMember Type Var

data Value
    = VarVal Var
    | LitVal Literal

