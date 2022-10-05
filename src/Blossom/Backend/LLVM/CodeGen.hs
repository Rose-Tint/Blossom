{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Blossom.Backend.LLVM.CodeGen (
    CodeGen(..),
    CodeGenState(..),
) where

import Blossom.Cmd (CmdLine)
import Control.Monad.State (State)
import Data.ByteString (ByteString)


data CodeGenState = CodeGenState {
    -- | Name of the current module being compiled.
    cgsModuleName :: ByteString,
    cgsCmdLine :: CmdLine
    }

newtype CodeGen a = CodeGen {
    runCodeGen :: State CodeGenState a
    }
    deriving (Functor, Applicative, Monad)
