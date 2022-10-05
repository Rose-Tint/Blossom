{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Blossom.Backend.LLVM.ToLLVM (
    llvm,
) where

import qualified Blossom.Common.Literal as Bl
import qualified Blossom.Common.Name as Bl
import qualified Blossom.LLTree.Body as Bl
import qualified Blossom.LLTree.Type as Bl
import qualified Data.ByteString as BS (length)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Short (toShort)
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Constant as LLVM hiding (isPacked)
import qualified LLVM.AST.Constant (isPacked)
import qualified LLVM.AST.Float as LLVM
import qualified LLVM.AST.Type as LLVM


-- | A typeclass that for types that can 'trivially'
-- convert to a type that is compatible with or used in
-- conjunction with the llvm-hs-pure package.
--
-- A type is "trivially convertible" if it can be
-- converted without use of any monad.
class ToLLVM a b | b -> a where
    -- | Converts to an LLVM type
    llvm :: a -> b

instance ToLLVM Bl.Name LLVM.Name where
    llvm = LLVM.Name . toShort . Bl.display

instance ToLLVM Bl.Type LLVM.Type where
    llvm Bl.I64 = LLVM.IntegerType 64
    llvm Bl.I32 = LLVM.IntegerType 32
    llvm Bl.I16 = LLVM.IntegerType 16
    llvm Bl.I8 = LLVM.IntegerType 8
    llvm Bl.F64 = LLVM.FloatingPointType LLVM.FloatFP
    llvm Bl.F32 = LLVM.FloatingPointType LLVM.DoubleFP
    llvm Bl.Char = LLVM.IntegerType 8
    llvm Bl.String = LLVM.StructureType {
        LLVM.isPacked = False,
        LLVM.elementTypes = [
            LLVM.i8,
            LLVM.ArrayType 0 (llvm Bl.Char)
            ]
        }
    llvm Bl.Unit = LLVM.VoidType
    llvm (Bl.Pointer typ) = LLVM.ptr (llvm typ)
    llvm (Bl.Struct types) = LLVM.StructureType False (map llvm types)
    llvm (Bl.FuncType retType paramTypes) = LLVM.FunctionType {
        LLVM.resultType = llvm retType,
        LLVM.argumentTypes = map llvm paramTypes,
        LLVM.isVarArg = False
    }

instance ToLLVM Bl.Value LLVM.Operand where
    llvm (Bl.VarVal (Bl.Typed name typ)) =
        LLVM.LocalReference (llvm typ) (llvm name)
    llvm (Bl.LitVal lit) = LLVM.ConstantOperand (llvm lit)

instance ToLLVM Bl.Literal LLVM.Constant where
    llvm (Bl.IntLit n) = LLVM.Int 32 (fromIntegral n)
    llvm (Bl.FloatLit n) = LLVM.Float (LLVM.Double n)
    llvm (Bl.CharLit ch) = LLVM.Int 8 (fromIntegral (fromEnum ch))
    llvm (Bl.StringLit str) = LLVM.Struct {
        LLVM.structName = Just "String",
        LLVM.AST.Constant.isPacked = False,
        LLVM.memberValues = [
            LLVM.Int 8 (fromIntegral (BS.length str)),
            LLVM.Struct {
                LLVM.structName = Nothing,
                LLVM.AST.Constant.isPacked = True,
                LLVM.memberValues = map
                    (LLVM.Int 8 . fromIntegral . fromEnum)
                    (unpack str)
                }
            ]
        }
