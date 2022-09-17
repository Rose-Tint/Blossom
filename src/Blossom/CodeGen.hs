module Blossom.CodeGen (
    compileFunction,
) where

import LLVM.IRBuilder (
    MonadIRBuilder,
    MonadModuleBuilder,
    ParameterName(..),
    freshUnName,
    function,
    )
import Blossom.Parsing.AbsSynTree (
    Param(..),
    Function(..),
    )
import qualified LLVM.AST as LLVM
import Blossom.Typing.Type (Type(..),)
import Control.Monad (forM,)


-- nameFromMaybe :: MonadIRBuilder m => Maybe LLVM.Name -> m LLVM.Name
-- nameFromMaybe Nothing -> j

compileFunction :: (MonadIRBuilder m, MonadModuleBuilder m) => Function -> m LLVM.Operand
compileFunction (Function mName params retType _body) = do
    name <- maybe freshUnName return mName
    paramsList <- forM params (\(Param pName pType) -> do
        typ <- compileType pType
        let pName' = case pName of
                Just (LLVM.Name pName'') -> ParameterName pName''
                _ -> NoParameterName
        return (typ, pName')
        )
    retType' <- compileType retType
    bodyBuilder <- undefined
    function name paramsList retType' bodyBuilder

-- | [TODO] Searched a symbol table to make the correct type
compileType :: {- MonadIRBuilder m => -} Type -> m LLVM.Type
compileType (TypeCon _name _args) = undefined
compileType (_t1 :-> _t2) = undefined
