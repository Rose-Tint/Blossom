{-# LANGUAGE OverloadedStrings #-}

{- |
In this module, the C++ representation of LLVM-IR
is constructed. In turn, the C++ representation is
then used to generate LLVM code.
-}

module Blossom.Backend.LLVM.AST (
    genModule,
) where

import Blossom.Backend.LLVM.ToLLVM (llvm)
import Blossom.Common.Literal (Literal(..))
import Blossom.Common.Name (ModuleName(MdlName), Name, affixName, display)
import Blossom.LLTree.Module (ModuleLLT(..))
import Blossom.LLTree.Definition (Definition(..), Param(..))
import Blossom.LLTree.Type (Type(..), Typed(..))
import Blossom.LLTree.Closure (Closure(..), ClosureType(..), FuncPtr(..))
import Blossom.LLTree.Body (Body(..), Instruction(..), Terminator(..), Value(..))
import Blossom.LLTree.Info (Info(..))
import Data.ByteString.Short (toShort)
import Data.String (fromString)
import qualified LLVM.AST as LLVM (Definition, Module(..), Operand(..), Type(..))
import LLVM.AST.Type (ptr)
import LLVM.AST.DataLayout (Endianness(..), defaultDataLayout)
import LLVM.IRBuilder hiding (buildModule)


genModule :: ModuleLLT -> LLVM.Module
genModule (ModuleLLT (MdlName name) path _imports defs) = LLVM.Module {
    LLVM.moduleName = toShort name,
    LLVM.moduleSourceFileName = fromString path,
    LLVM.moduleDataLayout = Just (defaultDataLayout LittleEndian),
    LLVM.moduleTargetTriple = Nothing,
    LLVM.moduleDefinitions = buildModule defs
    }

buildModule :: [Definition] -> [LLVM.Definition]
buildModule = execModuleBuilder emptyModuleBuilder . mapM_ genDefn

genDefn :: Definition -> ModuleBuilder ()
genDefn (FuncDef name closure params retType info body) = do
    let closType = infoClosType info
    _closure' <- genClosure name closure closType
    _info' <- genInfo info body
    let params' = map (\(Param name' typ) ->
            let name'' = toShort $ display name'
            in (llvm typ, ParameterName name'')
            ) params
    let retType' = llvm retType
    function (llvm name) params' retType' (genBody body)
    return ()
genDefn DataDef{} = return ()

-- TODO
genClosure :: Name -> Closure -> ClosureType -> ModuleBuilder ()
genClosure name closure closType = do
    let name' = affixName name "_closure"
    case closType of
        StaticFuncClosure -> return ()
        DynamicFuncClosure -> do
            typedef (llvm name') $ Just (closureStructure closure)
            return ()
        DynamicCtorClosure -> return ()
        StaticCtorClosure -> return ()

closureStructure :: Closure -> LLVM.Type
closureStructure (Closure frees (FuncPtr _name typ)) =
    let ptrs = map (\(Typed _name' typ') -> ptr (llvm typ')) frees
    in LLVM.StructureType False $ ptrs ++ [ptr (llvm typ)]

genInfo :: Info -> Body -> ModuleBuilder ()
genInfo _ _ = return ()

type BodyBuilder = [LLVM.Operand] -> IRBuilderT ModuleBuilder ()

genBody :: Body -> BodyBuilder
genBody (Body instrs terminator) _ops = do
    emitBlockStart =<< fresh
    mapM_ genInstr instrs
    genTerminator terminator
    return ()

genBody' :: Body -> IRBuilderT ModuleBuilder ()
genBody' (Body instrs terminator) = do
    mapM_ genInstr instrs
    genTerminator terminator
    return ()

genInstr :: Instruction -> IRBuilderT ModuleBuilder LLVM.Operand
genInstr (BitCast val typ) = bitcast (llvm val) (llvm typ)
genInstr (AutoCast val typ) = case (val, typ) of
    (LitVal FloatLit{}, _) -> case typ of
        I8 -> fptosi val' typ'
        I16 -> fptosi val' typ'
        I32 -> fptosi val' typ'
        I64 -> fptosi val' typ'
        F64 -> return val'
        F32 -> fptrunc val' typ'
        Char -> error "getInstr: Cannot cast float to char"
        String -> error "getInstr: Cannot cast float to string"
        Unit -> return undefined -- TODO?
        Pointer _to -> error "getInstr: Cannot cast float to pointer"
        Struct _types -> bitcast val' typ' -- ?
        FuncType _ _ -> error "getInstr: Cannot cast float to function"
    _ -> bitcast val' typ' -- TODO (I'm lazy)
    where
        val' = llvm val
        typ' = llvm typ
genInstr (Call (Typed name typ) params) = call
    (LLVM.LocalReference (llvm typ) (llvm name))
    (map (\param -> (llvm param, [])) params)
genInstr (StackAlloc typ) = alloca (llvm typ) Nothing 1
genInstr (GetMember _typ _name) = gep undefined undefined

genTerminator :: Terminator -> IRBuilderT ModuleBuilder ()
genTerminator (Return Nothing) = retVoid
genTerminator (Return (Just val)) = ret (llvm val)
genTerminator (Branch cond tr fl) = do
    currName <- show <$> currentBlock
    trName <- freshName (fromString (currName ++ "_true"))
    flName <- freshName (fromString (currName ++ "_false"))
    condBr (llvm cond) trName flName
    emitBlockStart trName
    genBody' tr
    emitBlockStart flName
    genBody' fl
