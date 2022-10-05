{-# LANGUAGE OverloadedStrings #-}

{- |
In this module, the C++ representation of LLVM-IR
is constructed. In turn, the C++ representation is
then used to generate LLVM code.
-}

module Blossom.Backend.LLVM.AST (
    genModule,
) where

import Data.ByteString.Short (toShort)
import LLVM.IRBuilder (
    execModuleBuilder,
    emptyModuleBuilder,
    function,
    )
import qualified Blossom.LLTree.Module as LLT
import qualified Blossom.LLTree.Definition as LLT
import qualified Blossom.LLTree.Type as LLT
import qualified Blossom.LLTree.Info as LLT
import qualified Blossom.LLTree.Closure as LLT
import qualified Blossom.LLTree.Body as LLT
import qualified LLVM.AST as LLVM
import qualified LLVM.AST.Type as LLVM
import LLVM.AST.DataLayout (defaultDataLayout, Endianness(..))
import qualified LLVM.IRBuilder as LLVM
import Blossom.Common.Name (Name, display, catIdent)
import Data.String (fromString)
import Blossom.Backend.LLVM.ToLLVM (llvm)
import Blossom.LLTree.Info (Info(infoClosType))
import qualified Blossom.Common.Literal as LLT


genModule :: LLT.ModuleLLT -> LLVM.Module
genModule (LLT.ModuleLLT name path _imports defs) = LLVM.Module {
        LLVM.moduleName = toShort name,
        LLVM.moduleSourceFileName = fromString path,
        LLVM.moduleDataLayout = Just (defaultDataLayout LittleEndian),
        LLVM.moduleTargetTriple = Nothing,
        LLVM.moduleDefinitions = buildModule defs
    }

buildModule :: [LLT.Definition] -> [LLVM.Definition]
buildModule = execModuleBuilder emptyModuleBuilder . mapM_ genDefn

-- mkGlobalFunc :: Name -> [LLT.Param] -> LLT.Type -> LLVM.Global
-- mkGlobalFunc name params typ = LLVM.functionDefaults {
--     LLVM.G.callingConvention = LLVM.GHC,
--     LLVM.G.returnType = llvm typ,
--     LLVM.G.name = llvm name,
--     LLVM.G.parameters = (params', False)
--     }
--     where
--         params' = map (\(LLT.Param name' typ') ->
--             LLVM.Parameter (llvm typ') (llvm name') []
--             ) params

genDefn :: LLT.Definition -> LLVM.ModuleBuilder ()
genDefn (LLT.FuncDef name closure params retType info body) = do
    let closType = infoClosType info
    _closure' <- genClosure name closure closType
    _info' <- genInfo info body
    let params' = map (\(LLT.Param name' typ) ->
            let name'' = toShort $ display name'
            in (llvm typ, LLVM.ParameterName name'')
            ) params
    let retType' = llvm retType
    function (llvm name) params' retType' (genBody body)
    return ()
genDefn LLT.DataDef{} = return ()

-- TODO
genClosure :: Name -> LLT.Closure -> LLT.ClosureType -> LLVM.ModuleBuilder ()
genClosure name closure closType = do
    let name' = catIdent name "_closure"
    case closType of
        LLT.StaticFuncClosure -> return ()
        LLT.DynamicFuncClosure -> do
            LLVM.typedef (llvm name') $ Just (closureStructure closure)
            return ()
        LLT.DynamicCtorClosure -> return ()
        LLT.StaticCtorClosure -> return ()

closureStructure :: LLT.Closure -> LLVM.Type
closureStructure (LLT.Closure frees (LLT.FuncPtr _name typ)) =
    let ptrs = map (\(LLT.Typed _name' typ') -> LLVM.ptr (llvm typ')) frees
    in LLVM.StructureType False $ ptrs ++ [LLVM.ptr (llvm typ)]

genInfo :: LLT.Info -> LLT.Body -> LLVM.ModuleBuilder ()
genInfo _ _ = return ()

type BodyBuilder = [LLVM.Operand] -> LLVM.IRBuilderT LLVM.ModuleBuilder ()

genBody :: LLT.Body -> BodyBuilder
genBody (LLT.Body instrs terminator) _ops = do
    LLVM.emitBlockStart =<< LLVM.fresh
    mapM_ genInstr instrs
    genTerminator terminator
    return ()

genBody' :: LLT.Body -> LLVM.IRBuilderT LLVM.ModuleBuilder ()
genBody' (LLT.Body instrs terminator) = do
    mapM_ genInstr instrs
    genTerminator terminator
    return ()

genInstr :: LLT.Instruction -> LLVM.IRBuilderT LLVM.ModuleBuilder LLVM.Operand
genInstr (LLT.BitCast val typ) = LLVM.bitcast (llvm val) (llvm typ)
genInstr (LLT.AutoCast val typ) = case (val, typ) of
    (LLT.LitVal LLT.FloatLit{}, _) -> case typ of
        LLT.I8 -> LLVM.fptosi val' typ'
        LLT.I16 -> LLVM.fptosi val' typ'
        LLT.I32 -> LLVM.fptosi val' typ'
        LLT.I64 -> LLVM.fptosi val' typ'
        LLT.F64 -> return val'
        LLT.F32 -> LLVM.fptrunc val' typ'
        LLT.Char -> error "getInstr: Cannot cast float to char"
        LLT.String -> error "getInstr: Cannot cast float to string"
        LLT.Unit -> return undefined -- TODO?
        LLT.Pointer _to -> error "getInstr: Cannot cast float to pointer"
        LLT.Struct _types -> LLVM.bitcast val' typ' -- ?
        LLT.FuncType _ _ -> error "getInstr: Cannot cast float to function"
    _ -> LLVM.bitcast val' typ' -- TODO (I'm lazy)
    where
        val' = llvm val
        typ' = llvm typ
genInstr (LLT.Call (LLT.Typed name typ) params) = LLVM.call
    (LLVM.LocalReference (llvm typ) (llvm name))
    (map (\param -> (llvm param, [])) params)
genInstr (LLT.StackAlloc typ) = LLVM.alloca (llvm typ) Nothing 1
genInstr (LLT.GetMember _typ _name) = LLVM.gep undefined undefined

genTerminator :: LLT.Terminator -> LLVM.IRBuilderT LLVM.ModuleBuilder ()
genTerminator (LLT.Return Nothing) = LLVM.retVoid
genTerminator (LLT.Return (Just val)) = LLVM.ret (llvm val)
genTerminator (LLT.Branch cond tr fl) = do
    currName <- show <$> LLVM.currentBlock
    trName <- LLVM.freshName (fromString (currName ++ "_true"))
    flName <- LLVM.freshName (fromString (currName ++ "_false"))
    LLVM.condBr (llvm cond) trName flName
    LLVM.emitBlockStart trName
    genBody' tr
    LLVM.emitBlockStart flName
    genBody' fl
