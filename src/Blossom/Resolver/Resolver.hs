{-# LANGUAGE OverloadedStrings #-}

module Blossom.Resolver.Resolver (
    module Blossom.Resolver.Resolver,
    -- resolveAST,
) where

import Blossom.Common.Name (Ident, Name, display, external)
import qualified Blossom.LLTree.Body as LLT
-- import qualified Blossom.LLTree.Closure as LLT
import qualified Blossom.LLTree.Definition as LLT
-- import qualified Blossom.LLTree.Info as LLT
import qualified Blossom.LLTree.Module as LLT
import qualified Blossom.LLTree.Type as LLT
import qualified Blossom.Parsing.AbsSynTree as AST
import Blossom.Resolver.Monad (
    ResolverT,
    moduleName,
    modulePath,
    definitionList,
    emitDefn,
    throwTODO,
    throwDeclNoDef,
    throwMultipleDefs,
    throwTooManyParams,
    )
import Data.ByteString.Char8 (unpack)
import Blossom.Common.Source (HasLoc(getLoc))


resolveAST :: Monad m => AST.ModuleAST -> ResolverT m LLT.ModuleLLT
resolveAST (AST.ModuleAST imports defs) = do
    mdl <- moduleName
    path <- modulePath
    imports' <- mapM resolveImport imports
    resolveTopExprs defs
    defs' <- definitionList
    let llt = LLT.ModuleLLT mdl path imports' defs'
    return llt

resolveImport :: Monad m => AST.Import -> ResolverT m LLT.Import
resolveImport (AST.Import name) = do
    name' <- resolveIdent name
    let path = map (\ch ->
            if ch == '_' then '/' else ch
            ) (unpack (display name'))
            ++ ".bl"
    return (LLT.Import path)

-- | Combines multiple function defintions into one, provided that they are
-- 'compatible'. Two function definitions are 'compatible' if they cannot
-- match against the same inputs.
resolveTopExprs :: Monad m => [AST.TopLevelExpr] -> ResolverT m ()
resolveTopExprs (AST.FuncDecl iden typ : exprs) = do
    name <- resolveIdent iden
    let (defs, remExprs) = takeFuncDefs iden exprs
    (params, body) <- mergeDefs defs
    (params', retType) <- applyParams params typ
    body' <- resolveExpr body
    emitDefn $ LLT.FuncDef {
            LLT.defnName = name,
            LLT.defnClosure = undefined, -- throwTODO "function closures",
            LLT.defnParams = params',
            LLT.defnRtnType = retType,
            LLT.defnInfo = undefined, -- throwTODO "info tables",
            LLT.defnBody = body'
            }
    resolveTopExprs remExprs
    where
        mergeDefs [] = resolveIdent iden >>= throwDeclNoDef
        -- if there is only one definition, there is no need to (nor potential
        -- for) merging
        mergeDefs [(params, body)] = return (params, body)
        -- treat multiple (compatible) definitions as a match clause.
        -- (TODO: Figure out a way to merge different parameter names that
        -- represent the same variable)
        -- TODO:
        mergeDefs _defs = resolveIdent iden >>= throwMultipleDefs
            -- Match $ map (uncurry Case) defs
resolveTopExprs _ = return ()

-- | Merges parameters and a type into parameters with their respective types,
-- and the return type
applyParams :: Monad m => AST.Params -> AST.Type
    -> ResolverT m ([LLT.Param], LLT.Type)
applyParams params typ = do
    mResult <- go params typ
    case mResult of
        Nothing -> throwTooManyParams params typ
        Just result -> return result
    where
        go :: Monad m => AST.Params -> AST.Type
            -> ResolverT m (Maybe ([LLT.Param], LLT.Type))
        go [] typ' = do
            let typ'' = LLT.fromASTType typ'
            return (Just ([], typ''))
        go (p:ps) (t1 AST.:-> t2) = do
            name <- resolvePattern p
            let typ' = LLT.fromASTType t1
            let p' = LLT.Param name typ'
            result <- go ps t2
            case result of
                Nothing -> return Nothing
                Just (ps', rtn) -> return (Just (p':ps', rtn))
        go (:){} AST.TypeCon{} = return Nothing

takeFuncDefs :: Ident -> [AST.TopLevelExpr]
    -> ([(AST.Params, AST.Expr)], [AST.TopLevelExpr])
takeFuncDefs declName allExprs@(AST.FuncDef defName params body : exprs)
    | declName == defName =
        let (defs, remExprs) = takeFuncDefs declName exprs
        in ((params, body):defs, remExprs)
    | otherwise = ([], allExprs)
takeFuncDefs _ exprs = ([], exprs)

-- TODO!!!
resolvePattern :: Monad m => AST.Pattern -> ResolverT m Name
resolvePattern (AST.Param name) = resolveIdent name
resolvePattern ptrn@AST.CtorPtrn{} = throwTODO (getLoc ptrn) "Patterns"

resolveExpr :: Monad m => AST.Expr -> ResolverT m LLT.Body
resolveExpr _ = throwTODO undefined "Expressions"

resolveIdent :: Monad m => Ident -> ResolverT m Name
resolveIdent ident = do
    mdl <- moduleName
    return $! external mdl ident
