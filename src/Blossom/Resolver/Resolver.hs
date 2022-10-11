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
import Blossom.Resolver.Monad
import Data.ByteString.Char8 (unpack)


resolveAST :: AST.ModuleAST -> Resolver LLT.ModuleLLT
resolveAST (AST.ModuleAST imports defs) = do
    mdl <- moduleName
    path <- modulePath
    imports' <- mapM resolveImport imports
    resolveTopExprs defs
    defs' <- definitionList
    let llt = LLT.ModuleLLT mdl path imports' defs'
    return llt

resolveImport :: AST.Import -> Resolver LLT.Import
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
resolveTopExprs :: [AST.TopLevelExpr] -> Resolver ()
resolveTopExprs (AST.FuncDecl iden typ : exprs) = do
    name <- resolveIdent iden
    let (defs, remExprs) = takeFuncDefs iden exprs
    (params, body) <- mergeDefs defs
    (params', retType) <- applyParams params typ
    body' <- resolveExpr body
    emitDefn $ LLT.FuncDef {
            LLT.defnName = name,
            LLT.defnClosure = error "TODO (function closures)",
            LLT.defnParams = params',
            LLT.defnRtnType = retType,
            LLT.defnInfo = error "TODO (info tables)",
            LLT.defnBody = body'
            }
    resolveTopExprs remExprs
    where
        mergeDefs [] = error "Function declared without definition."
        -- if there is only one definition, there is no need to (nor potential
        -- for) merging
        mergeDefs [(params, body)] = return (params, body)
        -- treat multiple (compatible) definitions as a match clause.
        -- (TODO: Figure out a way to merge different parameter names that
        -- represent the same variable)
        -- TODO:
        mergeDefs _defs = error
            "Functions with multiple definitions not supported (yet)."
            -- Match $ map (uncurry Case) defs
resolveTopExprs _ = return ()

-- | Merges parameters and a type into parameters with their respective types,
-- and the return type
applyParams :: AST.Params -> AST.Type -> Resolver ([LLT.Param], LLT.Type)
applyParams [] typ = do
    let typ' = LLT.fromASTType typ
    return ([], typ')
applyParams (p:ps) (t1 AST.:-> t2) = do
    name <- resolvePattern p
    let typ = LLT.fromASTType t1
    let param = LLT.Param name typ
    (params, retType) <- applyParams ps t2
    return ((param:params), retType)
applyParams (_:_) AST.TypeCon{} = error
    "Too many parameters for the given function type."

takeFuncDefs :: Ident -> [AST.TopLevelExpr]
    -> ([(AST.Params, AST.Expr)], [AST.TopLevelExpr])
takeFuncDefs declName allExprs@(AST.FuncDef defName params body : exprs)
    | declName == defName =
        let (defs, remExprs) = takeFuncDefs declName exprs
        in (((params, body):defs), remExprs)
    | otherwise = ([], allExprs)
takeFuncDefs _ exprs = ([], exprs)

-- TODO!!!
resolvePattern :: AST.Pattern -> Resolver Name
resolvePattern (AST.Param name) = resolveIdent name
resolvePattern AST.CtorPtrn{} = error "Patterns are not supported (yet)."

resolveExpr :: AST.Expr -> Resolver LLT.Body
resolveExpr _ = error "Expressions not supported (yet)"

resolveIdent :: Ident -> Resolver Name
resolveIdent ident = do
    mdl <- moduleName
    return $! external mdl ident
