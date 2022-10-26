module Blossom.Rename (
    runRenamerT,
) where

import Blossom.Common.Name (Ident, Name, internal)
import Blossom.Parsing.AbsSynTree (AbsSynTree)
import Blossom.Parsing.SynTree (
    Constructor(..),
    Pattern(..),
    Expr(..),
    TopLevelExpr(..),
    SynTree(SynTree),
    Case(..),
    )
import Blossom.Typing.Type (Type(..))


runRenamerT :: Monad m => AbsSynTree -> m (SynTree Name)
runRenamerT = rename

class Rename a where
    rename :: Monad m => a Ident -> m (a Name)

instance Rename SynTree where
    rename (SynTree imports tops) = do
        tops' <- mapM rename tops
        return (SynTree imports tops')

instance Rename TopLevelExpr where
    rename (FuncDecl ident typ) = do
        let name = internal ident
        typ' <- rename typ
        return (FuncDecl name typ')
    rename (FuncDef ident params expr) = do
        let name = internal ident
        params' <- mapM rename params
        expr' <- rename expr
        return (FuncDef name params' expr')
    rename (DataDef ident tps ctors) = do
        let name = internal ident
        let tps' = map internal tps
        ctors' <- mapM rename ctors
        return (DataDef name tps' ctors')

instance Rename Type where
    rename (TypeCon ident) = return (TypeCon (internal ident))
    rename (TypeVar ident) = return (TypeVar (internal ident))
    rename (TypeApp con arg) = do
        con' <- rename con
        arg' <- rename arg
        return (TypeApp con' arg')
    rename (t1 :-> t2) = do
        t1' <- rename t1
        t2' <- rename t2
        return (t1' :-> t2')

instance Rename Pattern where
    rename (Param ident) = return (Param (internal ident))
    rename (CtorPtrn ctor args) = do
        let ctor' = internal ctor
        args' <- mapM rename args
        return (CtorPtrn ctor' args')

instance Rename Expr where
    rename (VarExpr name) = return (VarExpr (internal name))
    rename (LitExpr lit) = return (LitExpr lit)
    rename (FuncApp exprs) = FuncApp <$> mapM rename exprs
    rename (Lambda params expr) = do
        params' <- mapM rename params
        expr' <- rename expr
        return (Lambda params' expr')
    rename (Match value cases) = do
        value' <- rename value
        cases' <- mapM rename cases
        return (Match value' cases')
    rename (TypedExpr expr typ) = do
        expr' <- rename expr
        typ' <- rename typ
        return (TypedExpr expr' typ')

instance Rename Case where
    rename (Case ptrn expr) = do
        ptrn' <- rename ptrn
        expr' <- rename expr
        return (Case ptrn' expr')

instance Rename Constructor where
    rename (Nullary ident) = return (Nullary (internal ident))
    rename (Constructor ident typ) = do
        let name = internal ident
        typ' <- rename typ
        return (Constructor name typ')
