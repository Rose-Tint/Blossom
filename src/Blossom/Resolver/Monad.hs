{-# LANGUAGE FlexibleInstances #-}

module Blossom.Resolver.Monad (
    Resolver,
    ResolverT,
    runResolverT,
    moduleName,
    modulePath,
    definitionList,
    emitDefn,
    insertExternal,
    insertInternal,
    lookupName,
    throwTODO,
    throwDeclNoDef,
    throwMultipleDefs,
    throwTooManyParams,
) where

import Blossom.Common.Name (Ident(..), ModuleName, Name, internal, external)
import Blossom.LLTree.Definition (Definition)
import Blossom.Resolver.Errors (ResolverError(..))
import qualified Blossom.Parsing.AbsSynTree as AST (Params, Type)
import Control.Monad.Except (ExceptT, runExceptT, MonadError(throwError))
import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT, gets, evalStateT, modify)
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Blossom.Common.Source (SourceLoc)


-- | The resolver is responsible for converting the Abstract Syntax Tree (AST)
-- into a Low-Level Tree (LLT), resolving namespace conflicts, desugaring, etc.
type ResolverT m = ExceptT ResolverError (StateT ResolverState m)
type Resolver = ResolverT Identity

data ResolverState
    = ResolverState {
        -- | The name of the module where the resolution is taking place.
        rsModuleName :: ModuleName,
        rsModulePath :: FilePath,
        -- | A list of definitions
        rsDefns :: [Definition],
        -- -- | A map of names to their associated definition.
        -- rsDefns :: M.Map Name Definition
        rsNameMap :: M.Map ByteString Name
    }

newState :: ModuleName -> FilePath -> ResolverState
newState mdl path = ResolverState mdl path mempty mempty

runResolverT :: Monad m => ModuleName -> FilePath -> ResolverT m a
    -> m (Either ResolverError a)
runResolverT mdl path res = evalStateT (runExceptT res) state
    where
        state = newState mdl path

moduleName :: Monad m => ResolverT m ByteString
moduleName = gets rsModuleName

modulePath :: Monad m => ResolverT m FilePath
modulePath = gets rsModulePath

definitionList :: Monad m => ResolverT m [Definition]
definitionList = gets rsDefns

emitDefn :: Monad m => Definition -> ResolverT m ()
emitDefn defn = modify $ \rs -> rs { rsDefns = defn:rsDefns rs }

-- emitDefn :: Name -> Definition -> Resolver ()
-- emitDefn name defn = modify $ \(rs@(ResolverState _mdl defs)) ->
--     rs { rsDefns = M.insert name defn defs }

insertInternal :: Monad m => Ident -> ResolverT m ()
insertInternal ident = insertName ident (internal ident)

insertExternal :: Monad m => ModuleName -> Ident -> ResolverT m ()
insertExternal mdl ident = insertName ident (external mdl ident)

insertName :: Monad m => Ident -> Name -> ResolverT m ()
insertName (Ident iden _loc) name = modify $ \rs ->
    rs { rsNameMap = M.insert iden name (rsNameMap rs) }

lookupName :: Monad m => Ident -> ResolverT m (Maybe Name)
lookupName (Ident iden _loc) = gets (M.lookup iden . rsNameMap)

throwTODO :: Monad m => SourceLoc -> String -> ResolverT m a
throwTODO loc = throwError . TODO loc

throwDeclNoDef :: Monad m => Name -> ResolverT m a
throwDeclNoDef = throwError . DeclNoDef

throwMultipleDefs :: Monad m => Name -> ResolverT m a
throwMultipleDefs = throwError . MultipleDefs

throwTooManyParams :: Monad m => AST.Params -> AST.Type -> ResolverT m a
throwTooManyParams [] _ = throwError $ InternalError "empty parameter list"
throwTooManyParams ps typ = throwError $ TooManyParams ps typ
