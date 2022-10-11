{-# LANGUAGE FlexibleInstances #-}

module Blossom.Resolver.Monad (
    Resolver,
    runResolver,
    moduleName,
    modulePath,
    definitionList,
    emitDefn,
    insertExternal,
    insertInternal,
    lookupName,
) where

import Blossom.Common.Name (Ident(..), ModuleName, Name, internal, external)
import Blossom.LLTree.Definition (Definition)
import Control.Monad.State (State, gets, evalState, modify)
import Data.ByteString (ByteString)
import qualified Data.Map as M


-- | The resolver is responsible for converting the Abstract Syntax Tree (AST)
-- into a Low-Level Tree (LLT), resolving namespace conflicts, desugaring, etc.
type Resolver = State ResolverState

data ResolverState
    = RS {
        -- | The name of the module where the resolution is taking place.
        rsModuleName :: ModuleName,
        rsModulePath :: FilePath,
        -- | A list of definitions
        rsDefns :: [Definition],
        -- -- | A map of names to their associated definition.
        -- rsDefns :: M.Map Name Definition
        rsNameMap :: M.Map ByteString Name
    }

runResolver :: ByteString -> FilePath -> Resolver a -> a
runResolver mdl path = flip evalState (RS mdl path mempty mempty)

moduleName :: Resolver ByteString
moduleName = gets rsModuleName

modulePath :: Resolver FilePath
modulePath = gets rsModulePath

definitionList :: Resolver [Definition]
definitionList = gets rsDefns

emitDefn :: Definition -> Resolver ()
emitDefn defn = modify $ \rs -> rs { rsDefns = defn:rsDefns rs }

-- emitDefn :: Name -> Definition -> Resolver ()
-- emitDefn name defn = modify $ \(rs@(RS _mdl defs)) ->
--     rs { rsDefns = M.insert name defn defs }

insertInternal :: Ident -> Resolver ()
insertInternal ident = insertName ident (internal ident)

insertExternal :: ModuleName -> Ident -> Resolver ()
insertExternal mdl ident = insertName ident (external mdl ident)

insertName :: Ident -> Name -> Resolver ()
insertName (Ident iden _loc) name = modify $ \rs ->
    rs { rsNameMap = M.insert iden name (rsNameMap rs) }

lookupName :: Ident -> Resolver (Maybe Name)
lookupName (Ident iden _loc) = gets (M.lookup iden . rsNameMap)

instance MonadFail Resolver where
    fail = error
