{-# LANGUAGE FlexibleInstances #-}

module Blossom.Resolver.Monad (
    Resolver,
    runResolver,
    moduleName,
    modulePath,
    definitionList,
    emitDefn,
) where

import Blossom.LLTree.Definition (Definition)
import Control.Monad.State (State, gets, evalState, modify)
import Data.ByteString (ByteString)
-- import qualified Data.Map as M


-- | The resolver is responsible for converting the Abstract Syntax Tree (AST)
-- into a Low-Level Tree (LLT), resolving namespace conflicts, desugaring, etc.
type Resolver = State ResolverState

data ResolverState
    = RS {
        -- | The name of the module where the resolution is taking place.
        rsModuleName :: ByteString,
        rsModulePath :: FilePath,
        -- | A list of definitions
        rsDefns :: [Definition]
        -- -- | A map of names to their associated definition.
        -- rsDefns :: M.Map Name Definition
    }

runResolver :: ByteString -> FilePath -> Resolver a -> a
runResolver mdl path = flip evalState (RS mdl path mempty)

moduleName :: Resolver ByteString
moduleName = gets rsModuleName

modulePath :: Resolver FilePath
modulePath = gets rsModulePath

definitionList :: Resolver [Definition]
definitionList = gets rsDefns

emitDefn :: Definition -> Resolver ()
emitDefn defn = modify $ \rs ->rs { rsDefns = (defn:rsDefns rs) }

-- emitDefn :: Name -> Definition -> Resolver ()
-- emitDefn name defn = modify $ \(rs@(RS _mdl defs)) ->
--     rs { rsDefns = M.insert name defn defs }

instance MonadFail Resolver where
    fail = error
