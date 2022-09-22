module Blossom.Rename (
    runRename,
) where

import Control.Monad.State (State, gets, modify, evalState)
import Data.ByteString (ByteString, append)

import Blossom.Common.Name (Name(..))
import Blossom.Typing.Type (Type(..))


data RenamerState = RenamerState {
    stateCounter :: Word,
    stateModule :: ByteString
    }

type Renamer = State RenamerState


class Rename a where
    rename :: a -> Renamer a

instance Rename Type where
    rename (TypeCon name args) = do
        name' <- prependModule name
        args' <- mapM rename args
        return (TypeCon name' args')
    rename (t1 :-> t2) = do
        t1' <- rename t1
        t2' <- rename t2
        return (t1' :-> t2')


newState :: ByteString -> RenamerState
newState = RenamerState 01

runRename :: Rename a => ByteString -> a -> a
runRename modName = flip evalState (newState modName) . rename

prependModule :: Name -> Renamer Name
prependModule (Name name) = do
    modName <- gets stateModule
    let name' = append name modName
    return (Name name')
prependModule _ = freshId

freshId :: Renamer Name
freshId = do
    n <- gets stateCounter
    let name = Id n
    modify (\s -> s { stateCounter = n + 1 })
    return name
