module Blossom.Rename (
    runRename,
) where

import qualified Control.Monad.State as S (State)
import Control.Monad.State hiding (State)
import qualified Data.ByteString as BS (append)
import Data.ByteString (ByteString)

import Blossom.Common.Name (Name(..))
import Blossom.Typing.Type (Type(..))


data State = State {
    stateCounter :: Word,
    stateModule :: ByteString
    }

type Renamer = S.State State


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


newState :: ByteString -> State
newState = State 01

runRename :: Rename a => ByteString -> a -> a
runRename modName = flip evalState (newState modName) . rename

prependModule :: Name -> Renamer Name
prependModule (Name name) = do
    modName <- gets stateModule
    let name' = BS.append name modName
    return (Name name')
prependModule _ = freshId

freshId :: Renamer Name
freshId = do
    n <- gets stateCounter
    let name = Id n
    modify (\s -> s { stateCounter = n + 1 })
    return name
