module Blossom.Typing.TypeEnv (
    TypeEnv,
) where

import Blossom.Common.Name (Name)
import Blossom.Typing.Type (Type)
import qualified Data.Map as M


type TypeEnv = M.Map Name Type
