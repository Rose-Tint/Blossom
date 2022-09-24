module Blossom.LLTree.Info (
    Header(..),
    Info(..),
    Entry,
) where

import Blossom.LLTree.Closure (ClosureType)

newtype Header = Header Info

data Info = Info {
    infoClosType :: ClosureType,
    infoLayout :: (),
    -- | Code that *runs* the object this "info" belongs to.
    infoEntry :: Maybe Entry
    }

-- | Typically runs whatever its 'attached' to. For example,
-- if 'attached' to a `@FuncApp@`, it will apply the
-- arguments to the function and run it.
data Entry
