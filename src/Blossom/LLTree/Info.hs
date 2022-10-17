module Blossom.LLTree.Info (
    Entry(..),
    Header(..),
    Info(..),
    isStaticInfo,
) where

import Blossom.LLTree.Closure (ClosureType, isStaticClosType)

newtype Header = Header Info

data Info = Info {
    infoClosType :: ClosureType,
    -- | Code that *runs* the object that this "info" belongs to.
    infoEntry :: Maybe Entry
    }

-- | Typically runs whatever its 'attached' to. For example,
-- if 'attached' to a `@FuncApp@`, it will apply the
-- arguments to the function and run it.
data Entry = Entry


-- | Checks if the info table is for a static object
isStaticInfo :: Info -> Bool
isStaticInfo (Info typ _entry) = isStaticClosType typ
