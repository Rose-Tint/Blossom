module Blossom.Package.Component.GenOpts (
    GenOpts(..),
    IsCmpnt(..),
    newGenOpts,
    getCmpntName,
    getCmpntTargets,
    getCmpntSourceDirs,
    getCmpntDepends,
    setCmpntName,
    setCmpntTargets,
    setCmpntSourceDirs,
    setCmpntDepends,
) where

import Blossom.Package.Target (Target)
import Data.ByteString (ByteString)
import Blossom.Common.Name.Ident (Ident)


-- | Options that may appear in any type of component
data GenOpts
    = GenOpts {
        genoptName :: Ident,
        genoptTargets :: [Target],
        genoptSourceDirs :: [FilePath],
        genoptDepends :: [ByteString]
    }

newGenOpts :: GenOpts
newGenOpts = GenOpts {
    genoptName = error "'name' is a required field",
    genoptTargets = [],
    genoptSourceDirs = [],
    genoptDepends = []
    }

-- |This class makes it easy to access fields of `@BuildOptions@` despite
-- indirection.
class IsCmpnt a where
    getGenOpts :: a -> GenOpts
    modifyGenOpts :: (GenOpts -> GenOpts) -> a -> a

getCmpntName :: IsCmpnt a => a -> Ident
getCmpntName = genoptName . getGenOpts

getCmpntTargets :: IsCmpnt a => a -> [Target]
getCmpntTargets = genoptTargets . getGenOpts

getCmpntSourceDirs :: IsCmpnt a => a -> [FilePath]
getCmpntSourceDirs = genoptSourceDirs . getGenOpts

getCmpntDepends :: IsCmpnt a => a -> [ByteString]
getCmpntDepends = genoptDepends . getGenOpts

setCmpntName :: IsCmpnt a => Ident -> a -> a
setCmpntName name = modifyGenOpts (\o -> o { genoptName = name })

setCmpntTargets :: IsCmpnt a => [Target] -> a -> a
setCmpntTargets ts = modifyGenOpts (\o -> o { genoptTargets = ts })

setCmpntSourceDirs :: IsCmpnt a => [FilePath] -> a -> a
setCmpntSourceDirs dirs = modifyGenOpts (\o -> o { genoptSourceDirs = dirs })

setCmpntDepends :: IsCmpnt a => [ByteString] -> a -> a
setCmpntDepends deps = modifyGenOpts (\o -> o { genoptDepends = deps })

instance IsCmpnt GenOpts where
    getGenOpts = id
    modifyGenOpts f o = f o
