module Blossom.Common.Name (
    Name(..),
    mkName,
) where

import Data.ByteString (ByteString)
import Data.String (IsString(fromString))


data Name
    = Name ByteString
    | Id Word
    deriving (Show, Eq, Ord)

instance IsString Name where
    fromString = Name . fromString

mkName :: String -> Name
mkName = Name . fromString
