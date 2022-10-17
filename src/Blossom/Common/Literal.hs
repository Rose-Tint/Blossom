module Blossom.Common.Literal (
    Literal(..),
) where

import Data.ByteString.Char8 (ByteString, unpack)
import Prettyprinter (Pretty(pretty), unsafeViaShow)


data Literal
    = IntLit Integer
    | FloatLit Double
    | CharLit Char
    | StringLit ByteString
    deriving (Show, Eq)

instance Pretty Literal where
    pretty (IntLit n) = pretty n
    -- convert n to a Float b/c prettyprinter does some goofy formatting with
    -- Doubles
    pretty (FloatLit n) = pretty (realToFrac n :: Float)
    pretty (CharLit ch) = unsafeViaShow ch
    pretty (StringLit bs) = unsafeViaShow (unpack bs)
