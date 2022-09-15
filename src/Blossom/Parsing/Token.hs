module Blossom.Parsing.Token (
    Token(..),
    mkSmallId,
    mkBigId,
) where

import qualified Data.ByteString.Lazy as BS
import qualified LLVM.AST.Name as LLVM


data Token
    = TokInteger Int
    | TokFloat Double
    | TokString BS.ByteString
    | TokOperator LLVM.Name
    | TokSmallId LLVM.Name
    | TokBigId LLVM.Name
    | TokSemi
    | TokColon
    | TokDoubleColon
    | TokArrow
    | TokEqArrow
    | TokImport
    | TokFunc
    | TokData
    | TokIf
    | TokThen
    | TokElse
    | TokEnd
    deriving (Show, Eq)


-- | Creates a `@TokSmallId@` using a `@String@`
mkSmallId :: String -> Token
mkSmallId = TokSmallId . LLVM.mkName

-- | Creates a `@TokBigId@` using a `@String@`
mkBigId :: String -> Token
mkBigId = TokBigId . LLVM.mkName

