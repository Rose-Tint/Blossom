module Blossom.Parsing.Token (
    Token(..),
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
    | TokEnd
