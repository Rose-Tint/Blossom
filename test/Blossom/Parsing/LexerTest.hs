module Blossom.Parsing.LexerTest (
    tests
) where

import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as BSC

import Blossom.Parsing.Lexer
import Blossom.Parsing.Token


tests :: Test
tests = TestLabel "Blossom.Parsing.Lexer" $ TestList [
    TestLabel "Function (tokenize)" $ TestList [
        TestLabel "integers (TokInteger):" $ TestCase $
            let actual = tokenize (BSC.pack "947235 3 51333332")
                expected = Right [
                    TokInteger 947235,
                    TokInteger 3,
                    TokInteger 51333332
                    ]
            in expected @=? actual,
        TestLabel "floats (TokFloat):" $ TestCase $
            let actual = tokenize (BSC.pack "3.14159 512.4 314.2634")
                expected = Right [
                    TokFloat 3.14159,
                    TokFloat 512.4,
                    TokFloat 314.2634
                    ]
            in expected @=? actual,
    -- | TokString BS.ByteString
    -- | TokOperator LLVM.Name
    -- | TokSmallId LLVM.Name
    -- | TokBigId LLVM.Name
    -- | TokSemi
    -- | TokColon
    -- | TokDoubleColon
    -- | TokArrow
    -- | TokEqArrow
    -- | TokEnd
        TestCase $
            let actual = tokenize (BSC.pack "aBc4' 123 45.6 Def'9")
                expected = Right [
                    mkSmallId "aBc4'",
                    TokInteger 123,
                    TokFloat 45.6,
                    mkBigId "Def'9"
                    ]
            in expected @=? actual,
        TestCase $
            let actual = tokenize (BSC.pack "aBc4' 123 45.6 Def'9")
                expected = Right [
                    mkSmallId "aBc4'",
                    TokInteger 123,
                    TokFloat 45.6,
                    mkBigId "Def'9"
                    ]
            in expected @=? actual
        ]
    ]
