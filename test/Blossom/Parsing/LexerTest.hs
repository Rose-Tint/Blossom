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
        TestLabel "integers" $ TestCase $
            let actual = tokenize (BSC.pack "947235 3 51333332")
                expected = Right [
                    TokInteger 947235,
                    TokInteger 3,
                    TokInteger 51333332
                    ]
            in expected @=? actual,
        TestLabel "floats" $ TestCase $
            let actual = tokenize (BSC.pack "3.14159 512.4 314.2634")
                expected = Right [
                    TokFloat 3.14159,
                    TokFloat 512.4,
                    TokFloat 314.2634
                    ]
            in expected @=? actual,
        TestLabel "strings" $ TestCase $
            -- TODO: allow escaped quotations
            let actual = tokenize (BSC.pack "\"foo\" \"bar\" \"\"")
                expected = Right [
                    TokString (BSC.pack "foo"),
                    TokString (BSC.pack "bar"),
                    TokString BSC.empty
                    ]
            in expected @=? actual,
        TestLabel "operators" $ TestList [], -- TODO
        TestLabel "small identifiers" $ TestCase $
            let actual = tokenize (BSC.pack "fo'0 bar' _FooBar")
                expected = Right [
                    mkSmallId "fo'0",
                    mkSmallId "bar'",
                    mkSmallId "_FooBar"
                    ]
            in expected @=? actual,
        TestLabel "big identifiers" $ TestCase $
            let actual = tokenize (BSC.pack "F0o Bar' Foo'Bar")
                expected = Right [
                    mkBigId "F0o",
                    mkBigId "Bar'",
                    mkBigId "Foo'Bar"
                    ]
            in expected @=? actual,
        -- TODO: (TokSemi, TokColon, TokDoubleColon, TokArrow, TokEqArrow)
        TestLabel "reserved operators and keywords" $ TestList [],
        TestLabel "mixed" $ TestList [
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
    ]
