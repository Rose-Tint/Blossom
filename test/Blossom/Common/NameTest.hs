{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.NameTest (
    tests,
) where

import Test.HUnit
import Blossom.Common.Name (
    Name(..),
    fromQualified,
    catIdent,
    display
    )


tests :: Test
tests = TestLabel "Blossom.Common.NameTest" $ TestList [
    TestLabel "fromQualified" $ TestList [
        TestCase $
            let actual = fromQualified "Abc::Def::Ghi::jkl"
                expected = Name "Abc_Def_Ghi" "jkl"
            in expected @=? actual,
        TestCase $
            let actual = fromQualified "Abc::Def::ghi"
                expected = Name "Abc_Def" "ghi"
            in expected @=? actual,
        TestCase $
            let actual = fromQualified "Abc::def"
                expected = Name "Abc" "def"
            in expected @=? actual,
        TestCase $
            let actual = fromQualified "abc"
                expected = Name "" "abc"
            in expected @=? actual
        ],
    TestLabel "catIdent" $ TestCase $
            let actual = catIdent (Name "Abc_Def_Ghi" "jkl") "mnop"
                expected = Name "Abc_Def_Ghi" "jklmnop"
            in expected @=? actual,
    TestLabel "display" $ TestCase $
            let actual = display (Name "Abc_Def_Ghi" "jklmnop")
                expected = "Abc_Def_Ghi_jklmnop"
            in expected @=? actual
    ]
