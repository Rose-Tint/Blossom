{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.NameTest (
    tests,
) where

import Test.HUnit ((@=?), Test(..))
import Blossom.Common.Name (
    display,
    Name(Name),
    NameType(External, Internal),
    internal,
    )
import Blossom.Common.Source (zeroLoc)


tests :: Test
tests = TestLabel "Blossom.Common.NameTest" $ TestList [
    TestLabel "internal" $ TestList [
        TestCase $
            let actual = internal "Abc::Def::ghi"
                expected = Name (External "Abc::Def") "ghi" zeroLoc
            in expected @=? actual,
        TestCase $
            let actual = internal "Abc::def"
                expected = Name (External "Abc") "def" zeroLoc
            in expected @=? actual,
        TestCase $
            let actual = internal "abc"
                expected = Name Internal "abc" zeroLoc
            in expected @=? actual
        ],
    TestLabel "display" $
        TestCase $
            let actual = display (Name (External "Abc::Def") "ghi" zeroLoc)
                expected = "Abc::Def::ghi"
            in expected @=? actual
    ]
