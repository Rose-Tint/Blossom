{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.NameTest (
    tests,
) where

import Test.HUnit ((@=?), Test(..))
import Blossom.Common.Name (
    display,
    Ident(Ident),
    Name(Name),
    NameType(External, Internal),
    internal,
    )
import Blossom.Common.Source (testLoc)
import Data.String (fromString)


tests :: Test
tests = TestLabel "Blossom.Common.NameTest" $ TestList [
    TestLabel "internal" $ TestList [
        TestCase $
            let actual = internal $ testIdent "Abc::Def::ghi"
                expected = Name (External "Abc::Def") "ghi" testLoc
            in expected @=? actual,
        TestCase $
            let actual = internal $ testIdent "Abc::def"
                expected = Name (External "Abc") "def" testLoc
            in expected @=? actual,
        TestCase $
            let actual = internal $ testIdent "abc"
                expected = Name Internal "abc" testLoc
            in expected @=? actual
        ],
    TestLabel "display" $
        TestCase $
            let actual = display (Name (External "Abc::Def") "ghi" testLoc)
                expected = "Abc::Def::ghi"
            in expected @=? actual
    ]

testIdent :: String -> Ident
testIdent iden = Ident (fromString iden) testLoc
