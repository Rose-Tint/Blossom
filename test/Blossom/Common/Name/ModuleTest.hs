{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Name.ModuleTest (
    tests,
) where

import Test.HUnit ((@=?), Test(..))
import Blossom.Common.Name.Module (
    ModuleName(..),
    fromFilePath,
    makeValid,
    )


tests :: Test
tests = TestLabel "Blossom.Common.NameTest" $ TestList [
    TestLabel "makeValid" $ TestList [
        TestCase $
            let actual = makeValid (MdlName "abc:::::deF::GHi")
                expected = MdlName "Abc::DeF::GHi"
            in expected @=? actual,
        TestCase $
            let actual = makeValid (MdlName "ab?c:$::D,ef::Gh\\i")
                expected = MdlName "AbC::DEf::GhI"
            in expected @=? actual,
        TestLabel "Idempotency" $ TestCase $
            let mdl = MdlName "Abc::Def::Ghi"
                actual = makeValid (makeValid (makeValid mdl))
                expected = makeValid mdl
            in expected @=? actual
        ],
    TestLabel "fromFilePath" $ TestList [
        TestCase $
            let actual = fromFilePath "abc/def.bl"
                expected = MdlName "Abc::Def"
            in expected @=? actual,
        TestCase $
            let actual = fromFilePath "/abc/def-ghi/"
                expected = MdlName "Abc::DefGhi"
            in expected @=? actual,
        TestCase $
            let actual = fromFilePath "././\\/Abc/dEF/../g-_hi.bl"
                expected = MdlName "Abc::DEF::GHi"
            in expected @=? actual
        ]
    ]
