module Main (
    main,
    tests,
) where

import qualified Blossom.Common.NameTest as NameTest (tests)
import qualified Blossom.Common.Name.ModuleTest as ModuleNameTest (tests)
import qualified Blossom.Parsing.LexerTest as LexerTest (tests)
import qualified Blossom.Parsing.ParserTest as ParserTest (tests)
import Test.HUnit (Test(TestList), runTestTTAndExit)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    NameTest.tests,
    ModuleNameTest.tests,
    LexerTest.tests,
    ParserTest.tests
    ]
