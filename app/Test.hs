module Main (
    main,
    tests,
) where

import qualified Blossom.LLVM.ToLLVMTest as ToLLVMTest (tests)
import qualified Blossom.Common.NameTest as NameTest (tests)
import qualified Blossom.Parsing.LexerTest as LexerTest (tests)
import qualified Blossom.Parsing.ParserTest as ParserTest (tests)
import Test.HUnit (Test(TestList), runTestTTAndExit)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    ToLLVMTest.tests,
    NameTest.tests,
    LexerTest.tests,
    ParserTest.tests
    ]
