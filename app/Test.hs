module Main (
    main,
    tests,
) where


import Test.HUnit (Test(TestList), runTestTTAndExit)

import qualified Blossom.LLVM.ToLLVMTest (tests)
import qualified Blossom.Common.NameTest (tests)
import qualified Blossom.Parsing.LexerTest (tests)
import qualified Blossom.Parsing.ParserTest (tests)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    Blossom.LLVM.ToLLVMTest.tests,
    Blossom.Common.NameTest.tests,
    Blossom.Parsing.LexerTest.tests,
    Blossom.Parsing.ParserTest.tests
    ]
