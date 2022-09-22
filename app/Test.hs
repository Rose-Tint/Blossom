module Main (
    main
) where


import Test.HUnit (Test(TestList), runTestTTAndExit)

import qualified Blossom.Parsing.LexerTest (tests)
import qualified Blossom.Parsing.ParserTest (tests)
import qualified Blossom.RenameTest (tests)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    Blossom.Parsing.LexerTest.tests,
    Blossom.Parsing.ParserTest.tests,
    Blossom.RenameTest.tests
    ]
