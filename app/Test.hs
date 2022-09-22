module Main (
    main
) where


import Test.HUnit

import qualified Blossom.CmdTest
import qualified Blossom.Parsing.LexerTest
import qualified Blossom.Parsing.ParserTest
import qualified Blossom.RenameTest


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    Blossom.CmdTest.tests,
    Blossom.Parsing.LexerTest.tests,
    Blossom.Parsing.ParserTest.tests,
    Blossom.RenameTest.tests
    ]
