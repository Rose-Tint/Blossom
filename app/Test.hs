module Main (
    main
) where


import Test.HUnit

import qualified Blossom.CmdTest
import qualified Blossom.CodeGenTest
import qualified Blossom.Parsing.LexerTest
import qualified Blossom.Parsing.ParserTest


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestLabel "Blossom Tests" $ TestList [
    Blossom.CmdTest.tests,
    Blossom.CodeGenTest.tests,
    Blossom.Parsing.LexerTest.tests,
    Blossom.Parsing.ParserTest.tests
    ]
