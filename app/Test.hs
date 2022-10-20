module Main (
    main,
    tests,
) where

import qualified Blossom.CmdTest as Cmd (tests)
import qualified Blossom.Common.NameTest as Name (tests)
import qualified Blossom.Common.Name.ModuleTest as ModuleName (tests)
import qualified Blossom.Parsing.LexerTest as Lexer (tests)
import qualified Blossom.Parsing.ParserTest as Parser (tests)
import Test.HUnit (Test(TestList), runTestTTAndExit)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    Cmd.tests,
    Name.tests,
    ModuleName.tests,
    Lexer.tests,
    Parser.tests
    ]
