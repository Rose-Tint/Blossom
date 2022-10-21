module Main (
    main,
    tests,
) where

import qualified Blossom.Common.NameTest as Common.Name (tests)
import qualified Blossom.Common.Name.ModuleTest as Common.Name.ModuleName (tests)
import qualified Blossom.Config.CmdLineTest as Config.CmdLine (tests)
import qualified Blossom.Parsing.LexerTest as Parsing.Lexer (tests)
import qualified Blossom.Parsing.ParserTest as Parsing.Parser (tests)
import Test.HUnit (Test(TestList), runTestTTAndExit)


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    Common.Name.tests,
    Common.Name.ModuleName.tests,
    Config.CmdLine.tests,
    Parsing.Lexer.tests,
    Parsing.Parser.tests
    ]
