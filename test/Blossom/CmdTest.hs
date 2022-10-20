module Blossom.CmdTest (
    tests,
) where

import Test.HUnit.Base
import Blossom.Cmd
import System.Environment (withArgs)


tests :: Test
tests = TestLabel "Blossom.Cmd" $ TestList [
    TestLabel "verbosity" $ TestList [
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' []
            Normal @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["-v"]
            Verbose @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbose"]
            Verbose @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["-s"]
            Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--silent"]
            Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity=0"]
            Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity=1"]
            ErrorsOnly @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity=2"]
            Normal @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity=3"]
            Verbose @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity", "0"]
            Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity", "1"]
            ErrorsOnly @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity", "2"]
            Normal @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbosity", "3"]
            Verbose @=? actual
        ],
    TestCase $ do
        actual <- parseCmdLine' [
            "path/to/some/module.bl",
            "path/to/diff/module.bl",
            "some/path/to/another.bl",
            "-v"
            ]
        let expected = CmdLine {
                cmdSourceFiles = [
                    "path/to/some/module.bl",
                    "path/to/diff/module.bl",
                    "some/path/to/another.bl"
                    ],
                cmdVerbosity = Verbose
                }
        expected @=? actual
    ]


parseCmdLine' :: [String] -> IO CmdLine
parseCmdLine' = flip withArgs parseCmdLine
