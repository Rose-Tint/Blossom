module Blossom.Config.CmdLineTest (
    tests,
) where

import Test.HUnit.Base (Test(..), (@=?))
import Blossom.Config.CmdLine (CmdLine(..), parseCmdLine)
import Blossom.Config.Verbosity (Verbosity(..))
import System.Environment (withArgs)


tests :: Test
tests = TestLabel "Blossom.Config.CmdLine" $ TestList [
    TestLabel "verbosity" $ TestList [
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' []
            Nothing @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["-v"]
            Just Verbose @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--verbose"]
            Just Verbose @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["-s"]
            Just Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--silent"]
            Just Silent @=? actual,
        TestCase $ do
            actual <- cmdVerbosity <$> parseCmdLine' ["--errors-only"]
            Just ErrorsOnly @=? actual
        ],
    TestLabel "output-file" $ TestList [
        TestCase $ do
            actual <- cmdOStream <$> parseCmdLine' []
            Nothing @=? actual,
        TestCase $ do
            actual <- cmdOStream <$> parseCmdLine' ["--output-file=<stdout>"]
            Just "<stdout>" @=? actual,
        TestCase $ do
            actual <- cmdOStream <$> parseCmdLine' [
                "--output-file", "/logs/test.log"
                ]
            Just "/logs/test.log" @=? actual
        ],
    TestCase $ do
        actual <- parseCmdLine' ["-v"]
        let expected = CmdLine {
                cmdVerbosity = Just Verbose,
                cmdOStream = Nothing
                }
        expected @=? actual
    ]

parseCmdLine' :: [String] -> IO CmdLine
parseCmdLine' = flip withArgs parseCmdLine
