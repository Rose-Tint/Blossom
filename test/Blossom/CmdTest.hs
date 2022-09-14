module Blossom.CmdTest (
    tests,
) where

import Test.HUnit

-- import Blossom.Cmd


tests :: Test
tests = TestLabel "Blossom.Cmd" $ TestList [
    TestLabel "parseCmdLine" $ TestList [
        -- TestCase $ parseCmdLine ~=?
        ]
    ]
