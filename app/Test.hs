module Main (
    main
) where


import Test.HUnit


main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestLabel "Blossom Tests" $ TestList []
