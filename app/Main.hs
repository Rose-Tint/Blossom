module Main (
    main
) where

import Blossom.Cmd


main :: IO ()
main = do
    _cmd <- parseCmdLine
    return ()
