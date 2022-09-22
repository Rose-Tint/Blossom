module Main (
    main
) where

import Blossom.Cmd (parseCmdLine)


main :: IO ()
main = do
    _cmd <- parseCmdLine
    return ()
