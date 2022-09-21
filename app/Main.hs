module Main (
    main
) where

import Blossom.Cmd
import Blossom.Backend.Cpp (transpileFile)


main :: IO ()
main = do
    cmd <- parseCmdLine
    case cmdBackend cmd of
        Cpp -> mapM_ transpileFile (cmdSourceFiles cmd)
        LLVM -> error "llvm backend not yet implemented"
    return ()
