module Main (
    main
) where

import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.Parsing.Parser (parseFile)
import Blossom.Resolver.Monad (runResolver)
import Data.ByteString.Char8 (pack)
import Blossom.Resolver.Resolver (resolveAST)


main :: IO ()
main = do
    cmd <- parseCmdLine
    let sourceFiles = cmdSourceFiles cmd
    mapM_ (\path -> do
        let mdl = pack path
        eAst <- parseFile path mdl
        case eAst of
            Left errMsg -> putStrLn errMsg
            Right !ast -> do
                let !_ = runResolver mdl path (resolveAST ast)
                return ()
        ) sourceFiles
