module Main (
    main
) where

import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.Parsing.Parser (parseFile)
import Data.Maybe (catMaybes)


main :: IO ()
main = do
    cmd <- parseCmdLine
    _asts <- catMaybes <$> mapM (\path -> do
        eAst <- parseFile path
        case eAst of
            Left errMsg -> do
                putStrLn errMsg
                return Nothing
            Right !ast -> do
                return (Just ast)
        ) (cmdSourceFiles cmd)
    return ()
