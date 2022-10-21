{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
) where

import Blossom.Monad (
    Blossom,
    liftIO,
    runBlossom,
    getCmd,
    message',
    printError,
    )
import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.Common.Name.Module (fromFilePath)
import Blossom.Parsing.Parser (parseModuleFile)
import Prettyprinter (pretty, (<+>), brackets)


main :: IO ()
main = do
    cmd <- parseCmdLine
    runBlossom cmd main'
    return ()

main' :: Blossom ()
main' = do
    sourceFiles <- getCmd cmdSourceFiles
    let fileCount = length sourceFiles
    mapM_ (\(n, path) -> do
        let mdl = fromFilePath path
        message' $ brackets (pretty n <+> "of" <+> pretty fileCount)
            <+> "Compiling" <+> pretty mdl
        eAst <- liftIO $ parseModuleFile path mdl
        case eAst of
            Left errMsg -> printError (pretty errMsg)
            Right !_ast -> return ()
        ) (zip [1..fileCount] sourceFiles)
