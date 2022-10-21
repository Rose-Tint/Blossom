{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
) where

import Blossom.Monad (
    Blossom,
    liftIO,
    runBlossom,
    getCnf,
    message',
    printError,
    )
import Blossom.Config (cnfSourceFiles, readConfiguration)
import Blossom.Common.Name.Module (fromFilePath)
import Blossom.Parsing.Parser (parseModuleFile)
import Prettyprinter (pretty, (<+>), brackets)


main :: IO ()
main = do
    config <- readConfiguration
    runBlossom config main'
    return ()

main' :: Blossom ()
main' = do
    sourceFiles <- getCnf cnfSourceFiles
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
