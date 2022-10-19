module Main (
    main,
) where

import Blossom.Monad (
    Blossom,
    liftIO,
    runBlossom,
    getCmd,
    message,
    printError,
    )
import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.Common.Name.Module (fromFilePath)
import Blossom.Parsing.Parser (parseFile)
import Blossom.Resolver.Monad (runResolverT)
import Blossom.Resolver.Resolver (resolveAST)
import Prettyprinter (pretty, line)


main :: IO ()
main = do
    cmd <- parseCmdLine
    runBlossom cmd main'
    return ()

main' :: Blossom ()
main' = do
    sourceFiles <- getCmd cmdSourceFiles
    mapM_ (\path -> do
        let mdl = fromFilePath path
        eAst <- liftIO $ parseFile path mdl
        case eAst of
            Left errMsg -> printError (pretty errMsg)
            Right !ast -> do
                eLlt <- runResolverT mdl path (resolveAST ast)
                case eLlt of
                  Left err -> printError (pretty err)
                  Right _llt -> return ()
                return ()
        ) sourceFiles
    message (pretty "Done!" <> line)
    return ()
