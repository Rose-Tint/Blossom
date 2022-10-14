module Main (
    main,
) where

import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.Common.Name.Module (fromFilePath)
import Blossom.LLTree.Module (ModuleLLT)
import Blossom.Parsing.Parser (parseFile)
import Blossom.Resolver.Monad (ResolverT, runResolverT)
import Blossom.Resolver.Resolver (resolveAST)
import Prettyprinter (pretty)
import Prettyprinter.Util (putDocW)


main :: IO ()
main = do
    cmd <- parseCmdLine
    let sourceFiles = cmdSourceFiles cmd
    mapM_ (\path -> do
        let mdl = fromFilePath path
        eAst <- parseFile path mdl
        case eAst of
            Left errMsg -> putStrLn errMsg
            Right !ast -> do
                -- putDocW 65 (pretty ast)
                let resolver = resolveAST ast :: ResolverT IO ModuleLLT
                eLlt <- runResolverT mdl path resolver
                case eLlt of
                  Left err -> putDocW 65 (pretty err)
                  Right _llt -> return ()
                return ()
        ) sourceFiles
