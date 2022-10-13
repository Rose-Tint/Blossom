module Main (
    main,
) where

import Blossom.Cmd (CmdLine(..), parseCmdLine)
import Blossom.LLTree.Module (ModuleLLT)
import Blossom.Parsing.Parser (parseFile)
import Blossom.Resolver.Monad (ResolverT, runResolverT)
import Blossom.Resolver.Resolver (resolveAST)
import Data.String (fromString)
import Prettyprinter (pretty)
import Prettyprinter.Util (putDocW)


main :: IO ()
main = do
    cmd <- parseCmdLine
    let sourceFiles = cmdSourceFiles cmd
    mapM_ (\path -> do
        let mdl = fromString path
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
