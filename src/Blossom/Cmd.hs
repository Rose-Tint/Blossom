module Blossom.Cmd (
    CmdLine(..),
    Backend(..),
    parseCmdLine,
) where

import Options.Applicative


data Backend
    = Cpp
    | LLVM

data CmdLine = CmdLine {
    cmdSourceFiles :: [FilePath],
    cmdBackend :: Backend,
    cmdVerbose :: Bool
    }


parseCmdLine :: IO CmdLine
parseCmdLine = execParser cmdLineParserInfo

cmdLineParserInfo :: ParserInfo CmdLine
cmdLineParserInfo = info
    (helper <*> cmdLineParser)
    fullDesc

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> many (strArgument (
            metavar "files"
        ))
    <*> flag LLVM Cpp (
            long "cpp"
            <> help "Use C++ as the backend (transpile to C++)"
        )
    <*> switch (
            short 'v'
            <> long "verbose"
            <> help "Enable more detailed output"
        )
