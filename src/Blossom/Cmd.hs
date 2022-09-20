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
    cmdVerbose :: Bool,
    cmdBackend :: Backend
    }


parseCmdLine :: IO CmdLine
parseCmdLine = execParser cmdLineParserInfo

cmdLineParserInfo :: ParserInfo CmdLine
cmdLineParserInfo = info
    (helper <*> cmdLineParser)
    fullDesc

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> some (strArgument mempty)
    <*> switch (
            short 'v'
            <> long "verbose"
            <> help "Enable more detailed output"
        )
    <*> flag LLVM Cpp (
            long "cpp"
            <> help "Use C++ as the backend (transpile to C++)"
        )
