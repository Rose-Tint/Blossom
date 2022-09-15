module Blossom.Cmd (
    CmdLine(..),
    parseCmdLine,
) where

import Options.Applicative


data CmdLine = CmdLine {
    cmdSourceFiles :: [FilePath],
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
    <$> many (strArgument mempty)
    <*> switch (
        short 'v'
        <> long "verbose"
        <> help "Enable more detailed output"
        )
