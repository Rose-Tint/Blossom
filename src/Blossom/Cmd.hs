module Blossom.Cmd (
    CmdLine(..),
    parseCmdLine,
) where

import Options.Applicative (
    Alternative(many),
    Parser,
    ParserInfo,
    execParser,
    fullDesc,
    help,
    helper,
    info,
    long,
    metavar,
    short,
    strArgument,
    switch,
    )


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
    <$> many (strArgument (
            metavar "files"
        ))
    <*> switch (
            short 'v'
            <> long "verbose"
            <> help "Enable more detailed output"
        )
