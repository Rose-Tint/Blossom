module Blossom.Cmd (
    CmdLine(..),
    parseCmdLine,
) where

import Options.Applicative


data CmdLine = CmdLine {
    cmdSourceFiles :: [FilePath]
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
