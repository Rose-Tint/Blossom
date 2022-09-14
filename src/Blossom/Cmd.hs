module Blossom.Cmd (
    CommandLine(..),
    parseCommandLine,
) where

import Options.Applicative


data CommandLine = CommandLine {
    cmdSourceFiles :: [FilePath]
    }


parseCommandLine :: IO CommandLine
parseCommandLine = execParser commandLineParserInfo

commandLineParserInfo :: ParserInfo CommandLine
commandLineParserInfo = info
    (helper <*> commandLineParser)
    fullDesc

commandLineParser :: Parser CommandLine
commandLineParser = CommandLine
    <$> many (strArgument mempty)
