module Blossom.Cmd (
    CommandLine(..),
    Verbosity(..),
    parseCommandLine,
) where

import Options.Applicative


data CommandLine = CommandLine {
    cmdSourceFiles :: [FilePath]
    }


parseCommandLine :: IO CommandLine
parseCommandLine = execParser commandLineInfo

commandLineInfo :: ParserInfo CommandLine
commandLineInfo = info
    (helper <*> commandLine)
    fullDesc

commandLine :: Parser CommandLine
commandLine = CommandLine
    <$> many (strArgument mempty)
