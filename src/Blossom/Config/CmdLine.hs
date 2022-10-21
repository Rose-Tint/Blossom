module Blossom.Config.CmdLine (
    CmdLine(..),
    parseCmdLine,
) where

import Blossom.Config.Verbosity
import Options.Applicative

-- | Represents options provided by command-line arguments.
--
-- Note: Most of the types of the fields use `@Maybe@` to show whether or not
-- an argument was actually provided. This is helpful when applying them
-- to their respective `@Config@` settings.
data CmdLine = CmdLine {
    cmdSourceFiles :: [FilePath],
    cmdVerbosity :: Maybe Verbosity,
    cmdOStream :: Maybe FilePath
    }
    deriving (Show, Eq)


parseCmdLine :: IO CmdLine
parseCmdLine = execParser cmdLineParserInfo

cmdLineParserInfo :: ParserInfo CmdLine
cmdLineParserInfo = info (helper <*> cmdLineParser) fullDesc

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> many (strArgument (
            metavar "FILES"
        ))
    <*> optional (
            flag' Silent (
                short 's'
                <> long "silent"
                <> help "Disable ALL output"
            )
        <|>
            flag' Verbose (
                short 'v'
                <> long "verbose"
                <> help "Enable more detailed output"
            )
        <|>
            flag' ErrorsOnly (
                long "errors-only"
                <> help "Only output error messages"
            )
        )
    <*>
        optional (option str (
            long "output-file"
            <> metavar "FILE"
            -- <> value "stdout"
            <> help "Path to the file to output to"
        ))
