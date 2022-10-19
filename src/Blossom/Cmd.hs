module Blossom.Cmd (
    CmdLine(..),
    Verbosity(..),
    parseCmdLine,
) where

import Options.Applicative (
    strArgument,
    short,
    metavar,
    long,
    info,
    helper,
    help,
    fullDesc,
    execParser,
    Alternative(many),
    Parser,
    ParserInfo,
    maybeReader,
    option,
    ReadM, value,
    )
import Data.Char (toLower, digitToInt, isDigit)


data Verbosity
    = Silent
    | ErrorsOnly
    | Normal
    | Verbose
    deriving (Show, Eq, Ord, Enum)

data CmdLine = CmdLine {
    cmdSourceFiles :: [FilePath],
    cmdVerbosity :: Verbosity
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
    <*> option verbosityReader (
            short 'v'
            <> long "verbose"
            <> help "Enable more detailed output"
            <> value Normal
        )

verbosityReader :: ReadM Verbosity
verbosityReader = maybeReader go
    where
        readNum [] = Nothing
        readNum (ch : chs)
            | not (isDigit ch) = Nothing
            | null chs = Just (digitToInt ch)
            | otherwise = (digitToInt ch +) . (* 10) <$> readNum chs
        go str = case map toLower str of
            "silent" -> Just Silent
            "errors" -> Just ErrorsOnly
            "normal" -> Just Normal
            "verbose" -> Just Verbose
            _ -> toEnum <$> readNum str

