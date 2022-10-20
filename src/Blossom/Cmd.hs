module Blossom.Cmd (
    CmdLine(..),
    Verbosity(..),
    parseCmdLine,
) where

import Options.Applicative
import Data.Char (digitToInt, isDigit)


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
cmdLineParserInfo = info (helper <*> cmdLineParser) fullDesc

cmdLineParser :: Parser CmdLine
cmdLineParser = CmdLine
    <$> many (strArgument (
            metavar "files"
        ))
    <*> (
            flag' Silent (
                short 's'
                <> long "silent"
                <> help "Disable ALL output"
            )
        <|>
            flag Normal Verbose (
                short 'v'
                <> long "verbose"
                <> help "Enable more detailed output"
            )
        <|>
            option verbosityReader (
                short 'v'
                <> long "verbosity"
                <> value Normal
                <> help "Set the level of output detail"
            )
        )

verbosityReader :: ReadM Verbosity
verbosityReader = maybeReader ((toEnum <$>) . readNum)
    where
        readNum [] = Nothing
        readNum (ch : chs)
            | not (isDigit ch) = Nothing
            | null chs = Just (digitToInt ch)
            | otherwise = (digitToInt ch +) . (* 10) <$> readNum chs
