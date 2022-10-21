module Blossom.Config (
    Config(..),
    readConfiguration,
    defaultConfig,
) where

import Blossom.Config.Verbosity (Verbosity(Normal))
import Data.List (union)
import Blossom.Config.CmdLine (CmdLine(..), parseCmdLine)
import Data.Maybe (fromMaybe)
import Blossom.Common.OStream (OStream, stdout, fileStream)
import Prettyprinter (LayoutOptions(..), PageWidth(..))


data Config
    = Config {
        cnfSourceFiles :: [FilePath],
        cnfVerbosity :: Verbosity,
        cnfOStream :: OStream,
        cnfDocLayout :: LayoutOptions
    }

defaultConfig :: Config
defaultConfig = Config {
    cnfSourceFiles = [],
    cnfVerbosity = Normal,
    cnfOStream = stdout,
    cnfDocLayout = LayoutOptions (AvailablePerLine 70 1.0)
    }

readConfiguration :: IO Config
readConfiguration = do
    let config = defaultConfig
    -- TODO: read global config file
    -- TODO: read local config file
    -- TODO: read environment vars
    cmdline <- parseCmdLine
    config' <- applyCmdLine cmdline config
    return $! config'

applyCmdLine :: CmdLine -> Config -> IO Config
applyCmdLine cmd cnf = do
    ostream <- case cmdOStream cmd of
        Nothing -> return $! cnfOStream cnf
        Just filepath -> fileStream filepath
    return $ cnf {
        cnfSourceFiles = cmdSourceFiles cmd `union` cnfSourceFiles cnf,
        cnfVerbosity = fromMaybe (cnfVerbosity cnf) (cmdVerbosity cmd),
        cnfOStream = ostream
    }
