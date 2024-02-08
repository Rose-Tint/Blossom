module Blossom.Config.Config (
    Config(..),
    defaultConfig,
) where

import Blossom.Config.Verbosity (Verbosity(Normal))
import Blossom.Common.OStream (OStream, stdout)
import Prettyprinter (PageWidth(..), LayoutOptions(..))


data Config
    = Config {
        cnfVerbosity :: Verbosity,
        cnfOStream :: OStream,
        cnfDocLayout :: LayoutOptions
    }

defaultConfig :: Config
defaultConfig = Config {
    cnfVerbosity = Normal,
    cnfOStream = stdout,
    cnfDocLayout = LayoutOptions (AvailablePerLine 70 1.0)
    }
