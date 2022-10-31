module Blossom.Config (
    Config(..),
    readConfiguration,
) where

import Blossom.Config.Config (Config(..), defaultConfig)
import Blossom.Config.CmdLine (applyCmdLine)


readConfiguration :: IO Config
readConfiguration = do
    let config = defaultConfig
    -- TODO: read global config file
    -- TODO: read local config file
    -- TODO: read environment vars
    config' <- applyCmdLine config
    return $! config'
