{-# LANGUAGE OverloadedStrings #-}

module Blossom.Package.Properties (
    Properties(..),
    emptyProperties,
) where

import Blossom.Package.Version (Version(..), newVersion)
import Data.ByteString (ByteString)


data Properties
    = Properties {
        propsName :: ByteString,
        propsVersion :: Version
    }

emptyProperties :: Properties
emptyProperties = Properties {
    propsName = error "'name' is a required package-property",
    propsVersion = newVersion
}
