{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Source (
    Line,
    Column,
    Offset,
    SourceLoc(..),
    Position(..),
    HasLocation(..),
    mergeSourceLocs,
    mergeSourceLocs',
    getSourceLines,
    mkLine,
    mkColumn,
    mkOffset,
    zeroLoc,
    zeroPos,
    mkPos,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (
    splitAt,
    concat,
    takeWhile,
    takeWhileEnd,
    )
import Data.Ord (comparing)
import Prettyprinter (Pretty(pretty))


newtype Line = Line { unLine :: Word }
    deriving (Show, Eq, Ord, Enum, Num, Pretty)

newtype Column = Column { unColumn :: Word }
    deriving (Show, Eq, Ord, Enum, Num, Pretty)

newtype Offset = Offset { unOffset :: Word }
    deriving (Show, Eq, Ord, Enum, Num)

-- | A span from one positon to the next, along with the path to and name of
-- the containing module.
data SourceLoc
    = SourceLoc {
        locPath :: FilePath,
        locModule :: ByteString,
        locBegin :: Position,
        locEnd :: Position
    }
    deriving (Show, Eq)

-- | Represents a position *within* a module. Does not contain module or file
-- information because it is not meant to be used outside of `@SourceLoc@`.
data Position
    = Pos {
        posLine :: Line,
        posColumn :: Column,
        posOffset :: Offset
    }
    deriving (Show, Eq)

instance Ord Position where
    compare = comparing posOffset

class HasLocation a where
    getLoc :: a -> SourceLoc

instance HasLocation SourceLoc where
    getLoc = id

-- | Merges source locations so that the beginning and ending of the resulting
-- `@SourceLoc@` is the earliest and latest, respectively, of the two given
-- `@SourceLoc@`s.
mergeSourceLocs :: SourceLoc -> SourceLoc -> Either String SourceLoc
mergeSourceLocs (SourceLoc p1 m1 b1 e1) (SourceLoc p2 m2 b2 e2)
    | p1 /= p2 = Left "paths did not match while merging locations."
    | m1 /= m2 = Left "module names did not match while merging locations."
    | otherwise = Right $ SourceLoc p1 m1 (min b1 b2) (max e1 e2)

-- | In the event of a conflict between the module name or path,
-- this version of the function will simply prefer the first one. Please prefer
-- to use `@mergeSourceLocs@` over this one.
mergeSourceLocs' :: SourceLoc -> SourceLoc -> SourceLoc
mergeSourceLocs' (SourceLoc path mdl b1 e1) (SourceLoc _p2 _m2 b2 e2) =
    SourceLoc path mdl (min b1 b2) (max e1 e2)

-- | Extracts all of the lines that contain the location
getSourceLines :: ByteString -> SourceLoc -> ByteString
getSourceLines bs (SourceLoc _path _mdl begin end) =
    let (preBeginSplit, postBeginSplit) = flip BS.splitAt bs $
            fromEnum (posOffset begin)
        -- `preBegin` is the part after the last newline before the offset of
        -- `begin`
        preBegin = BS.takeWhileEnd (/= '\n') preBeginSplit
        (between, postEndSplit) = flip BS.splitAt postBeginSplit $
            fromEnum (posOffset end - posOffset begin)
        -- `preEnd` is the part before the first newline after the offset of
        -- `end`
        postEnd = BS.takeWhile (/= '\n') postEndSplit
    in BS.concat [preBegin, between, postEnd]

mkPos :: (Integral l, Integral c, Integral o) => l -> c -> o -> Position
mkPos l c o = Pos (fromIntegral l) (fromIntegral c) (fromIntegral o)

mkLine :: Integral n => n -> Line
mkLine = Line . fromIntegral

mkColumn :: Integral n => n -> Column
mkColumn = Column . fromIntegral

mkOffset :: Integral n => n -> Offset
mkOffset = Offset . fromIntegral

zeroPos :: Position
zeroPos = Pos 0 0 0

zeroLoc :: SourceLoc
zeroLoc = SourceLoc mempty mempty zeroPos zeroPos

instance Pretty SourceLoc where
    pretty (SourceLoc path _mdl begin _end) = pretty path <> ":" <> pretty begin

instance Pretty Position where
    pretty (Pos ln col _off) = pretty ln <> ":" <> pretty col
