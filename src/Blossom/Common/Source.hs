{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Source (
    Line,
    Column,
    Offset,
    SourceLoc(..),
    Position(..),
    HasLoc(..),
    mergeLocs,
    mergeSourceLocs,
    mergeSourceLocs',
    getSourceLines,
    zeroLoc,
    zeroPos,
    mkPos,
    unknownLoc,
    testLoc,
) where

import Blossom.Common.Name.Module (ModuleName)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS (
    splitAt,
    concat,
    takeWhile,
    takeWhileEnd,
    )
import Data.Ord (comparing)
import Data.String (fromString)
import Prettyprinter (Pretty(pretty))


type Line = Int

type Column = Int

type Offset = Int

-- | A span from one positon to the next, along with the path to and name of
-- the containing module.
data SourceLoc
    = SourceLoc {
        locPath :: FilePath,
        locModule :: ModuleName,
        locBegin :: Position,
        locEnd :: Position
    }
    -- | If a source location does not exist, a reason must be given.
    | UnknownLoc String
    deriving (Show)

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

class HasLoc a where
    getLoc :: a -> SourceLoc

instance HasLoc SourceLoc where
    getLoc = id

instance HasLoc a => HasLoc [a] where
    getLoc [] = error "getLoc: empty list"
    getLoc xs = mergeLocs (head xs) (last xs)

mergeLocs :: (HasLoc a, HasLoc b) => a -> b -> SourceLoc
mergeLocs a b = mergeSourceLocs (getLoc a) (getLoc b)

-- | Merges source locations so that the beginning and ending of the resulting
-- `@SourceLoc@` is the earliest and latest, respectively, of the two given
-- `@SourceLoc@`s.
mergeSourceLocs :: SourceLoc -> SourceLoc -> SourceLoc
mergeSourceLocs (SourceLoc p1 m1 b1 e1) (SourceLoc p2 m2 b2 e2)
    | p1 /= p2 = UnknownLoc "paths did not match while merging locations."
    | m1 /= m2 = UnknownLoc "module names did not match while merging locations."
    | otherwise = SourceLoc p1 m1 (min b1 b2) (max e1 e2)
mergeSourceLocs (UnknownLoc rsn1) (UnknownLoc rsn2) = UnknownLoc $
    rsn1 ++ "; " ++ rsn2
mergeSourceLocs UnknownLoc{} loc = loc
mergeSourceLocs loc UnknownLoc{} = loc

-- | In the event of a conflict between the module name or path,
-- this version of the function will simply prefer the first one. Please prefer
-- to use `@mergeSourceLocs@` over this one.
mergeSourceLocs' :: SourceLoc -> SourceLoc -> SourceLoc
mergeSourceLocs' (SourceLoc path mdl b1 e1) (SourceLoc _p2 _m2 b2 e2) =
    SourceLoc path mdl (min b1 b2) (max e1 e2)
mergeSourceLocs' (UnknownLoc rsn1) (UnknownLoc rsn2) = UnknownLoc $
    rsn1 ++ "; " ++ rsn2
mergeSourceLocs' UnknownLoc{} loc = loc
mergeSourceLocs' loc UnknownLoc{} = loc

-- | Extracts all of the lines that contain the location
getSourceLines :: ByteString -> SourceLoc -> ByteString
getSourceLines _ (UnknownLoc rsn) = fromString rsn
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

zeroPos :: Position
zeroPos = Pos 0 0 0

zeroLoc :: SourceLoc
zeroLoc = SourceLoc mempty mempty zeroPos zeroPos

unknownLoc :: String -> SourceLoc
unknownLoc = UnknownLoc

-- | SHOULD ONLY BE USED FOR TESTING
testLoc :: SourceLoc
testLoc = UnknownLoc testLocRsn

-- | The reason given for 'unknown' locations used for testing purposes
testLocRsn :: String
testLocRsn = "TEST"

instance Eq SourceLoc where
    SourceLoc p1 m1 b1 e1 == SourceLoc p2 m2 b2 e2 =
        p1 == p2 && m1 == m2 && b1 == b2 && e1 == e2
    -- two unknown locations should only ever compare equal if at least one of
    -- them is for testing.
    -- guards are used for readability
    UnknownLoc rsn1 == UnknownLoc rsn2
        | testLocRsn `elem` [rsn1, rsn2] = True
        | otherwise = False
    -- unknown locations should always compare inequal (unless its for testing)
    UnknownLoc rsn == _
        | rsn == testLocRsn = True
        | otherwise = False
    _ == UnknownLoc rsn
        | rsn == testLocRsn = True
        | otherwise = False

instance Pretty SourceLoc where
    pretty (UnknownLoc rsn) = "?(" <> pretty rsn <> ")"
    pretty (SourceLoc path _mdl begin _end) = pretty path <> ":" <> pretty begin

instance Pretty Position where
    pretty (Pos ln col _off) = pretty ln <> ":" <> pretty col
