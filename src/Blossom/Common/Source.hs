{-# LANGUAGE OverloadedStrings #-}

module Blossom.Common.Source (
    Line,
    Column,
    Offset,
    SourceLoc(..),
    Position(..),
    HasLoc(..),
    mergeLocs,
    mkPos,
    unknownLoc,
    testLoc,
) where

import Blossom.Common.Name.Module (ModuleName)
import Data.Ord (comparing)
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
    | UnknownLoc {
        locReason :: String
    }

-- | Represents a position *within* a module. Does not contain module or file
-- information because it is not meant to be used outside of `@SourceLoc@`.
data Position
    = Pos {
        posLine :: Line,
        posColumn :: Column,
        posOffset :: Offset
    }
    deriving (Eq)

class HasLoc a where
    getLoc :: a -> SourceLoc

-- | Merges source locations so that the beginning and ending of the resulting
-- `@SourceLoc@` is the earliest and latest of the source locations of the
-- given values, respectively.
mergeLocs :: (HasLoc a, HasLoc b) => a -> b -> SourceLoc
mergeLocs a b = getLoc a <> getLoc b

mergeSourceLocs :: SourceLoc -> SourceLoc -> SourceLoc
mergeSourceLocs (SourceLoc p1 m1 b1 e1) (SourceLoc p2 m2 b2 e2)
    | p1 /= p2 = UnknownLoc "paths did not match."
    | m1 /= m2 = UnknownLoc "module names did not match."
    | otherwise = SourceLoc p1 m1 (min b1 b2) (max e1 e2)
mergeSourceLocs (UnknownLoc rsn1) (UnknownLoc rsn2) = UnknownLoc $
    rsn1 ++ "; " ++ rsn2
mergeSourceLocs UnknownLoc{} loc = loc
mergeSourceLocs loc UnknownLoc{} = loc

mkPos :: (Integral l, Integral c, Integral o) => l -> c -> o -> Position
mkPos l c o = Pos (fromIntegral l) (fromIntegral c) (fromIntegral o)

unknownLoc :: String -> SourceLoc
unknownLoc = UnknownLoc

-- | SHOULD ONLY BE USED FOR TESTING
testLoc :: SourceLoc
testLoc = UnknownLoc testLocRsn

-- | The reason given for 'unknown' locations used for testing purposes
testLocRsn :: String
testLocRsn = "TEST"

instance Show SourceLoc where
    show (SourceLoc path mdl begin end) = "SourceLoc " ++
        show path ++ " (" ++ show mdl ++ ") (" ++
        show begin ++ ") (" ++ show end ++ ")"
    show (UnknownLoc rsn) = "UnknownLoc " ++ show rsn

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

instance HasLoc SourceLoc where
    getLoc = id

instance HasLoc a => HasLoc [a] where
    getLoc [] = error "getLoc: empty list"
    getLoc xs = getLoc (head xs) <> getLoc (last xs)

instance Semigroup SourceLoc where
    (<>) = mergeSourceLocs

instance Monoid SourceLoc where
    mempty = SourceLoc mempty mempty (Pos 0 0 0) (Pos 0 0 0)

instance Pretty SourceLoc where
    pretty (UnknownLoc rsn) = "?(" <> pretty rsn <> ")"
    pretty (SourceLoc path _mdl begin _end) = pretty path <> ":" <> pretty begin

instance Show Position where
    show (Pos ln col off) = "Pos " ++
        show ln ++ " " ++ show col ++ " " ++ show off

instance Ord Position where
    compare = comparing posOffset

instance Pretty Position where
    pretty (Pos ln col _off) = pretty ln <> ":" <> pretty col
