module Blossom.Common.Name.Module (
    ModuleName(..),
    fromFilePath,
    makeValid,
    mdlIsEmpty,
) where

import qualified Data.ByteString as BS (length)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.Char (toUpper, isAlpha, isAlphaNum)
import Data.List (dropWhileEnd)
import Data.String (IsString(fromString))
import Prettyprinter (Pretty(pretty))
import System.FilePath (
    dropExtensions,
    isPathSeparator,
    isExtSeparator,
    normalise,
    )


newtype ModuleName = MdlName ByteString
    deriving (Eq)

fromFilePath :: FilePath -> ModuleName
fromFilePath = makeValid . go . dropExtensions . normalise
    where
        go :: FilePath -> ModuleName
        go [] = mempty
        go path =
            let dropPred ch = isPathSeparator ch || isExtSeparator ch
                path' = dropWhile dropPred path
                (mdl, rest) = break isPathSeparator path'
                mdl' = MdlName (fromString mdl :: ByteString)
            in mdl' <> go rest

-- | This function is fairly expensive, so please try to avoid using it too
-- often.
makeValid :: ModuleName -> ModuleName
makeValid (MdlName mdl) = MdlName $ pack $ stripColons $ go0 $ unpack mdl
    where
        -- `go0` ensures that the first letter is capitalized
        go0 [] = mempty
        go0 str = case dropWhile (not . isAlpha) str of
            [] -> mempty
            c : cs -> toUpper c : go cs
        go [] = mempty
        go (c:cs)
            | isAlphaNum c = c : go cs
            -- truncate excessive or trailing colons
            | eqC c = case dropWhile eqC cs of
                [] -> mempty
                c' : cs'
                    | isAlpha c' -> "::" ++ toUpper c' : go cs'
                    | otherwise -> go cs' -- just skip it :(
            -- eliminate non-alphanumeric symbols, and turn the following
            -- letter uppercase
            | otherwise = case dropWhile (not . isAlphaNum) cs of
                [] -> mempty
                c' : cs' -> toUpper c' : go cs'
        -- colon equality
        eqC = (== ':')
        stripColons = dropWhile eqC . dropWhileEnd eqC

mdlIsEmpty :: ModuleName -> Bool
mdlIsEmpty (MdlName mdl) = BS.length mdl <= 0

instance Show ModuleName where
    show (MdlName mdl) = "MdlName " ++ show mdl

instance Pretty ModuleName where
    pretty (MdlName mdl) = pretty (unpack mdl)

instance Semigroup ModuleName where
    MdlName mdl1 <> MdlName mdl2 = MdlName $ mdl1 <> fromString "::" <> mdl2

instance Monoid ModuleName where
    mempty = MdlName mempty

instance IsString ModuleName where
    fromString = MdlName . fromString
