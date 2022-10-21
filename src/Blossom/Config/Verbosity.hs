module Blossom.Config.Verbosity (
    Verbosity(..),
) where


data Verbosity
    = Silent
    | ErrorsOnly
    | Normal
    | Verbose
    deriving (Show, Eq, Ord)

instance Enum Verbosity where
    toEnum n
        | n < 0 = Silent
        | otherwise = case n of
            0 -> ErrorsOnly
            1 -> Normal
            _ -> Verbose -- n > 1
    fromEnum Silent = -1
    fromEnum ErrorsOnly = 0
    fromEnum Normal = 1
    fromEnum Verbose = 2
