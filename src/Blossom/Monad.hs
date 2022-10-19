module Blossom.Monad (
    Blossom,
    runBlossom,
    getCmd,
    verbosity,
    print',
    printError,
    message,
    verbose,
    liftIO,
) where

import Blossom.Cmd (CmdLine(cmdVerbosity), Verbosity(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, gets)
import Prettyprinter (Doc, LayoutOptions(..), PageWidth(..), layoutSmart)
import Prettyprinter.Render.Text (renderIO)
import System.IO (Handle, stdout)


newtype BlossomState
    = BlossomState {
        bsCmdLine :: CmdLine
    }

type Blossom = StateT BlossomState IO


newState :: CmdLine -> BlossomState
newState = BlossomState

runBlossom :: CmdLine -> Blossom a -> IO a
runBlossom cmd blos = evalStateT blos (newState cmd)

getCmd :: (CmdLine -> a) -> Blossom a
getCmd field = gets (field . bsCmdLine)

verbosity :: Blossom Verbosity
verbosity = getCmd cmdVerbosity

outputHandle :: Blossom Handle
outputHandle = return stdout

print' :: Doc ann -> Blossom ()
print' doc = do
    hdl <- outputHandle
    let opts = LayoutOptions (AvailablePerLine 60 1.0)
    let stream = layoutSmart opts doc
    liftIO $ renderIO hdl stream
    return ()

-- | Prints a doc if the verbosity 'filter' allows it (e.g. the verbosity on
-- the command line is <= the verbosity given to the function).
filterVerbosity :: Verbosity -> Doc ann -> Blossom ()
filterVerbosity minVerb doc = do
    verb <- verbosity
    when (verb >= minVerb) $
        print' doc

-- | Print a doc if the verbosity is `ErrorsOnly` (i.e. >= 1, or 'errors' on
-- the command line)
printError :: Doc ann -> Blossom ()
printError = filterVerbosity ErrorsOnly

-- | Print a doc if the verbosity is `Normal` (i.e. >= 2, or defaulted on the
-- command line)
message :: Doc ann -> Blossom ()
message = filterVerbosity Normal

-- | Print a doc if the verbosity is `Verbose` (i.e. >= 3, or 'verbose' on the
-- command line)
verbose :: Doc ann -> Blossom ()
verbose = filterVerbosity Verbose
