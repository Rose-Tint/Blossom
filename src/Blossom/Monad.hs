module Blossom.Monad (
    Blossom,
    runBlossom,
    getCnf,
    verbosity,
    printError,
    printError',
    message,
    message',
    verbose,
    verbose',
    liftIO,
) where

import Blossom.Config (Config(cnfVerbosity, cnfDocLayout, cnfOStream))
import Blossom.Config.Verbosity (Verbosity(..))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, evalStateT, gets)
import Prettyprinter (line)
import Blossom.Common.OStream (DocStream, putDoc)


newtype BlossomState
    = BlossomState {
        bsConfig :: Config
    }

type Blossom = StateT BlossomState IO


newState :: Config -> BlossomState
newState = BlossomState

runBlossom :: Config -> Blossom a -> IO a
runBlossom cmd blos = evalStateT blos (newState cmd)

getCnf :: (Config -> a) -> Blossom a
getCnf field = gets (field . bsConfig)

verbosity :: Blossom Verbosity
verbosity = getCnf cnfVerbosity

write :: DocStream -> Blossom ()
write doc = do
    layout <- getCnf cnfDocLayout
    ostream <- getCnf cnfOStream
    liftIO $ putDoc layout doc ostream

-- | Prints a doc if the verbosity 'filter' allows it (e.g. the verbosity on
-- the command line is <= the verbosity given to the function).
filterVerbosity :: Verbosity -> DocStream -> Blossom ()
filterVerbosity minVerb doc = do
    verb <- verbosity
    when (verb >= minVerb) $
        write doc

-- | Print a doc if the verbosity is `ErrorsOnly` (i.e. >= 1, or 'errors' on
-- the command line)
printError :: DocStream -> Blossom ()
printError = filterVerbosity ErrorsOnly

-- | Print a doc if the verbosity is `ErrorsOnly` (i.e. >= 1, or 'errors' on
-- the command line)
printError' :: DocStream -> Blossom ()
printError' = printError . (<> line)

-- | Print a doc if the verbosity is `Normal` (i.e. >= 2, or defaulted on the
-- command line)
message :: DocStream -> Blossom ()
message = filterVerbosity Normal

-- | Exactly like `@message@`, but appends a newline
message' :: DocStream -> Blossom ()
message' = message . (<> line)

-- | Print a doc if the verbosity is `Verbose` (i.e. >= 3, or 'verbose' on the
-- command line)
verbose :: DocStream -> Blossom ()
verbose = filterVerbosity Verbose

-- | Exactly like `@verbose@`, but appends a newline
verbose' :: DocStream -> Blossom ()
verbose' = message . (<> line)
