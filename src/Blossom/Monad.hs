module Blossom.Monad (
    Blossom,
    runBlossom,
    getCnf,
    fatal,
    message,
    status,
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
runBlossom cnf blos = evalStateT blos (newState cnf)

getCnf :: (Config -> a) -> Blossom a
getCnf field = gets (field . bsConfig)

-- | Prints a doc if the verbosity 'filter' allows it (e.g. the verbosity on
-- the command line is <= the verbosity given to the function).
write :: Verbosity -> DocStream -> Blossom ()
write minVerb doc = do
    verb <- getCnf cnfVerbosity
    when (verb >= minVerb) $ do
        layout <- getCnf cnfDocLayout
        ostream <- getCnf cnfOStream
        liftIO $ putDoc layout (doc <> line) ostream
        return ()

-- | Print a doc if the verbosity is `ErrorsOnly` (i.e. >= 1)
fatal :: DocStream -> Blossom ()
fatal = write ErrorsOnly

-- | Print a doc if the verbosity is `Normal` (i.e. >= 2, or defaulted)
message :: DocStream -> Blossom ()
message = write Normal

-- | Print a doc if the verbosity is `Verbose` (i.e. >= 3)
status :: DocStream -> Blossom ()
status = write Verbose
