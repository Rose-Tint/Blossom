{-# LANGUAGE RankNTypes #-}

module Blossom.Common.OStream (
    DocStream,
    OStream,
    stdout,
    stderr,
    nullStream,
    fileStream,
    docStream,
    putDoc,
    getDoc,
) where

import Data.IORef (IORef, modifyIORef', readIORef, newIORef)
import Prettyprinter (Doc, LayoutOptions(..), layoutSmart)
import Prettyprinter.Render.Terminal (AnsiStyle, renderIO)
import System.IO (Handle, IOMode(..), openFile, hFlush)
import qualified System.IO (stdout, stderr)


type DocStream = Doc AnsiStyle

data OStream
    = NullStream
    | FileStream !Handle
    | DocStream (IORef DocStream)

stdout :: OStream
stdout = FileStream System.IO.stdout

stderr :: OStream
stderr = FileStream System.IO.stderr

nullStream :: OStream
nullStream = NullStream

fileStream :: FilePath -> IO OStream
fileStream "<stdout>" = return stdout
fileStream "<stderr>" = return stderr
fileStream path = do
    hdl <- openFile path WriteMode
    return (FileStream hdl)

docStream :: IO OStream
docStream = do
    ref <- newIORef mempty
    return (DocStream ref)

-- | Appends a doc to the stream.
--
-- Note: the `@LayoutOptions@` are only used when the `@OStream@` represents a
-- file.
putDoc :: LayoutOptions -> DocStream -> OStream -> IO ()
putDoc _layout _doc NullStream = return ()
putDoc layout doc (FileStream hdl) = do
    let sds = layoutSmart layout doc
    renderIO hdl sds
    hFlush hdl
putDoc _layout doc (DocStream ref) = modifyIORef' ref (<> doc)

getDoc :: OStream -> IO (Maybe DocStream)
getDoc (DocStream ref) = Just <$> readIORef ref
getDoc _ = return Nothing
