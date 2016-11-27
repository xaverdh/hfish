{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.IO (
  forkWithFileDescriptors
  ,duplicate
  ,withFileR
  ,withFileW
  ,readFrom
  ,readLineFrom
  ,writeTo
  ,echo
  ,echoLn
  ,warn
) where

import Fish.Interpreter.Util
import Fish.Interpreter.FdTable
import Fish.Interpreter.Core

import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Data.Monoid
import Data.Bool
import Control.Concurrent
import System.IO
import System.Posix.Files
import qualified System.Posix.Types as PT
import qualified System.Posix.IO as P
import qualified Fish.Lang.Lang as L
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import qualified Data.Map as M

import Debug.Trace (trace)

-- | Execute an IO action in an environment
--   where the interal fd state has been translated
--   into OS calls.
--   
forkWithFileDescriptors :: IO () -> Fish ThreadId
forkWithFileDescriptors action = do
  FdTable fdescs closedStat <- askFdTable
  liftIO . forkOS $ do
    -- get a list of all os fds in use
    let pfds = M.keys closedStat
    -- save copies of all fds to avoid conflicts
    forM_ pfds $ \i -> P.dupTo i (i+10)
    -- set up redirections
    forM_ (M.toList fdescs) $ \(fd,pfd) ->
      P.dupTo (toEnum $ fromEnum pfd + 10) (toEnum $ fromEnum fd)
    -- close all fds marked closed:
    forM_ (M.toList closedStat) $ \(pfd,closed) ->
      when closed ( P.closeFd pfd )
    -- run the action
    action


-- | Make an abstract fd a duplicate of another abstract fd, i.e. a fd pointing
--
--   to the same OS fd as the other (it mimics the dup2 syscall on Posix systems).
-- 
--   This may fail if the second fd is invalid.
--
duplicate :: L.Fd -- ^ file descriptor to duplicate
  -> L.Fd -- ^ \"new\" file descriptor, which shall point to the
          --   same OS fd as the first after the call.
  -> Fish a -> Fish a
duplicate fd1 fd2 k = do
  pfd <- lookupFd' fd1
  insert fd2 pfd k

withFileR :: T.Text -> L.Fd -> Fish () -> Fish ()
withFileR fpath fd k = do
  pfd <- liftIO $ P.openFd
    (T.unpack fpath)
    P.ReadOnly
    Nothing
    P.defaultFileFlags
  insert fd pfd
    ( k `finally` liftIO (P.closeFd pfd) )


withFileW :: T.Text -> L.FileMode -> L.Fd -> Fish () -> Fish ()
withFileW fpath mode fd k =
  let popen m f = liftIO $ P.openFd (T.unpack fpath) P.WriteOnly m f
   in do
    pfd <- case mode of
      L.FModeWrite -> popen (Just accessMode)
        P.defaultFileFlags { P.trunc = True }
      L.FModeApp -> popen (Just accessMode)
        P.defaultFileFlags { P.append = True }
      L.FModeNoClob -> popen Nothing
        P.defaultFileFlags { P.exclusive = True }
    insert fd pfd
      ( k `finally` liftIO (P.closeFd pfd) )
  where
    accessMode = 
      foldr unionFileModes nullFileMode
      [ ownerReadMode, ownerWriteMode, groupReadMode, otherReadMode ]

readFrom :: L.Fd -> Fish T.Text
readFrom fd = do
  pfd <- lookupFd' fd
  h <- liftIO (P.fdToHandle pfd)
  r <- liftIO (hIsReadable h)
  unless r $ notReadableErr fd
  liftIO (TextIO.hGetContents h)

readLineFrom :: L.Fd -> Fish T.Text
readLineFrom fd = do
  pfd <- lookupFd' fd
  h <- liftIO (P.fdToHandle pfd)
  r <- liftIO (hIsReadable h)
  unless r $ notReadableErr fd
  liftIO (TextIO.hGetLine h)

writeTo :: T.Text -> L.Fd -> Fish ()
writeTo text fd = do
  pfd <- lookupFd' fd
  h <- liftIO (P.fdToHandle pfd)
  w <- liftIO (hIsWritable h)
  unless w $ notWriteableErr fd
  liftIO (TextIO.hPutStr h text)
  return () -- this has to be here for reasons I do not yet fully understand, likely related to lazyness

echo :: T.Text -> Fish ()
echo t = writeTo t L.Fd1

echoLn :: T.Text -> Fish ()
echoLn t = echo (t <> "\n")

-- | 'warn' bypasses the whole Fd passing machinery
--
--   and writes directly to stderr. Use for debugging only.
warn :: T.Text -> Fish ()
warn t = liftIO (TextIO.hPutStrLn stderr $ "Warning: " <> t)

lookupFd' :: L.Fd -> Fish PT.Fd
lookupFd' fd = lookupFd fd >>=
  maybe (notOpenErr fd) return

-- Errors:

mkFdErr :: T.Text -> L.Fd -> Fish a
mkFdErr t fd = errork
  $ "file descriptor " <> (showText . fromEnum) fd <> " is " <> t

invalidFdErr = mkFdErr "invalid"
notOpenErr = mkFdErr "not open"
notReadableErr = mkFdErr "not readable"
notWriteableErr = mkFdErr "not writeable"
      
