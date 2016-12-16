{-# LANGUAGE TemplateHaskell, LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Process.FdSetup (
  forkWithFileDescriptors
) where

import HFish.Interpreter.Util
import HFish.Interpreter.Core
import HFish.Interpreter.FdTable
import qualified Fish.Lang as L
import qualified Data.Map as M
import qualified Data.IntSet as S
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import System.Posix.Resource
import System.Posix.Process

import Control.Lens
import Control.Monad
import Control.Monad.State

-- | Get the maximum number of allowed file descriptors.
--
getMaxNumFds :: Fish Int
getMaxNumFds =
  liftIO (softLimit <$> getResourceLimit ResourceOpenFiles)
  <$$> \case
    ResourceLimitUnknown -> 2^8
    -- ^ Conservative guess (ghc guesses the same
    -- when closing all handles in createProcess).
    ResourceLimitInfinity -> 2^12
    -- ^ Random high number
    ResourceLimit i -> fromInteger i

data FdSetupState =
  FdSetupState {
    _table :: M.Map L.Fd PT.Fd
    -- ^ Initally a copy of the main table
    ,_fdsInUse :: S.IntSet
    -- ^ A list of currently used OS fds
    ,_allocIndex :: Int
    -- ^ A pointer-like index; all fds above this index
    --   are currently in use. This is purely a performance
    --   optimisation.
  }
makeLenses ''FdSetupState

-- | A State Monad for setting up the OS file descriptors.
type FdSetupMonad a = StateT FdSetupState IO a

-- | Obtain a fresh, unused OS fd number.
getUnusedFd :: FdSetupMonad PT.Fd
getUnusedFd = do
  i <- use allocIndex
  used <- use fdsInUse
  [ j | j <- map (\x -> i-x) [0..i] , S.notMember j used ] & \case
    [] -> error "too many file descriptors open"
    r:_ -> do
      fdsInUse %= S.insert r
      allocIndex .= r-1
      return (toEnum r)

-- | Ensure that the given OS fd can be safely overwritten.
save :: PT.Fd -> FdSetupMonad ()
save i = do
  j <- getUnusedFd
  lift $ P.dupTo i j
  table %= M.map (\x -> if x == i then j else x)

-- | Set up the file descriptors.
setupFds :: FdSetupMonad ()
setupFds = forM_ [0..9] setupFd
  where
    setupFd i = do
      mbPfd <- use $ table . at (toEnum i)
      whenJust mbPfd
        $ \pfd -> when (fromEnum pfd /= i) $ do
          used <- uses fdsInUse (S.member i)
          when used (save $ toEnum i)
          lift $ P.dupTo pfd (toEnum i)
          return ()

-- | Execute an IO action in an environment
--   where the interal fd state has been translated
--   into OS calls.
--
forkWithFileDescriptors :: IO () -> Fish PT.ProcessID
forkWithFileDescriptors action = do
  max_num_fds <- getMaxNumFds
  FdTable fdescs closed <- askFdTable
  map fromEnum ( M.fold (:) [] fdescs ) & \used ->
    liftIO . forkProcess $ do
      -- close all fds marked closed:
      forM_ closed P.closeFd
  
      -- set up the redirections
      runStateT setupFds FdSetupState
        { _table = fdescs
         ,_fdsInUse = S.fromList used
         ,_allocIndex = max_num_fds-1 }
  
      -- run the action
      action

