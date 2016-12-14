{-# language LambdaCase, TemplateHaskell #-}
module HFish.Interpreter.FdTable where

import qualified Data.Map as M
import qualified Fish.Lang as L
import qualified System.Posix.IO as P
import qualified System.Posix.Types as PT
import Control.Lens
import Control.Monad.Reader.Class
import Control.Monad.IO.Class

-- | A global table, holding all file descriptors.
--
--   Implemented as a map from /abstract/ fds to the underlying
--
--   Posix file descriptors (OS fds from 'System.Posix.Types.Fd').
data FdTable = FdTable {
    _mainTable :: M.Map L.Fd PT.Fd
    ,_closed :: [PT.Fd]
  } deriving (Show)
makeLenses ''FdTable

-- | A typeclass for Monads which hold a 'FdTable' in their state.
--
--   Using MultiParamTypeClasses and FlexibleContexts,
--   this could have been defined as:
--
-- > class MonadState HasFdTable m => HasFdTable m
-- > askFdTable = get
-- > putFdTable = put
-- > stateFdTable = state
--
class Monad m => HasFdTable m where
  askFdTable :: m FdTable
  localFdTable :: (FdTable -> FdTable) -> m a -> m a

-- | Do something with the 'FdTable'
withFdTable :: HasFdTable m => (FdTable -> m a) -> m a
withFdTable = (askFdTable >>=)

-- | Look up the OS fd corresponding to an abstract fd
--
--   (if any) in the current 'FdTable'.
lookupFd :: HasFdTable m => L.Fd -> m (Maybe PT.Fd)
lookupFd fd = do
  withFdTable (return . (^. mainTable . at fd))

-- | Insert an (abstract fd,OS fd) pair into the 'FdTable'.
insert :: HasFdTable m => L.Fd -> PT.Fd -> m a -> m a
insert fd pfd =
  localFdTable (mainTable %~ M.insert fd pfd)

-- | Mark the OS fd corresponding to this (abstract) fd as closed.
--
--   It will appear closed to builtins and child processes.
--
--   Silently ignores the case where fd does not exits.
close :: HasFdTable m => L.Fd -> m a -> m a
close fd k = lookupFd fd >>= \case
  Nothing -> k
  Just pfd -> 
    flip localFdTable k
      (  ( closed %~ (pfd:) )
       . ( mainTable %~ M.mapMaybe (erase pfd) ) )
  where
    erase y x = if x == y then Nothing else Just x

-- | The initial FdTable stdin / -out / -err.
--
initialFdTable :: FdTable
initialFdTable = FdTable fds []
  where
    fds = M.fromList
      [ ( L.Fd0, P.stdInput )
        ,( L.Fd1, P.stdOutput )
        ,( L.Fd2, P.stdError ) ]
