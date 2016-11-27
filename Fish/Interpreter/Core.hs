{-# LANGUAGE TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Core where

import Fish.Lang.Lang
import Fish.Interpreter.Util
import Fish.Interpreter.FdTable as FDT

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TextIO
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Lens
import System.Process
import System.IO
import System.Exit
import System.Environment
import System.Posix.Types (CPid)

-- | The Fish 'Monad', it holds both mutable and unmutable (reader)
--   state.
--
--   In addition it contains a ContT transformer, which is used
--   to implement control flow features,
--
--   such as: return, break, continue and error handling.
--
--   The latter means that we use our own error handling mechanism
--   rather then the builtin 'error'.
newtype Fish a = Fish ((ReaderT FishReader) (StateT FishState (ContT FishState IO)) a)
  deriving (Applicative,Functor,Monad,MonadIO,MonadState FishState,MonadReader FishReader,MonadCont)

runFish :: Fish a -> FishReader -> FishState -> IO FishState
runFish (Fish f) r s =
  ((f `runReaderT` r) `execStateT` s) `runContT` return

-- | The type of a fish /variable/
data Var = Var {
    _exported :: Bool
    ,_value :: [T.Text]
  }
  deriving (Eq,Ord)


-- | The type of a fish /environment/, mapping identifiers to
--   their values.
type Env a = M.Map T.Text a

-- | The /mutable/ state of the interpreter.
data FishState = FishState {
    -- _universalEnv :: Env
    _globalEnv :: Env Var
    ,_flocalEnv :: Env Var
    ,_localEnv :: Env Var
    ,_readOnlyEnv :: Env Var
    ,_functions :: Env ([T.Text] -> Fish ())
    ,_status :: ExitCode
    ,_cwdir :: FilePath
    ,_dirstack :: [FilePath]
    ,_lastPid :: Maybe CPid
  }

-- | The /readonly/ state of the interpreter.
--   Readonly means that it will not propagate the
--   stack upwards, only downwards.
data FishReader = FishReader {
    _fdTable :: FDT.FdTable
    ,_builtins :: Env (Bool -> [T.Text] -> Fish ())
    ,_breakK :: [() -> Fish ()]
    ,_continueK :: [() -> Fish ()]
    ,_returnK :: [() -> Fish ()]
    ,_errorK :: [T.Text -> Fish ()]
  }

makeLenses ''Var
makeLenses ''FishReader
makeLenses ''FishState

instance HasFdTable Fish where
  askFdTable = view fdTable
  localFdTable = local . (fdTable %~)

-- | Sets a breakpoint which is jumped to by a call to /continue/.
setContinueK f = callCC (\k -> local (continueK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to /break/.
setBreakK f = callCC (\k -> local (breakK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to /return/.
setReturnK f = callCC (\k -> local (returnK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to 'errork'.
setErrorK f = callCC (\k -> local (errorK %~ (k:)) f)

-- | Callins the top '_errorK' continuation.
--   Use this instead of 'error'
errork :: T.Text -> Fish a
errork t = do
  k:_ <- view errorK
  k t
  return undefined

-- | Takes a lens to one of the continuation stacks,
--   a cleanup routine and a fish action.
--
--   It then executes this action and, should a jump occur,
--   runs the cleanup routine before continuing the jump.
reThrow :: Lens' FishReader [a -> Fish ()]
  -- ^ The lens to the continuation stack.
  -> Fish b
  -- ^ A cleanup routine, its return value gets ignored.
  -> Fish ()
  -- ^ The fish action to execute.
  -> Fish ()
reThrow lensK cleanup f = 
  callCC $ \k -> flip local f
    ( lensK %~ map (\k' x -> cleanup >> k' x) )

-- | Run cleanup even if jumping out of context via some
--   continuation and resume the jump afterwards.
finally :: Fish b -> Fish () -> Fish ()
finally cleanup = 
  reThrow continueK cleanup
  . reThrow breakK cleanup
  . reThrow returnK cleanup
  . reThrow errorK cleanup

-- | Clearing all continuations,
--   calls to them will be silently ignored.
disallowK :: Fish a -> Fish a
disallowK =
  let noA = const $ return ()
   in local
    ( ( breakK .~ [noA] )
    . ( continueK .~ [noA] ) )

-- | An empty FishState
emptyFishState =
  FishState
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    ExitSuccess "" []
    Nothing

