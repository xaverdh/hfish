{-# LANGUAGE TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Core where

import Fish.Lang
import HFish.Interpreter.Util
import HFish.Interpreter.FdTable as FDT
import Data.NText as NText
import HFish.Interpreter.Env as Env

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Cont
import Control.Lens
import Control.Exception as E
import System.Exit
import System.Posix.Types (CPid)
import System.Posix.Signals (Signal)

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
  deriving (Functor,Applicative,Monad,MonadIO,MonadState FishState,MonadReader FishReader,MonadCont)

-- | Run a fish action with given reader and state, returning
--   the final state.
runFish :: Fish a -> FishReader -> FishState -> IO FishState
runFish (Fish f) r s =
  ((f `runReaderT` r) `execStateT` s) `runContT` return

-- | Return an IO action, running the given fish action
--   in the IO Monad with and returning the new / final state.
--
--   The fish action will be run with the state (and reader) from
--   the time of the call to projectIO,
--
--   i.e. projectIO captures this state when called.
--
projectIO :: Fish () -> Fish (IO FishState)
projectIO f = do
  r <- ask
  s <- get
  return (runFish f r s)

-- | Takes a fish action and a continuation and passes an IO
--
--   version of the fish action as an argument to the continuation.
--
asIO :: Fish () -> (IO FishState -> Fish a) -> Fish a
asIO f g = projectIO f >>= g

-- | The type of a fish /variable/
data Var = Var {
    _exported :: Export
    ,_len :: Int
    ,_value :: [T.Text]
  }
  deriving (Eq,Ord)


-- | The type of a (fish) function.
type Function =
  [T.Text] -- ^ The arguments to the function call, already evaluated.
  -> Fish ()

-- | The type of an event handler
newtype EventHandler = EventHandler NText
  deriving (Eq,Ord,Show)

-- | The type of a signal handler
newtype SignalHandler = SignalHandler NText
  deriving (Eq,Ord,Show)

-- | The /mutable/ state of the interpreter.
data FishState = FishState {
    -- _universalEnv :: Env
    _globalEnv :: Env Var
    ,_flocalEnv :: Env Var
    ,_localEnv :: Env Var
    ,_readOnlyEnv :: Env Var
    ,_functions :: Env Function
    ,_status :: ExitCode
    ,_cwdir :: FilePath
    ,_dirstack :: [FilePath]
    ,_lastPid :: Maybe CPid
    ,_eventHandlers :: Env (S.Set EventHandler)
    ,_signalHandlers :: M.Map Signal (S.Set SignalHandler)
  }


-- | Restore given scope after fish action exits.
localise :: Lens' FishState b -> Fish a -> Fish a
localise l f = do
  memory <- use l
  r <- f
  modify (l .~ memory)
  return r


-- | The type of a builtin.
type Builtin = 
  Bool
  -- ^ whether the builtin is forked (executed in background)
  --
  --   builtins may or may not honour this hint. Most don't.
  -> [T.Text]
  -- ^ The arguments to the call, already evaluated.
  -> Fish ()

-- | The /readonly/ state of the interpreter.
--   Readonly means that it will not propagate the
--   stack upwards, only downwards.
data FishReader = FishReader {
    _fdTable :: FDT.FdTable
    ,_builtins :: Env Builtin
    ,_breakK :: [() -> Fish ()]
    ,_continueK :: [() -> Fish ()]
    ,_returnK :: [() -> Fish ()]
    ,_errorK :: [T.Text -> Fish ()]
    ,_breakpoint :: Fish ()
    ,_fishCompatible :: Bool
  }

makeLenses ''Var
makeLenses ''FishReader
makeLenses ''FishState

instance HasFdTable Fish where
  askFdTable = view fdTable
  localFdTable = local . (fdTable %~)

-- | Set a breakpoint.
setBreakpoint :: Fish ()
setBreakpoint =
  view breakpoint >>= id

-- | Sets a breakpoint which is jumped to by a call to /continue/.
setContinueK :: Fish () -> Fish ()
setContinueK f = callCC (\k -> local (continueK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to /break/.
setBreakK :: Fish () -> Fish ()
setBreakK f = callCC (\k -> local (breakK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to /return/.
setReturnK :: Fish () -> Fish ()
setReturnK f = callCC (\k -> local (returnK %~ (k:)) f)

-- | Sets a breakpoint which is jumped to by a call to 'errork'.
setErrorK :: Fish T.Text -> Fish T.Text
setErrorK f = callCC (\k -> local (errorK %~ (k:)) f)

-- | Calls the top '_errorK' continuation.
--   Use this instead of 'error'
errork :: T.Text -> Fish a
errork t = view errorK >>= \case
  [] -> error $ T.unpack t
  k:_ -> k t >> return undefined

-- | Takes a lens to one of the continuation stacks,
--   an interrupt routine and a fish action.
--
--   It then executes this action and, should a jump occur,
--   runs the interrupt routine before continuing the jump.
interruptK :: Lens' FishReader [a -> Fish ()]
  -- ^ The lens to the continuation stack.
  -> Fish b
  -- ^ An interrupt routine, its return value gets ignored.
  -> Fish ()
  -- ^ The fish action to execute.
  -> Fish ()
interruptK lensK interrupt f = 
  callCC $ \k -> flip local f
    ( lensK %~ map (\k' x -> interrupt >> k' x) )

-- | Run cleanup even if jumping out of context via some
--   continuation and resume the jump afterwards.
finallyFish :: Fish () -> Fish b  -> Fish ()
finallyFish f cleanup = 
  (f `onContinuationFish` cleanup)
  >> void cleanup

-- | Like 'finallyFish' but only run cleanup if a
--   continuation is called
onContinuationFish :: Fish () -> Fish b  -> Fish ()
onContinuationFish f cleanup = 
  ( interruptK continueK cleanup
  . interruptK breakK cleanup
  . interruptK returnK cleanup
  . interruptK errorK cleanup ) f

-- | Make sure cleanup is run regardless of continuation jumping
--   or errors (IO or pure).
finally :: Fish () -> IO b  -> Fish ()
finally f cleanup = 
  asIO
    ( f `onContinuationFish` liftIO cleanup )
    ( liftIO . (`E.finally` cleanup) )
  >>= put

-- | Clearing all continuations,
--   calls to them will be silently ignored.
disallowK :: Fish a -> Fish a
disallowK = local
    ( ( breakK .~ noA    )
    . ( continueK .~ noA )
    . ( returnK .~ noA   )
    . ( errorK .~ noA    ) )
  where
    noA = repeat ( const $ return () )

-- | An empty FishState
emptyFishState :: FishState
emptyFishState =
  FishState
    Env.empty
    Env.empty
    Env.empty
    Env.empty
    Env.empty
    ExitSuccess "" []
    Nothing
    Env.empty
    M.empty

