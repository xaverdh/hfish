{-# LANGUAGE TemplateHaskell, RankNTypes, GeneralizedNewtypeDeriving, LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Core where

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

{- Fish, runFish -}
newtype Fish a = Fish ((ReaderT FishReader) (StateT FishState (ContT FishState IO)) a)
  deriving (Applicative,Functor,Monad,MonadIO,MonadState FishState,MonadReader FishReader,MonadCont)

runFish :: Fish a -> FishReader -> FishState -> IO FishState
runFish (Fish f) r s =
  ((f `runReaderT` r) `runStateT` s) `runContT` (return . snd)

{- Var -}
data Var = Var {
    _exported :: Bool
    ,_value :: [T.Text]
  }
  deriving (Eq,Ord)

{- FishState, Env -}
type Env a = M.Map T.Text a

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

{- FishReader -}
data FishReader = FishReader {
    _hin :: Handle
    ,_hout :: Handle
    ,_herr :: Handle
    ,_builtins :: Env (Bool -> [T.Text] -> Fish ())
    ,_breakK :: [() -> Fish ()]
    ,_continueK :: [() -> Fish ()]
    ,_errorK :: [T.Text -> Fish ()]
  }

{- Lenses -}
makeLenses ''Var
makeLenses ''FishReader
makeLenses ''FishState

{- Setting Breakpoints -}
setContinueK f = callCC (\k -> local (continueK %~ (k:)) f)
setBreakK f = callCC (\k -> local (breakK %~ (k:)) f)
setErrorK f = callCC (\k -> local (errorK %~ (k:)) f)

{- Calling the top errorK continuation -}
errork :: T.Text -> Fish a
errork t = do
  k:_ <- view errorK
  k t
  return undefined

{- Clearing all continuations,
   calls to them will be sliently ignored. -}
disallowK :: Fish a -> Fish a
disallowK =
  let noA = const $ return ()
   in local
    ( ( breakK .~ [noA] )
    . ( continueK .~ [noA] ) )
                   
{- Instances -}
instance Show Var where
  show (Var False vs) = show vs
  show (Var True vs) = "(exported) " ++ show vs

{-
instance Show FishState where
  show st = unlines
    $ map (show . (st^.)) [readOnlyEnv,globalEnv,flocalEnv,localEnv]
    ++ [ "status: " ++ show (st ^. status)
        ,"dirstack: " ++ show (st ^. dirstack)
        ,"cwdir: " ++ show (st ^. cwdir)
        ,"lastPid: " ++ show (st ^. lastPid) ]
    ++ map (("function "++) . show) (M.keys $ st ^. functions)
-}

{- emptyFishState -}
emptyFishState =
  FishState
    M.empty
    M.empty
    M.empty
    M.empty
    M.empty
    ExitSuccess "" []
    Nothing

{- IO -}

echo :: T.Text -> Fish ()
echo t = do
  outH <- view hout
  liftIO (TextIO.hPutStrLn outH t)

warn :: T.Text -> Fish ()
warn t = liftIO (TextIO.hPutStrLn stderr $ "Warning: " <> t)
