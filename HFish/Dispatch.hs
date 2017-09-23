{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module HFish.Dispatch where

import HFish.Interpreter.Core
import HFish.Types

import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Lens

import qualified Data.Set as S

import System.Console.Haskeline.MonadException

data DispatchState = DispatchState
  { _dReader :: FishReader
  , _dState :: FishState
  , _dOnError :: Maybe ( HFishError -> Fish () )
  , _dCompat :: FishCompat
  , _dDebug :: S.Set DebugMain }
makeLenses ''DispatchState

newtype Dispatch a = Dispatch
  { getDispatch :: StateT DispatchState IO a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState DispatchState )

instance MonadException Dispatch where
  controlIO k = 
    let f (RunIO runio) = RunIO $
          fmap Dispatch . runio . getDispatch
     in Dispatch $ controlIO $ fmap getDispatch . k . f


evalDispatch :: Dispatch a -> DispatchState -> IO a
evalDispatch (Dispatch m) = evalStateT m



