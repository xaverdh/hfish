{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module HFish.Dispatch where

import HFish.Interpreter.Core
import HFish.Types
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Lens

data DispatchState = DispatchState
  { _dReader :: FishReader
  , _dState :: FishState
  , _dCompat :: FishCompat }
makeLenses ''DispatchState

newtype Dispatch a = Dispatch ( StateT DispatchState IO a )
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadState DispatchState )

evalDispatch :: Dispatch a -> DispatchState -> IO a
evalDispatch (Dispatch m) = evalStateT m


onState :: ( FishReader -> FishState -> a )
          -> ( a -> Dispatch b ) -> Dispatch b
onState f k = do
  r <- use dReader
  s <- use dState
  k $ f r s

onStateId :: (FishReader -> FishState -> Dispatch a) -> Dispatch a
onStateId = flip onState id



