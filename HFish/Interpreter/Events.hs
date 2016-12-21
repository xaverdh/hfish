{-# language LambdaCase, OverloadedStrings #-}
module HFish.Interpreter.Events where

import HFish.Interpreter.Core
import HFish.Interpreter.Util

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import qualified Data.Set as S
import qualified Data.Text as T

import System.Posix.Signals
import System.Posix.Types

handleEvent :: T.Text -> [T.Text] -> Fish ()
handleEvent evName args =
  use (eventHandlers . at evName)
  >>= flip whenJust ( mapM_ call . S.toList )
  where
    call :: EventHandler -> Fish ()
    call (EventHandler fname) = 
      use (functions . at fname)
      >>= flip whenJust ($args)

handleSignal :: Signal -> Fish ()
handleSignal sig =
  use (signalHandlers . at sig)
  >>= flip whenJust ( mapM_ call . S.toList )
  where
    call :: SignalHandler -> Fish ()
    call (SignalHandler fname) =
      use (functions . at fname)
      >>= flip whenJust ($[])

setupEventHandler :: T.Text -> EventHandler -> Fish ()
setupEventHandler evName h =
  eventHandlers . at evName %=
    (Just . maybe (S.singleton h) (S.insert h) )

setupSignalHandler :: T.Text -> SignalHandler -> Fish ()
setupSignalHandler sigspec h = do
  sig <- parseSigSpec sigspec
  signalHandlers . at sig %=
    (Just . maybe (S.singleton h) (S.insert h) )
  asIO (handleSignal sig) $ \h ->
    liftIO . void $ installHandler sig (Catch $ void h) Nothing

parseSigSpec :: T.Text -> Fish Signal
parseSigSpec sigspec
  | Just i <- readTextMaybe sigspec = return i
  | Just sig <- sigFromName sigspec = return sig
  | otherwise = errork $ "unknown signal: " <> sigspec

sigFromName :: T.Text -> Maybe Signal
sigFromName t =
  let t' = T.toUpper t
   in case maybe t' id (T.stripPrefix "SIG" t') of
    "HUP"    -> Just sigHUP
    "INT"    -> Just sigINT
    "QUIT"   -> Just sigQUIT
    "ILL"    -> Just sigILL
    "TRAP"   -> Just sigTRAP
    "ABRT"   -> Just sigABRT
    -- "IOT"    -> Just sigIOT
    "BUS"    -> Just sigBUS
    "FPE"    -> Just sigFPE
    "KILL"   -> Just sigKILL
    "USR1"   -> Just sigUSR1
    "SEGV"   -> Just sigSEGV
    "USR2"   -> Just sigUSR2
    "PIPE"   -> Just sigPIPE
    "ALRM"   -> Just sigALRM
    "TERM"   -> Just sigTERM
    -- "STKFLT" -> Just sigSTKFLT
    "CHLD"   -> Just sigCHLD
    -- "CLD"    -> Just sigCLD
    "CONT"   -> Just sigCONT
    "STOP"   -> Just sigSTOP
    "TSTP"   -> Just sigTSTP
    "TTIN"   -> Just sigTTIN
    "TTOU"   -> Just sigTTOU
    "URG"    -> Just sigURG
    "XCPU"   -> Just sigXCPU
    "XFSZ"   -> Just sigXFSZ
    "VTALRM" -> Just sigVTALRM
    "PROF"   -> Just sigPROF
    -- "WINCH"  -> Just sigWINCH
    -- "IO"     -> Just sigIO
    "POLL"   -> Just sigPOLL
    -- "PWR"    -> Just sigPWR
    -- "UNUSED" -> Just sigUNUSED
    "SYS"    -> Just sigSYS
    _ -> Nothing

