{-# language LambdaCase, OverloadedStrings #-}
module Fish.Interpreter.Status where

import Fish.Interpreter.Core
import Fish.Interpreter.Var

import qualified Data.Text as T
import Data.Monoid
import Control.Lens
import Control.Monad
import System.Exit

instance Enum ExitCode where
  toEnum = \case
    0 -> ExitSuccess
    i -> ExitFailure i
  fromEnum = \case
    ExitSuccess -> 0
    ExitFailure i -> i

getStatus :: Fish ExitCode
getStatus = use status

setStatus :: ExitCode -> Fish ()
setStatus exCode = do
  status .= exCode
  readOnlyEnv . at "status" .= Just
    (Var False [T.pack . show $ fromEnum exCode])

ok :: Fish ()
ok = setStatus ExitSuccess

isOk :: Fish Bool
isOk = getStatus >>= \case
  ExitSuccess -> return True
  _ -> return False

ifOk :: Fish () -> Fish ()
ifOk f = isOk >>= flip when f

unlessOk :: Fish () -> Fish ()
unlessOk f = isOk >>= flip unless f

modifyStatus :: (ExitCode -> ExitCode) -> Fish ()
modifyStatus f = do
  status %= f
  exCode <- use status
  readOnlyEnv . at "status" .= Just
    (Var False [T.pack . show $ fromEnum exCode])

