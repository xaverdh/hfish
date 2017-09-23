module HFish.Types
  ( DebugFlag(..)
  , DebugMain(..)
  , Debug(..)
  , IsBreakPoint(..)
  , FishCompat(..)
  , IsCommand(..)
  , NoExecute(..)
  , ShowAst(..) )
where

import HFish.Interpreter.Core (DebugFlag(..))


data ShowAst = ShowAst Bool | NoAst
newtype NoExecute = NoExecute Bool
newtype IsCommand = IsCommand Bool
newtype FishCompat = FishCompat Bool
newtype IsBreakPoint = IsBreakPoint Bool

data DebugMain = 
  DebugMainShowAstOnError
  deriving (Eq,Ord,Show)

data Debug = 
  DebugMain DebugMain
  | DebugLib DebugFlag


