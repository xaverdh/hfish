module HFish.Types where

data ShowAst = ShowAst Bool | NoAst
newtype NoExecute = NoExecute Bool
newtype IsCommand = IsCommand Bool
newtype FishCompat = FishCompat Bool
newtype IsBreakPoint = IsBreakPoint Bool


