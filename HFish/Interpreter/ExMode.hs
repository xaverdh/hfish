module HFish.Interpreter.ExMode where

-- | The execution mode of a hfish statement.
data ExMode = 
  InOrderExM -- ^ Execute statement in the /main thread/.
  | ForkedExM -- ^ Fork a /new process/ to execute the statement.
  | PipeExM -- ^ Statement is part of a /pipe/.
  deriving (Eq,Ord,Show)

-- | Check if mode is /forked/.
isFork :: ExMode -> Bool
isFork = (==ForkedExM)

-- | Check if mode is /piped/.
isPipe :: ExMode -> Bool
isPipe = (==PipeExM)

-- | Check if mode is /in order/.
isInOrder :: ExMode -> Bool
isInOrder = (==InOrderExM)

