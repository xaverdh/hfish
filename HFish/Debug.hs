module HFish.Debug where

import HFish.Types
import Control.Applicative

readDebug :: String -> Maybe Debug
readDebug s = readMainFlag s <|> readLibFlag s


readLibFlag :: String -> Maybe Debug
readLibFlag = fmap DebugLib . readLibFlag'

readLibFlag' :: String -> Maybe DebugFlag
readLibFlag' s
  | s == "all" = Just DebugAll
  | otherwise = Nothing

readMainFlag :: String -> Maybe Debug
readMainFlag = fmap DebugMain . readMainFlag'


readMainFlag' :: String -> Maybe DebugMain
readMainFlag' s
  | s == "ast-on-error" = Just DebugMainShowAstOnError
  | otherwise           = Nothing


