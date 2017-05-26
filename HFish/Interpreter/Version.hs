{-# language TemplateHaskell #-}
module HFish.Interpreter.Version where

import Development.GitRev

version :: String
version = $(gitHash)
