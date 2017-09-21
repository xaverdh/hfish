{-# language TemplateHaskell #-}
module HFish.Version where

import Development.GitRev

version :: String
version = $(gitHash)
