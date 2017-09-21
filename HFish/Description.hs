{-# language OverloadedStrings #-}
module HFish.Description (
  description
) where

import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen
import Data.Text as T

description :: Doc
description = ""
  -- "hfish is a fish-like shell."
  -- </> " It has the following options:"
  -- <> hang 4 options

options :: Doc
options = 
  help
  <$> version
  <$> noexec
  <$> ast
  <$> compat
  <$> cmd

help = ""
version = ""
noexec = ""
ast = ""
compat = ""
cmd = ""

-- "-c / --command"

