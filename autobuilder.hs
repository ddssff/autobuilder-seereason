{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Details (myParams)
import Debian.Debianize.Details (seereasonDefaultAtoms)
import System.Environment (getEnv, getExecutablePath, getArgs)
import System.Exit (exitWith)
import System.Process (rawSystem)

main = do
  user <- getEnv "USER"
  args <- getArgs
  prog <- getExecutablePath
  case user of
    -- This actually runs the autobuilder
    "root" -> M.main seereasonDefaultAtoms myParams
    -- Re-run the command with root permissions, preserving $HOME
    -- so we find the user's scratch directory in "$HOME/.autobuilder".
    _ -> rawSystem "sudo" ("-E" : prog : args) >>= exitWith
