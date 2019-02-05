{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Details (myParams)
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Release (ReleaseName(ReleaseName))
import Debian.Releases (parseReleaseTree)
import System.Environment (getEnv, getExecutablePath, getArgs)
import System.Exit (exitWith)
import System.Process (rawSystem)
import Debug.Trace

main = do
  user <- getEnv "USER"
  args <- getArgs
  prog <- getExecutablePath
  case user of
    -- This actually runs the autobuilder
    "root" -> M.main seereasonDefaults (\h r -> myParams h (parseReleaseTree (ReleaseName r)))
    -- Re-run the command with root permissions, preserving $HOME
    -- so we find the user's scratch directory in "$HOME/.autobuilder".
    _ -> rawSystem "sudo" ("-E" : prog : args) >>= exitWith
