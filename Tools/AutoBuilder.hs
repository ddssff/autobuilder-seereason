{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Details (myParams)
import Debian.AutoBuilder.Details.Sources (myReleaseURI)
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Codename (parseCodename)
import Debian.Releases (parseReleaseTree)
import Debian.Repo.DebError (DebError)
import Debian.TH (here)
import System.Environment (getEnv, getExecutablePath, getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.Process (rawSystem)

main = do
  user <- getEnv "USER"
  args <- getArgs
  prog <- getExecutablePath
  case user of
    -- This actually runs the autobuilder
    "root" -> do
       (es :: [DebError]) <- M.main seereasonDefaults (myReleaseURI [$here]) (\h r -> myParams h (parseReleaseTree (parseCodename r)))
       case es of
         [] -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure (length es))
{-
       case result of
         Left e -> exitWith (ExitFailure 1)
         Right () -> exitWith ExitSuccess
-}
    -- Re-run the command with root permissions, preserving $HOME
    -- so we find the user's scratch directory in "$HOME/.autobuilder".
    _ -> rawSystem "sudo" ("-E" : prog : args) >>= exitWith
