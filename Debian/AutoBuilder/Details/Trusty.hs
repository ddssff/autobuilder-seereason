{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Trusty (buildTargets) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Details.Common (TSt)
import qualified Debian.AutoBuilder.Details.Xenial as Xenial (buildTargets7)
import Debian.AutoBuilder.Types.Packages as P (apt, inGroups, patch)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))

buildTargets :: Monad m => ParamRec -> TSt m ()
buildTargets params = do
  _ghc <- apt "jessie" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc-comp"]
  Xenial.buildTargets7
