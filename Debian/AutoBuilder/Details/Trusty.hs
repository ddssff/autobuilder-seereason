{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Trusty (buildTargets) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Details.Common (TSt)
import qualified Debian.AutoBuilder.Details.Xenial as Xenial (buildTargets7)
import Debian.AutoBuilder.Types.Packages as P (apt, debdir, flag, inGroups, patch, uri, PackageFlag(RelaxDep), PackageId)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Repo.Fingerprint

buildTargets :: Monad m => ParamRec -> TSt m ()
buildTargets params = do
  -- _ghc <- apt "jessie" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc-comp"]
  nodejs
  Xenial.buildTargets7

nodejs :: Monad m => TSt m PackageId
nodejs =
    uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
    debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~trusty1.debian.tar.xz" "a93243ac859fae3a6832522b55f698bd") >>=
    flag (P.RelaxDep "libssl-dev") >>=
    inGroups ["ghcjs-comp"]
