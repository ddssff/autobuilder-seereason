{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}

module Debian.AutoBuilder.Details.Artful ( buildTargets ) where

import Control.Lens (use, view, (%=))
import Data.FileEmbed (embedFile)
import Data.Map as Map (elems, keys)
import Data.Set as Set (fromList, insert, member, Set)
import Data.Text as Text (unlines)
import Data.Version (Version(Version))
import Debian.AutoBuilder.Details.Common -- (named, ghcjs_flags, putSrcPkgName)
import Debian.AutoBuilder.Types.Packages as P (TSt, depends,
                                               PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           NoDoc, UDeb, OmitLTDeps, SkipVersion), packageMap,
                                               pid, proc, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri, release,
                                               GroupName, inGroups, createPackage)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..))
import Debian.Releases (baseRelease, BaseRelease(Trusty, Artful))
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec(Commit, Branch))

buildTargets :: Monad m => TSt m ()
buildTargets = do
  _haskell_devscripts <-
      -- Tagged 0.13.3
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "6e1e94bc4efd8a0ac37f34ac84f4813bcb0105cc"] >>=
      -- version we used from 8/2016-11/2016
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "2668216c654b0b302cb51162b2246c39cd6adc1e"] >>=
      -- git "https://github.com/ddssff/haskell-devscripts" [Branch "0.12"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  _libjs_jcrop <- apt "jessie" "libjs-jcrop" >>= \p ->
                 patch $(embedFile "patches/libjs-jcrop.diff") p >>= proc
  -- findGroup "ghcjs-libs" >>= mapM_ ghcjs_also
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp"]
  artfulTargets -- Targets with version numbers bumped past existing artful packages
  return ()

findGroup :: Monad m => GroupName -> TSt m (Set P.PackageId)
findGroup name =
  (Set.fromList . map (view pid) . filter (Set.member name . view groups) . Map.elems) <$> use packageMap
