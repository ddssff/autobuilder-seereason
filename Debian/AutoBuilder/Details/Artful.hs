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
                                               pid, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri, release,
                                               GroupName, inGroups, createPackage)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..))
import Debian.Releases (baseRelease, BaseRelease(Trusty, Artful))
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec(Commit, Branch))

buildTargets = do
  commonTargets
  findGroup "ghcjs-libs" >>= mapM_ ghcjs_flags

findGroup :: GroupName -> TSt (Set P.PackageId)
findGroup name =
  (Set.fromList . map (view pid) . filter (Set.member name . view groups) . Map.elems) <$> use packageMap
