{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}

module Debian.AutoBuilder.Details.Xenial ( buildTargets7, buildTargets8 ) where

import Control.Lens (at, use, view, (%=))
import Control.Monad.Trans (lift)
import Data.Char (toLower)
import Data.FileEmbed (embedFile)
import Data.Function (on)
import Data.List (sortBy)
import Data.Map as Map (elems, keys)
import Data.Set as Set (fromList, insert, member, Set)
import Data.Text as Text (unlines)
import Data.Version (Version(Version))
import Debian.AutoBuilder.Details.Common (TSt, ghcjs, ghcjs_also) -- (named, ghcjs_flags, putSrcPkgName)
import Debian.AutoBuilder.Details.Trusty (commonTargets)
import Debian.AutoBuilder.Types.Packages as P (depends,
                                               PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           NoDoc, UDeb, OmitLTDeps, SkipVersion), packageMap, Package(..),
                                               pid, proc, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri, release, spec, clonePackage,
                                               GroupName, inGroups, createPackage)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..), SrcPkgName(SrcPkgName))
import Debian.Releases (baseRelease, BaseRelease(..))
import Debian.Repo.Internal.Apt (getApt, MonadApt, AptImage(aptSourcePackageCache))
import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Fingerprint (RetrieveMethod(Apt, Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec(Commit, Branch))
import Debian.Repo.PackageIndex (SourcePackage(sourcePackageID))
import Debian.Repo.PackageID (packageName, packageVersion)
import Debian.Version (DebianVersion, prettyDebianVersion)

buildTargets7 :: Monad m => TSt m ()
buildTargets7 = do
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [] >>= inGroups ["ghcjs-comp"]
  _haddock_api7 <-
      hackage (Just "2.16.1") "haddock-api" >>=
             flag (P.CabalDebian ["--default-package", "haddock-api"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>=
             -- FIXME - This cabal-debian stuff does nothing because this isn't a Debianize target
             apply (execCabalM $ (debInfo . rulesFragments) %=
                                         Set.insert (Text.unlines [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                  , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"])) >>=
             debianize [] >>= inGroups ["ghcjs-comp"]
  _haddock_library7 <- hackage (Just "1.2.1") "haddock-library" >>= flag (P.DebVersion "1.2.1-2") >>= debianize [] >>= ghcjs_also
  _ghcjs_dom <- hackage (Just "0.2.4.0" {-"0.7.0.4"-} {-"0.4.0.0"-}) "ghcjs-dom" >>= debianize [] >>= inGroups ["glib"] >>= ghcjs
  _ghcjs_dom_hello <- hackage (Just "2.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs >>=
                      skip (Reason "see cairo and glib")
  buildTargets

buildTargets8 :: Monad m => TSt m ()
buildTargets8 = do
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp"]
  _ghc8 <- apt "sid" "ghc" >>=
           patch $(embedFile "patches/ghc.diff") >>=
           inGroups ["ghc8-comp"]
  _haddock_api8 <-
      hackage (Just "2.17.3") "haddock-api" >>=
             flag (P.CabalDebian ["--default-package", "haddock-api"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>=
{-
             -- This breaks the build
             apply (execCabalM $ (debInfo . rulesFragments) %=
                                         Set.insert (Text.unlines [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                  , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"])) >>=
-}
             debianize [] >>= inGroups ["ghcjs-comp"]
  _haddock_library8 <- hackage (Just "1.4.2") "haddock-library" >>= debianize [] >>= ghcjs_also
  _cabal_install <- hackage (Just "1.24.0.2") "cabal-install" >>=
                    debianize [] >>=
                    -- patch $(embedFile "patches/cabal-install.diff") >>= -- cabal-debian-4.35.7 outputs libghc-cabal-dev | ghc instead of ghc | libghc-cabal-dev
                    flag (P.CabalDebian ["--default-package", "cabal-install"]) >>=
                    inGroups []


  _ghcjs_dom <- hackage (Just "0.9.1.1") "ghcjs-dom" >>= debianize [] >>= inGroups ["glib"] >>= ghcjs
  _ghcjs_dom_hello <- hackage (Just "6.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs >>=
                      skip (Reason "see cairo and glib")
  buildTargets


buildTargets :: Monad m => TSt m ()
buildTargets = do
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "2668216c654b0b302cb51162b2246c39cd6adc1e"] >>=
      -- git "https://github.com/ddssff/haskell-devscripts" [Branch "0.12"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  -- _ghc_boot <- hackage (Just "8.0.1") "ghc-boot" >>= debianize [] -- Required by haddock-api
  _zlib <- hackage (Just "0.6.1.1") "zlib" >>= flag (P.DebVersion "0.6.1.1-1") >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also

  -- Trusty targets
  commonTargets

  return ()

