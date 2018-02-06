{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}

module Debian.AutoBuilder.Details.Xenial ( buildTargets7, buildTargets8 ) where

import Control.Lens ((%=))
--import Control.Monad.Trans (lift)
--import Data.Char (toLower)
import Data.FileEmbed (embedFile)
--import Data.Function (on)
--import Data.List (sortBy)
--import Data.Map as Map (elems, keys)
import Data.Set as Set (insert)
import Data.Text as Text (unlines)
--import Data.Version (Version(Version))
import Debian.AutoBuilder.Details.Common (TSt, ghcjs, ghcjs_also, skip, Reason(..)) -- (named, ghcjs_flags, putSrcPkgName)
import Debian.AutoBuilder.Details.CommonTargets (commonTargets)
import Debian.AutoBuilder.Types.Packages as P
    (apply, apt, debdir, debianize, flag, git, hackage, uri, inGroups, PackageFlag(CabalDebian, DebVersion, DevelDep, RelaxDep), patch, PackageId)
--import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D (execCabalM, rulesFragments, debInfo)
--import Debian.Relation (BinPkgName(..), SrcPkgName(SrcPkgName))
--import Debian.Releases (baseRelease, BaseRelease(..))
--import Debian.Repo.Internal.Apt (getApt, MonadApt, AptImage(aptSourcePackageCache))
--import Debian.Repo.Internal.Repos (MonadRepos)
import Debian.Repo.Fingerprint
--import Debian.Repo.PackageIndex (SourcePackage(sourcePackageID))
--import Debian.Repo.PackageID (packageName, packageVersion)
--import Debian.Version (DebianVersion, prettyDebianVersion)

buildTargets7 :: Monad m => TSt m ()
buildTargets7 = do
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [] >>= inGroups ["ghcjs-comp"]
  _nodejs <- nodejs
  -- _binary <- hackage (Just "0.8.4.0") "binary" >>= debianize [] >>= ghcjs_also
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
  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= flag (P.DebVersion "1.0.0.7-2") >>= debianize []
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= flag (P.DebVersion "1.1.0.3-2") >>= debianize []
  _ghcjs_dom <- hackage (Just "0.2.4.0" {-"0.7.0.4"-} {-"0.4.0.0"-}) "ghcjs-dom" >>= debianize [] >>= inGroups ["glib"] >>= ghcjs
  _ghcjs_dom_hello <- hackage (Just "2.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs >>=
                      skip (Reason "see cairo and glib")
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  -- _ghc_boot <- hackage (Just "8.0.1") "ghc-boot" >>= debianize [] -- Required by haddock-api
  -- traverse-with-class-1.0.0.0 requires base >= 4.9, aka ghc8.  This blocks haskell-names-0.9.
  _traverse_with_class <- hackage (Just "0.2.0.4") "traverse-with-class" >>= debianize [] >>= inGroups ["happstack", "important"]
  -- Unfortunately this package requires haskell-src-exts<1.19, which is gone
  _haskell_names <- hackage (Just "0.8.0") "haskell-names" >>= debianize []
  _singletons <- hackage (Just "2.1") "singletons" >>= debianize []
  buildTargets

nodejs :: Monad m => TSt m PackageId
nodejs =
    uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
    debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~xenial1.debian.tar.xz" "0083c158831134295e719a524d9c8513") >>=
    flag (P.RelaxDep "libssl-dev") >>=
    inGroups ["ghcjs-comp"]

buildTargets8 :: Monad m => TSt m ()
buildTargets8 = do
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp"]
  _nodejs <- nodejs
  -- _ghc8 <- apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc8-comp"]
  _haddock_api8 <-
      hackage (Just "2.17.4") "haddock-api" >>=
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
  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= flag (P.DebVersion "1.0.0.7-2") >>= debianize [] >>= ghcjs_also
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= flag (P.DebVersion "1.1.0.3-2") >>= debianize [] >>= ghcjs_also
  -- We don't need this until ghc-8.2.2, right?
  -- _cabal <- hackage (Just "2.0.0.2") "Cabal" >>= debianize []

  -- 1.24.0.2 is the last cabal-install that supported the topdown solver
  -- (which apparently doesn't work, but we can't build ghcjs without it.)
  _cabal_install <- hackage (Just "1.24.0.2") "cabal-install" >>=
                    -- Avoid creating a versioned libghc-cabal-dev
                    -- dependency, as it is a virtual package in ghc
                    patch $(embedFile "patches/cabal-install.diff") >>=
                    debianize [] >>=
                    -- Allow building with Cabal-2
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
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      -- Changes from debian since 0.11.2
      patch $(embedFile "patches/haskell-devscripts-debian.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  -- _ghc_boot <- hackage (Just "8.0.1") "ghc-boot" >>= debianize [] -- Required by haddock-api
  _traverse_with_class <- hackage (Just "1.0.0.0") "traverse-with-class" >>= debianize [] >>= inGroups ["happstack", "important"]
  _haskell_names <- hackage (Just "0.9.1") "haskell-names" >>= debianize []
  _singletons <- hackage (Just "2.3.1") "singletons" >>= debianize [] -- 2.4 requires base-4.11
  buildTargets


buildTargets :: Monad m => TSt m ()
buildTargets = do
  _zlib <- hackage (Just "0.6.1.1") "zlib" >>= flag (P.DebVersion "0.6.1.1-1") >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also

  -- Trusty targets
  commonTargets

  return ()

