{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}

module Debian.AutoBuilder.Details.Xenial ( buildTargets82, buildTargets80 ) where

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
import Debian.AutoBuilder.Details.Common (TSt, ghcjs_only, ghcjs_also, skip, Reason(..)) -- (named, ghcjs_flags, putSrcPkgName)
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

buildTargets :: Monad m => TSt m ()
buildTargets = do
  _ghcjs_dom <- hackage (Just "0.9.1.1") "ghcjs-dom" >>= debianize [] >>= inGroups ["glib"] >>= ghcjs_only
  _ghcjs_dom_hello <- hackage (Just "6.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs_only >>=
                      skip (Reason "see cairo and glib")
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
  -- Unfortunately this package requires haskell-src-exts<1.19, which is gone
  _haskell_names <- hackage (Just "0.9.1") "haskell-names" >>= debianize []
  _nodejs <- nodejs

  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= flag (P.DebVersion "1.0.0.7-2") >>= debianize [] >>= ghcjs_also
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= flag (P.DebVersion "1.1.0.3-2") >>= debianize [] >>= ghcjs_also

  _traverse_with_class <- hackage (Just "1.0.0.0") "traverse-with-class" >>= debianize [] >>= inGroups ["happstack", "important"]
  _zlib <- hackage (Just "0.6.1.1") "zlib" >>= flag (P.DebVersion "0.6.1.1-1") >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  commonTargets

nodejs :: Monad m => TSt m PackageId
nodejs =
    uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
    debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~xenial1.debian.tar.xz" "0083c158831134295e719a524d9c8513") >>=
    flag (P.RelaxDep "libssl-dev") >>=
    inGroups ["ghcjs-comp"]

buildTargets80 :: Monad m => TSt m ()
buildTargets80 = do
  _ghc8 <- apt "buster" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc8-comp"]
  _cabal_install <- hackage (Just "1.24.0.2") "cabal-install" >>=
                    -- Avoid creating a versioned libghc-cabal-dev
                    -- dependency, as it is a virtual package in ghc
                    patch $(embedFile "patches/cabal-install.diff") >>=
                    debianize [] >>=
                    -- Allow building with Cabal-2
                    flag (P.CabalDebian ["--default-package", "cabal-install"])
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp", "ghcjs-only"]
  _haddock_library8 <- hackage (Just "1.4.2") "haddock-library" >>= debianize [] >>= ghcjs_also
  -- singletons 2.2 requires base-4.9, supplied with ghc-8.0
  -- singletons 2.3.1 requires base-4.10, supplied with ghc-8.2
  -- singletons 2.4.1 requires base-4.11, supplied with ghc-8.4
  _singletons_ghc <- hackage (Just "2.4.1") "singletons" >>= debianize [] >>= ghcjs_also
  _uri_bytestring <- hackage (Just "0.3.1.1") "uri-bytestring" >>= debianize [] >>= inGroups ["servant"] >>= ghcjs_also
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      -- Changes from debian since 0.11.2
      patch $(embedFile "patches/haskell-devscripts-debian.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  buildTargets

-- Currently we just have an experimental haskell-devscripts in the 82
-- repo, it uses the current git repo and tries to fix the profiling flags.
buildTargets82 :: Monad m => TSt m ()
buildTargets82 = do
  -- These are targets likely to change when we go from 8.0 to 8.2.
#if 1
  _ghc8 <- apt "buster" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc8-comp"] >>= skip (Reason "We have revision 10, but buster now has revision 11")
  _cabal_install <- hackage (Just "1.24.0.2") "cabal-install" >>=
                    -- Avoid creating a versioned libghc-cabal-dev
                    -- dependency, as it is a virtual package in ghc
                    patch $(embedFile "patches/cabal-install.diff") >>=
                    debianize [] >>=
                    -- Allow building with Cabal-2
                    flag (P.CabalDebian ["--default-package", "cabal-install"])
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp", "ghcjs-only"]
  _haddock_library8 <- hackage (Just "1.4.2") "haddock-library" >>= debianize [] >>= ghcjs_also
  -- singletons 2.2 requires base-4.9, supplied with ghc-8.0
  -- singletons 2.3.1 requires base-4.10, supplied with ghc-8.2
  -- singletons 2.4.1 requires base-4.11, supplied with ghc-8.4
  _singletons_ghc <- hackage (Just "2.4.1") "singletons" >>= debianize [] >>= ghcjs_also
  _uri_bytestring <- hackage (Just "0.3.1.1") "uri-bytestring" >>= debianize [] >>= inGroups ["servant"] >>= ghcjs_also
#else
  -- Some of these are needed for ghc-8.2, some are wrong.
  _ghc82 <- apt "sid" "ghc" >>= patch $(embedFile "patches/ghc822.diff") >>= inGroups ["ghc8-comp"]
  _cabal_install <- hackage (Just "2.0.0.1") "cabal-install" >>=
                    debianize [] >>=
                    flag (P.CabalDebian ["--default-package", "cabal-install"])
  -- Stick with ghc-7.10 version of ghcjs, that's what Jeremy is using.
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [] >>= inGroups ["ghcjs-comp", "ghcjs-only"]
  _singletons_ghc <- hackage (Just "2.2") "singletons" >>= debianize []
  _singletons_ghcjs <- hackage (Just "2.1") "singletons" >>= debianize [] >>= ghcjs_only
  _haddock_library82 <- hackage (Just "1.4.5") "haddock-library" >>= debianize [] >>= ghcjs_also
  _uri_bytestring_ghc <- hackage (Just "0.3.1.1") "uri-bytestring" >>= debianize [] >>= inGroups ["servant"]
  _uri_bytestring_ghcjs <- hackage (Just "0.3.0.2") "uri-bytestring" >>= debianize [] >>= inGroups ["servant"] >>= ghcjs_only
#endif
  _haskell_devscripts <-
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git"
              -- Current version as of 26 Apr 2018
              [Commit "6e1e94bc4efd8a0ac37f34ac84f4813bcb0105cc"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      -- Also changes the enable profiling flags
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      -- Changes from debian since 0.11.2
      -- patch $(embedFile "patches/haskell-devscripts-debian.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  buildTargets
