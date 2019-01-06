{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-name-shadowing #-}

module Debian.AutoBuilder.Details.Xenial ( buildTargets84 ) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Details.Common (TSt, ghcjs_only, ghcjs_also, skip, substitute, Reason(..))
import Debian.AutoBuilder.Details.CommonTargets (commonTargets)
import Debian.AutoBuilder.Types.Packages as P
    (apply, apt, debdir, debianize, flag, git, hackage, uri, inGroups,
     PackageFlag(BuildDep, CabalDebian, DebVersion, DevelDep, NoDoc, RelaxDep), patch, PackageId)
import Debian.Repo.Fingerprint

buildTargets :: Monad m => TSt m ()
buildTargets = do
  -- _ghcjs_dom <- hackage (Just "0.9.1.1") "ghcjs-dom" >>= debianize [] >>= inGroups ["glib"] >>= ghcjs_only
  _ghcjs_dom_hello <- hackage (Just "6.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs_only >>=
                      skip (Reason "see cairo and glib")
  _haskell_devscripts <-
      -- Current version as of 26 Apr 2018
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "6e1e94bc4efd8a0ac37f34ac84f4813bcb0105cc"] >>=
      -- New repository
      git "https://salsa.debian.org/haskell-team/haskell-devscripts.git" [] >>=
      inGroups ["platform"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      -- Also changes the enable profiling flags
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal")
  -- haskell-names-0.9.1 requires aeson<1.3
  _haskell_names <- hackage (Just "0.9.6") "haskell-names" >>= debianize []

  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= flag (P.DebVersion "1.0.0.7-5build1") >>= debianize [] >>= inGroups ["autobuilder-group"] >>= ghcjs_also
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= flag (P.DebVersion "1.1.0.3-5build1") >>= debianize [] >>= inGroups ["autobuilder-group"] >>= ghcjs_also

  _traverse_with_class <- hackage (Just "1.0.0.0") "traverse-with-class" >>= debianize [] >>= inGroups ["happstack", "important"]
  _zlib <- hackage (Just "0.6.2") "zlib" >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  commonTargets

nodejs :: Monad m => TSt m PackageId
nodejs =
    uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
    debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~xenial1.debian.tar.xz" "0083c158831134295e719a524d9c8513") >>=
    flag (P.RelaxDep "libssl-dev") >>=
    inGroups ["ghcjs-comp"]

buildTargets84 :: Monad m => TSt m ()
buildTargets84 = do
  -- These are targets likely to change when we go from 8.0 to 8.2.
  -- Some of these are needed for ghc-8.2, some are wrong.
  -- _ghc84 <- apt "experimental" "ghc" >>= patch $(embedFile "patches/ghc84.diff") >>= inGroups ["ghc8-comp"]
  _ghc86 <- apt "experimental" "ghc" >>= patch $(embedFile "patches/ghc86.diff") >>= inGroups ["ghc8-comp"]
  -- Cabal 2.0.1.0 is included in ghc-8.2.2
  -- _cabal <- hackage (Just "1.24.0.0") "Cabal" >>= debianize []
  -- Patch removes the version range from the Cabal dependency so the
  -- debian/control file references the virtual libghc-cabal-dev
  -- package rather than a (non-existant) real deb.
  -- _cabal_install <- hackage (Just "2.2.0.0") "cabal-install" >>= patch $(embedFile "patches/cabal-install-22.diff") >>= debianize [] >>= flag (P.CabalDebian ["--default-package", "cabal-install"]) >>= inGroups ["ghc8-comp"]
  _cabal_install <- hackage (Just "2.4.1.0") "cabal-install" >>= patch $(embedFile "patches/cabal-install-24.diff") >>= debianize [] >>= flag (P.CabalDebian ["--default-package", "cabal-install"]) >>= inGroups ["ghc8-comp"]
  -- building with commit d1ded1f890e7ebebbbb462bb5a321983bb6be914
  -- _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.4"] >>= inGroups ["ghcjs-comp"]
  -- _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.6"] >>= inGroups ["ghcjs-comp"]
  -- singletons 2.2 requires base-4.9, supplied with ghc-8.0
  -- singletons 2.3.1 requires base-4.10, supplied with ghc-8.2
  -- singletons 2.4.1 requires base-4.11, supplied with ghc-8.4
  -- singletons 2.5.1 requires base-4.12, supplied with ghc-8.6 (I assume)
  -- _singletons <- hackage (Just "2.4.1") "singletons" >>= debianize [] >>= inGroups ["tmp"] {->>= ghcjs_also-}
  _singletons <- hackage (Just "2.5.1") "singletons" >>= debianize []
  -- _haddock_library <- hackage (Just "1.6.0") "haddock-library" >>= patch $(embedFile "patches/haddock-library-1.6.0.diff") >>= flag (P.BuildDep "hspec-discover") >>= inGroups ["ghcjs-comp", "appraisalscribe", "pandoc", "tmp"] >>= debianize [] >>= ghcjs_also
  _haddock_library <- hackage (Just "1.7.0") "haddock-library" >>= flag (P.BuildDep "hspec-discover") >>= inGroups ["ghcjs-comp", "appraisalscribe", "pandoc"] >>= debianize [] >>= ghcjs_also
      -- Version 1.4.4 is required by haddock-api-2.18.1, the next
      -- haddock-api requires version 1.5 and ghc-8.4.

      -- We need a doc package, otherwise the underlying package has
      -- a pseudo-dependency on haddock-interface-28 instead of 33.

      -- haddock-library has two "library" sections in its cabal file, which cabal
      -- debian (and haskell-devscripts) cannot handle.  Remove the second one
      -- and just use the available attoparsec library.
      -- 2.18.1 requires ghc-8.2, 2.19.0.1 requires ghc-8.4.1.  2.20.0 requires ghc-8.4.2.

  -- _haddock_api8 <- hackage (Just "2.20.0") "haddock-api" >>= inGroups ["ghcjs-comp", "tmp"] >>=
  _haddock_api8 <- hackage (Just "2.21.0") "haddock-api" >>= patch $(embedFile "patches/haddock-api.diff") >>= inGroups ["ghcjs-comp"] >>=
             flag (P.CabalDebian ["--default-package", "haddock-api"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>=
             debianize [] >>= inGroups ["ghcjs-comp"]
  -- pandoc>=2.4 requires haddock-library-1.7
  _pandoc <- hackage (Just "2.5") "pandoc" >>= patch $(embedFile "patches/pandoc-2.5.diff") >>=
  -- _pandoc <- hackage (Just "2.3.1") "pandoc" >>= patch $(embedFile "patches/pandoc.diff") >>=
             flag (P.CabalDebian ["--executable", "pandoc"]) >>=
             -- flag (P.CabalDebian ["--executable", "try-pandoc"]) >>=
             flag (P.CabalDebian ["--default-package", "pandoc-data"]) >>=
             flag (P.BuildDep "alex") >>=
             flag (P.BuildDep "happy") >>=
             debianize [] >>=
             inGroups ["appraisalscribe", "important", "pandoc", "tmp"]
  _uri_bytestring_ghc <- hackage (Just "0.3.2.1") "uri-bytestring" >>= {-patch $(embedFile "patches/uri-bytestring.diff") >>=-} debianize [] >>= inGroups ["servant"] >>= ghcjs_also
  _zlib <- hackage (Just "0.6.2") "zlib" >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform", "ghc8-comp"] >>= ghcjs_also
  -- parsec-3 is built into ghc-8.4.3, and the deb name is libghc-parsec-dev, not parsec3.
  -- text-1.2.3.0 is built into ghc-8.4.3
  -- nodejs-8.10 is available from ubuntu bionic

  buildTargets
