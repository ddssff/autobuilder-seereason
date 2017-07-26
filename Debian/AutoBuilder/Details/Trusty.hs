{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Trusty (buildTargets) where

import Control.Lens (at, use, view, (%=))
import Data.FileEmbed (embedFile)
import Data.Map as Map (elems, keys)
import Data.Set as Set (fromList, insert, member, Set)
import Data.Text as Text (unlines)
import Data.Version (Version(Version))
import Debian.AutoBuilder.Details.Common -- (named, ghcjs_flags, putSrcPkgName)
import qualified Debian.AutoBuilder.Details.Xenial as Xenial (buildTargets7)
-- import Debian.AutoBuilder.Details.DebVersion (setDebVersion)
import Debian.AutoBuilder.Types.Packages as P (depends,
                                               PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           NoDoc, UDeb, OmitLTDeps, SkipVersion), packageMap,
                                               pid, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri, spec, clonePackage,
                                               GroupName, inGroups, createPackage)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec(Commit, Branch))
import Debian.Repo.Internal.Apt (MonadApt)
import Debian.Repo.Internal.Repos (MonadRepos)

#if 1

buildTargets :: Monad m => ParamRec -> TSt m ()
buildTargets params = do
  _ghc <- apt "jessie" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc-comp"]
  Xenial.buildTargets7

#else

buildTargets :: Monad m => ParamRec -> TSt m ()
buildTargets params = do
#if 0
  let ghc8 :: Monad m => TSt m a -> TSt (Maybe a)
      ghc8 action = if hvrVersion params >= Just (Version [8] []) then Just <$> action else pure Nothing
      ghc7 :: Monad m => TSt m a -> TSt (Maybe a)
      ghc7 action = if hvrVersion params <  Just (Version [8] []) then Just <$> action else pure Nothing
#else
  -- Still using ghc7
  let ghc8 :: Monad m => TSt m a -> TSt m (Maybe a)
      ghc8 action = pure Nothing
      ghc7 :: Monad m => TSt m a -> TSt m (Maybe a)
      ghc7 action = Just <$> action
#endif
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "2668216c654b0b302cb51162b2246c39cd6adc1e"] >>=
      -- git "https://github.com/ddssff/haskell-devscripts" [Branch "0.12"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"]
  -- ghc76 <- ghcFlags $ apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff")
  -- ghc78 <- ghcFlags $ apt "experimental" "ghc" >>= patch $(embedFile "patches/trac9262.diff")
  -- _ghc710 <- apt "experimental" "ghc" >>= ghcFlags
  --                       >>= patch $(embedFile "patches/ghc.diff")
  --                           >>= skip (Reason "stick with current, avoid huge rebuild")
  _ghc8 <- apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc"] >>= skip (Reason "Waiting for interactive support in ghcjs-0.2.1")
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [] >>= relax "cabal-install" >>= inGroups ["ghcjs-comp"]

  _alex <- hackage (Just "3.1.7") "alex" >>=
           patch $(embedFile "patches/alex.diff") >>=
           flag (P.CabalDebian ["--default-package", "alex"]) >>=
           flag (P.RelaxDep "alex") >>=
           flag (P.BuildDep "happy") >>=
           debianize [] >>=
           inGroups []
  _dataenc <- hackage (Just "0.14.0.7") "dataenc" >>= debianize []
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
  _happy <- hackage (Just "1.19.5") "happy"
            >>= flag (P.CabalDebian ["--executable", "happy"])
             -- >>= flag (P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"),
            >>= apply (execCabalM $ do mapM_ (\ name -> (debInfo . atomSet) %= (Set.insert $ InstallData (BinPkgName "happy") name name))
                                               [ "HappyTemplate-arrays-coerce"
                                               , "GLR_Lib-ghc"
                                               , "HappyTemplate-arrays-ghc-debug"
                                               , "HappyTemplate"
                                               , "GLR_Lib"
                                               , "HappyTemplate-arrays-debug"
                                               , "GLR_Base"
                                               , "HappyTemplate-ghc"
                                               , "HappyTemplate-arrays-ghc"
                                               , "HappyTemplate-coerce"
                                               , "HappyTemplate-arrays"
                                               , "HappyTemplate-arrays-coerce-debug"
                                               , "GLR_Lib-ghc-debug" ] )
           >>= flag (P.RelaxDep "happy")
           >>= flag (P.BuildDep "happy")
           >>= debianize []
           >>= inGroups []
  _cabal <- hackage (Just "1.22.8.0") "Cabal" >>= debianize []
  _hS3 <- git "https://github.com/scsibug/hS3.git" [] >>= debianize [] >>=
          apply (execCabalM $ doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
  _memoTrie <- (hackage (Just "0.6.4") "MemoTrie") >>= debianize []
{-
  _microlens <- hackage (Just "0.4.6.0") "microlens" >>= debianize [] >>= ghcjs_also
  _microlens_dev <- hackage (Just "0.4.6.0") "microlens-dev" >>= debianize [] >>= ghcjs_also
  _microlens_ghc <- hackage (Just "0.4.6.0") "microlens-ghc" >>= debianize [] >>= ghcjs_also
  _microlens_mtl <- hackage (Just "0.1.9.0") "microlens-mtl" >>= debianize [] >>= ghcjs_also
  _microlens_platform <- hackage (Just "0.3.4.0") "microlens-platform" >>= debianize [] >>= ghcjs_also
  _microlens_th <- hackage (Just "0.4.0.0") "microlens-th" >>= debianize [] >>= ghcjs_also
  _microlens_compat <-
      git "https://github.com/seereason/microlens-compat.git" [] >>=
             apply (replacement "microlens-compat" "lens") >>=
             debianize [] >>= inGroups ["ghc-libs", "th-path", "important"] >>= ghcjs_also
-}
  _vector_space <- (hackage (Just "0.10.3") "vector-space") >>= debianize []
  _vty <- (hackage (Just "5.7.1") "vty") >>= debianize []
  _yi_language <- (hackage (Just "0.2.1") "yi-language" >>= flag (P.BuildDep "alex")) >>= debianize []
  -- _zlib <- hackage (Just "0.5.4.2") "zlib" >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _zlib <- hackage (Just "0.6.1.1") "zlib" >>= flag (P.DebVersion "0.6.1.1-1") >>= flag (P.DevelDep "zlib1g-dev") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also

  commonTargets
#endif
