-- THIS FILE IS WHERE YOU DO ALL THE CUSTOMIZATIONS REQUIRED FOR THE
-- REPOSTORIES YOU ARE BUILDING.  The Targets.hs file may also be
-- modified to reflect the sources for the packages you will be
-- building.  If you find yourself modifying other files I would like
-- to hear about it.

{-# LANGUAGE CPP, FlexibleContexts #-}
{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Details
    ( myParams
    ) where

import Control.Lens -- (use, view, (%=))
import Control.Monad (when)
import Control.Monad.State (execState {-, modify, MonadState-})
-- import Data.Map as Map (elems, insert, map)
import Data.Maybe
import Data.Version (showVersion, Version(Version))
import Debian.AutoBuilder.Details.Sources (myUploadURI, myBuildURI, myReleaseAliases, releaseRepoName, mySources)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.DefaultParams (defaultParams)
import Debian.AutoBuilder.Types.Packages (TSt)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.GHC (hvrCabalVersion)
import Debian.Relation (BinPkgName(..))
import Debian.Releases as Releases
    (Release(..), releaseString, parseReleaseName, isPrivateRelease, baseRelease, Distro(..))
import Debian.Repo.Slice (Slice, PPASlice{-(PersonalPackageArchive, ppaUser, ppaName)-})
import Debian.Version (parseDebianVersion')
import qualified Debian.AutoBuilder.Details.Targets as Targets
import Prelude hiding (map)

myParams :: FilePath -> Release -> ParamRec
myParams home myBuildRelease =
    let myUploadURIPrefix = "ssh://upload@deb.seereason.com/srv"
        myBuildURIPrefix = "http://deb.seereason.com"
        params = (defaultParams (releaseString myBuildRelease)
                   myCompilerVersion
                   myUploadURIPrefix
                   myBuildURIPrefix
                   myDevelopmentReleaseNames)
                 { vendorTag = myVendorTag
                 , hvrVersion = myCompilerVersion
                 , oldVendorTags = ["seereason"]
                 , autobuilderEmail = "SeeReason Autobuilder <partners@seereason.com>"
                 , releaseSuffixes = myReleaseSuffixes
                 , extraRepos = myExtraRepos
                 , uploadURI = myUploadURI myBuildRelease
                 , buildURI = myBuildURI myBuildRelease
                 , sources = mySources myBuildRelease
                 , globalRelaxInfo = myGlobalRelaxInfo
                 , includePackages = myIncludePackages myBuildRelease
                 , optionalIncludePackages = myOptionalIncludePackages
                 , excludePackages = myExcludePackages myBuildRelease
                 , components = myComponents myBuildRelease
                 , developmentReleaseNames = myDevelopmentReleaseNames
                 , releaseAliases = myReleaseAliases myBuildRelease
                 , newDistProgram = "newdist --sender-email=autobuilder@seereason.com --notify-email dsf@seereason.com --notify-email beshers@seereason.com --notify-email jeremy@seereason.com"
                 -- 6.14 adds the ExtraDevDep parameter.
                 -- 6.15 changes Epoch parameter arity to 2
                 -- 6.18 renames type Spec -> RetrieveMethod
                 -- 6.35 added the CabalDebian flag
                 -- 6.64 removes the myCompilerVersion argument from defaultParams
                 , requiredVersion = [(parseDebianVersion' ("6.64" :: String), Nothing)]
                 , hackageServer = myHackageServer
                 }
        rel = parseReleaseName (buildRelease params)
        st = execState (myKnownTargets params) (P.targetState rel home) in
    params {knownPackages = view P.packageMap st}

-- https://launchpad.net/~hvr/+archive/ubuntu/ghc
myExtraRepos :: [Either Slice PPASlice]
myExtraRepos = [{-Right (PersonalPackageArchive {ppaUser = "hvr", ppaName = "ghc"})-}]

-- This section has all the definitions relating to the particular
-- suffixes we will use on our build releases.
--
myReleaseSuffixes :: [String]
myReleaseSuffixes = ["-seereason", "-private"]

--
-- End of release suffix section.

-- The current list of development releases.  The version numbers for
-- these releases do not need to be tagged with the base release name,
-- only with the vendor tag.  Sid is always a development release,
-- Ubuntu creates a new one for each cycle.
--
myDevelopmentReleaseNames :: [String]
myDevelopmentReleaseNames = ["sid", "quantal"]

-- This tag is used to construct the customized part of the version
-- number for any package the autobuilder builds.
--
myVendorTag :: String
myVendorTag = "+seereason"

--myDiscards :: Set.Set String
--myDiscards = Set.empty

-- The set of all known package targets.  The targets we will
-- actually build are chosen from these.  The myBuildRelease argument
-- comes from the autobuilder argument list.
--
myKnownTargets :: ParamRec -> TSt ()
myKnownTargets params = do
  rel <- use P.release
  if isPrivateRelease rel
  then Targets.private params
  else (Targets.public params >> when (testWithPrivate params) (Targets.private params))

-- Additional packages to include in the clean build environment.
-- Adding packages here can speed things up when you are building many
-- packages, because for each package it reverts the build environment
-- to the clean environment and then installs all the build
-- dependencies.  This only affects newly created environments, so if
-- you change this value use the flushRoot option to get it to take
-- effect.
--
-- Note that these packages must exist and be valid at the time the
-- environment is created.  If there is a package that you want in the
-- clean environment that isn't available in the base repository (e.g.
-- seereason-keyring) you currently need to first build it and then
-- install it manually.
--
myIncludePackages :: Release -> [BinPkgName]
myIncludePackages myBuildRelease =
    fmap BinPkgName
    [ "debian-archive-keyring"
    , "build-essential"         -- This is required by autobuilder code that opens the essential-packages list
    , "pkg-config"              -- Some packages now depend on this package via new cabal options.
    , "debian-keyring"
    , "locales"
    , "software-properties-common" -- Required to run add-apt-repository to use a PPA.
    -- , "cabal-install"
    ] ++
    -- Private releases generally have ssh URIs in their sources.list,
    -- I have observed that this solves the "ssh died unexpectedly"
    -- errors.
    (if isPrivateRelease myBuildRelease then [BinPkgName "ssh"] else []) ++
    case releaseRepoName (baseRelease myBuildRelease) of
      Releases.Debian -> []
      Ubuntu -> [BinPkgName "ubuntu-keyring"]
      _ -> error $ "Invalid base distro: " ++ show myBuildRelease

-- This will not be available when a new release is created, so we
-- have to make do until it gets built and uploaded.
myOptionalIncludePackages :: [BinPkgName]
myOptionalIncludePackages =
    fmap BinPkgName
    [ "seereason-keyring"
    -- This pulls in dependencies required for some pre-build tasks,
    -- e.g. libghc-cabal-debian-dev which is needed to run
    -- Debianize.hs scripts.  But do we really need this in clean?
    , "autobuilder-seereason"
    -- You may need to omit ghc and ghcjs and flush the root to build
    -- ghcjs.  We need ghc and ghcjs to figure out bundled package
    -- lists.  Just be careful when trying to upgrade the compiler, if
    -- you need to back a build out you will tear your hair out
    -- figureing why the new compiler is still there!
    -- , "ghc-" ++ showVersion v
    -- , "ghcjs"
    -- the happy dependency list is (was?) broken, so installing this
    -- helps it build when necessary
    -- , "happy-" ++ showVersion (hvrHappyVersion v)
    ] ++
    (case myCompilerVersion of
       Nothing ->
           [BinPkgName "ghc",
            BinPkgName "cabal-install",
            BinPkgName "ghcjs"]
       Just v ->
           [BinPkgName ("ghc-" ++ showVersion v),
            BinPkgName ("cabal-install-" ++ showVersion (hvrCabalVersion v)),
            -- Switch to ghcjs-8.0 once it is built
            BinPkgName "ghcjs-8.0.1"])

myCompilerVersion :: Maybe Version
myCompilerVersion = Nothing -- Just use the package named ghc
-- myCompilerVersion = Just (Version [8,0,1] []) -- Use a specific version of ghc and ghcjs

myExcludePackages :: Release -> [BinPkgName]
myExcludePackages _ = []

myComponents :: Release -> [String]
myComponents myBuildRelease =
    case releaseRepoName (baseRelease myBuildRelease) of
      Releases.Debian -> ["main", "contrib", "non-free"]
      Ubuntu -> ["main", "restricted", "universe", "multiverse"]
      _ -> error $ "Invalid base distro: " ++ show myBuildRelease

myHackageServer :: String
myHackageServer = "hackage.haskell.org"
-- myHackageServer = "hackage.factisresearch.com"

-- Any package listed here will not trigger rebuilds when updated.
--
myGlobalRelaxInfo :: [String]
myGlobalRelaxInfo =
    [ "autoconf"
    , "base-files"
    , "base-passwd"
    , "bash"
    , "binutils"
    , "bsdutils"
    , "cabal-install"
    , "cdbs"
    , "coreutils"
    , "debhelper"
    , "debianutils"
    , "devscripts"
    , "diff"
    , "dpkg"
    , "dpkg-dev"
    , "e2fsprogs"
    , "findutils"
    , "flex"
    , "g++"
    , "gcc"
    , "ghc" -- ghc has a self dependency, so without this it would build every time
    , "grep"
    , "gs"
    , "gzip"
    , "haskell-devscripts"
    , "haskell-devscripts-minimal"
    , "hostname"
    , "intltool"
    , "libc6-dev"
    , "libc-bin"
    , "login"
    , "make"
    , "mktemp"
    , "mount"
    , "ncurses-base"
    , "ncurses-bin"
    , "perl"
    , "perl-base"
    , "python-minimal"
    , "sed"
    , "sysvinit"
    , "sysvinit-utils"
    , "tar"
    , "util-linux" ]

