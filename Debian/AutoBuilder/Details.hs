-- THIS FILE IS WHERE YOU DO ALL THE CUSTOMIZATIONS REQUIRED FOR THE
-- REPOSTORIES YOU ARE BUILDING.  The Targets.hs file may also be
-- modified to reflect the sources for the packages you will be
-- building.  If you find yourself modifying other files I would like
-- to hear about it.

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details
    ( myParams
    ) where

import Data.List as List (isSuffixOf, isPrefixOf, map)
import Data.Maybe
import Data.Monoid (mappend)
import Data.Set as Set (Set, empty, map)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (Packages(NoPackage), TargetName(TargetName))
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..), Strictness(..), TargetSpec(..))
import Debian.Release (ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.URI
import Debian.Version (parseDebianVersion)
import qualified Debian.AutoBuilder.Details.Targets as Targets
import Prelude hiding (map)

myParams :: FilePath -> String -> ParamRec
myParams home myBuildRelease =
    (\ params -> params {knownPackages = myKnownTargets home params}) $
    ParamRec
    { vendorTag = myVendorTag
    , oldVendorTags = ["seereason"]
    , autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.com>"
    , releaseSuffixes = myReleaseSuffixes
    , buildRelease = ReleaseName {relName = myBuildRelease}
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    -- What we plan to build
    , targets = TargetSpec {allTargets = False, targetNames = Set.empty}
    , doUpload = myDoUpload
    , doNewDist = myDoNewDist
    , flushPool = myFlushPool
    , useRepoCache = True
    , forceBuild = myForceBuild
    , buildTrumped = myBuildTrumped
    , doSSHExport = myDoSSHExport
    , report = False
    , doHelp = False
    -- Things that are occasionally useful
    , goals = myGoals
    , dryRun = False
    , allowBuildDependencyRegressions = False
    , setEnv = []
    , showSources = False
    , showParams = False
    , flushAll = False
    , flushSource = False
    , flushRoot = False
    , verbosity = myVerbosity
    , topDirParam = Nothing
    , createRelease = []
    , doNotChangeVersion = False
    -- Things that rarely change
    , sources = mySources myBuildRelease myDebianMirrorHost myUbuntuMirrorHost
    , globalRelaxInfo = myGlobalRelaxInfo
    , strictness = Lax
    , flushDepends = False
    , includePackages = myIncludePackages myBuildRelease
    , excludePackages = myExcludePackages myBuildRelease
    , components = myComponents myBuildRelease
    , ghcVersion = {- trace ("ghcVersion: " ++ show (myCompilerVersion myBuildRelease)) $ -} myCompilerVersion myBuildRelease
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = myReleaseAliases myBuildRelease
    , archList = [Binary "i386",Binary "amd64"]
    , newDistProgram = "newdist -v"
    -- 6.14 adds the ExtraDevDep parameter.
    -- 6.15 changes Epoch parameter arity to 2
    -- 6.18 renames type Spec -> RetrieveMethod
    -- 6.35 added the CabalDebian flag
    , requiredVersion = [(parseDebianVersion ("6.51" :: String), Nothing)]
    , hackageServer = myHackageServer
    -- Things that are probably obsolete
    , debug = False
    , discard = Set.map TargetName myDiscards
    , testWithPrivate = False
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    , knownPackages = NoPackage
    , buildPackages = NoPackage
    }

-- This section has all the definitions relating to the particular
-- suffixes we will use on our build releases.
--
myReleaseSuffixes = ["-seereason", "-private"]

-- Helper function to identify private releases.  This implies a different
-- upload uri, a different target list, and other as well.
--
isPrivateRelease release = isSuffixOf "-private" release

-- Our private releases are always based on our public releases, not
-- directly on upstream releases.
--
derivedReleaseNames myBuildRelease baseRelease =
    [baseRelease ++ "-seereason"] ++
    if isPrivateRelease myBuildRelease then [baseRelease ++ "-seereason-private"] else []

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateUploadURI else myPublicUploadURI)
    where
      myPrivateUploadURI = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName myBuildRelease
      myPublicUploadURI = "ssh://upload@deb.seereason.com/srv/deb/" ++ releaseRepoName myBuildRelease


-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
myBuildURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateBuildURI else myPublicBuildURI)
    where
      myPrivateBuildURI = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName myBuildRelease
      myPublicBuildURI = "http://deb.seereason.com/" ++ releaseRepoName myBuildRelease

--
-- End of release suffix section.

-- The current list of development releases.  The version numbers for
-- these releases do not need to be tagged with the base release name,
-- only with the vendor tag.  Sid is always a development release,
-- Ubuntu creates a new one for each cycle.
--
myDevelopmentReleaseNames = ["sid", "quantal"]

-- This tag is used to construct the customized part of the version
-- number for any package the autobuilder builds.
--
myVendorTag = "+seereason"

-- Put the names of any source packages you wish to rebuild whether or
-- not they appear to need it.  If you modified the package source but
-- did not modify the version number in the changelog this will force
-- a build.  This can lead to problems if you build the package for
-- multiple release or multiple architectures - you can end up with
-- different source for seemingly identical uploaded versions.  Add
-- elements from the command line using --force <name>.
--
myForceBuild = []

-- Packages we should build and upload even if their source code looks
-- older than the version already uploaded to the repository.

myBuildTrumped = []

-- Clear all the entries in the local pool before starting build.  Use
-- this when there is stuff already in there that you don't want to
-- upload to the remote repository.  Usually set from the command line
-- using --flush-pool.
--
myFlushPool = False

-- Make the output more or less chatty.  Zero is normal, -1 is
-- quieter, and so on.
--
myVerbosity = 0 :: Int

myDiscards :: Set.Set String
myDiscards = Set.empty

-- The set of all known package targets.  The targets we will
-- actually build are chosen from these.  The myBuildRelease argument
-- comes from the autobuilder argument list.
--
myKnownTargets :: FilePath -> ParamRec -> P.Packages
myKnownTargets home params =
    if isPrivateRelease rel
    then Targets.private home
    else mappend (Targets.public home rel) (if testWithPrivate params then Targets.private home else NoPackage)
    where
      rel = relName (buildRelease params)

-- If you are not interested in building everything, put one or more
-- source package names you want to build in this list.  Only these
-- packages and their build dependencies will be considered for
-- building.
--
myGoals = []

-- These host names are used to construct the sources.list lines to
-- access the Debian and Ubuntu repositories.  The anl.gov values here
-- probably won't work outside the United States.
--
--myDebianMirrorHost = "mirror.anl.gov"
myDebianMirrorHost = "ftp.debian.org"
--myUbuntuMirrorHost = "mirror.anl.gov"
myUbuntuMirrorHost = "us.archive.ubuntu.com/ubuntu"
--myUbuntuMirrorHost = "ubuntu.cs.utah.edu"
--myDebianMirrorHost = "mirrors.usc.edu/pub/linux/distributions"
--myUbuntuMirrorHost = "mirrors.usc.edu/pub/linux/distributions"

-- If true, upload the packages after a successful build
--
myDoUpload = False

-- If true, run newdist on the upload repository after a successful
-- build and upload, making them available to apt-get install.
--
myDoNewDist = False

-- If true, try to set up ssh access to the upload host if necessary.
--
myDoSSHExport = True

-- There is a debian standard for constructing the version numbers of
-- packages backported to older releases.  To follow this standard we
-- use bpo40+ for Debian 4.0, aka etch.  Don't start building for a new
-- Debian release without adding a bpo alias, otherwise you won't be able
-- to build when you add one because the existing packages will look too
-- new to trump.
--
-- (FIXME: This should be changed to a function.)
-- 
myReleaseAliases myBuildRelease =
    [("etch", "bpo40+"),
     ("lenny", "bpo50+"),
     ("squeeze", "bpo51+")] ++	-- Hopefully the actual version number when assigned will be greater
    concatMap (\ rel -> List.map (\ der -> (der, rel)) (derivedReleaseNames myBuildRelease rel)) ubuntuReleases

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
myIncludePackages myBuildRelease = 
    [ "debian-archive-keyring"
    , "build-essential"         -- This is required by autobuilder code that opens the essential-packages list
    , "pkg-config"              -- Some packages now depend on this package via new cabal options.
    -- , "perl-base"
    -- , "gnupg"
    -- , "dpkg"
    -- , "locales"
    -- , "language-pack-en"
    -- , "seereason-keyring"
    -- , "ghc6"
    -- , "ghc6-doc"
    -- , "ghc6-prof"
    -- , "makedev"
    ] ++
    -- Private releases generally have ssh URIs in their sources.list,
    -- I have observed that this solves the "ssh died unexpectedly"
    -- errors.
    (if isPrivateRelease myBuildRelease then ["ssh"] else []) ++
    case releaseRepoName myBuildRelease of
      "debian" -> []
      "ubuntu" ->
          ["ubuntu-keyring"] ++
          case () of
            _ | isPrefixOf "raring-" myBuildRelease -> []
              | isPrefixOf "quantal-" myBuildRelease -> []
              | isPrefixOf "precise-" myBuildRelease -> []
              | isPrefixOf "oneiric-" myBuildRelease -> []
              | isPrefixOf "natty-" myBuildRelease -> []
              | isPrefixOf "maverick-" myBuildRelease -> []
              | isPrefixOf "lucid-" myBuildRelease -> []
              | isPrefixOf "karmic-" myBuildRelease -> [{-"upstart"-}]
              | isPrefixOf "jaunty-" myBuildRelease -> [{-"upstart-compat-sysv"-}]
              | isPrefixOf "intrepid-" myBuildRelease -> [{-"upstart-compat-sysv", "belocs-locales-bin"-}]
              | isPrefixOf "hardy-" myBuildRelease -> [{-"upstart-compat-sysv", "belocs-locales-bin"-}]
              | True -> [{-"belocs-locales-bin"-}]
      _ -> error $ "Invalid build release: " ++ myBuildRelease

myExcludePackages _ = []

myComponents myBuildRelease =
    case releaseRepoName myBuildRelease of
      "debian" -> ["main", "contrib", "non-free"]
      "ubuntu" -> ["main", "restricted", "universe", "multiverse"]
      _ -> error $ "Invalid build release: " ++ myBuildRelease

-- | Unfortunately, we need to tell the autobuilder what version of ghc
-- is going to be in our release so that cabal-debian knows what packages
-- are bundled with the compiler.  If a compiler version is assigned here
-- it must be known to the cabal-debian library installed on the machine
-- on which the autobuilder is running.  If the result is Nothing it
-- assumes the same compiler is used in the build environment as in the
-- parent environment.
myCompilerVersion "quantal-seereason" = Just "7.6.1"
myCompilerVersion "natty-seereason" = Just "7.4.1"
myCompilerVersion "lucid-seereason" = Just "7.4.1"
myCompilerVersion _myBuildRelease = Nothing

myHackageServer = "hackage.haskell.org"
-- myHackageServer = "hackage.factisresearch.com"

------------------------- SOURCES --------------------------------

-- Return one of the elements in myReleaseSuffixes, or Nothing.
--
releaseSuffix :: String -> Maybe String
releaseSuffix release =
    case filter (`isSuffixOf` release) myReleaseSuffixes of
      [] -> Nothing
      [suffix] -> Just suffix
      suffixes -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show suffixes

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: String -> String -> String -> [String]
releaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case releaseSuffix release of
      Nothing -> baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost
      Just suff ->
          releaseSourceLines (dropSuffix suff release) debianMirrorHost ubuntuMirrorHost ++
          [ "deb " ++ uri ++ " " ++ release ++ " main"
          , "deb-src " ++ uri ++ " " ++ release ++ " main" ]
    where
      uri = show (fromJust (myBuildURI release))

baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case releaseRepoName release of
      "debian" -> debianSourceLines debianMirrorHost release
      "ubuntu" -> ubuntuSourceLines ubuntuMirrorHost release
      x -> error $ "Unknown release repository: " ++ show x

debianSourceLines debianMirrorHost release =
    [ "deb http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free"
    , "deb-src http://" ++ debianMirrorHost ++ "/debian " ++ release ++ " main contrib non-free" ]

ubuntuSourceLines ubuntuMirrorHost release =
    [ "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ " main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-updates main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-backports main restricted universe multiverse"
    , "deb http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse"
    , "deb-src http://" ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ release ++ "-security main restricted universe multiverse" ]

-- The names of the releases that we are able to create build environments for.
--
debianReleases = ["experimental", "sid", "wheezy", "squeeze", "lenny", "sarge"]
ubuntuReleases = ["raring", "quantal", "precise", "oneiric", "natty", "maverick", "lucid",
                  "karmic", "jaunty", "intrepid", "hardy", "feisty", "edgy", "dapper"]

oldDebianReleases = []
oldUbuntuReleases = []

-- A utility function
dropSuffix suff x = take (length x - length suff) x

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
--
mySources :: String -> String -> String -> [(String, String)]
mySources myBuildRelease debianMirrorHost ubuntuMirrorHost =
    List.map releaseSources
            (debianReleases ++ ubuntuReleases ++
             concatMap (derivedReleaseNames myBuildRelease) (debianReleases ++ ubuntuReleases)) ++
    [("debian-experimental", unlines (debianSourceLines debianMirrorHost "experimental")),
{-   ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])), -}
      ("kanotix",
       (unlines ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))]
    where
      releaseSources release = (release, unlines (releaseSourceLines release debianMirrorHost ubuntuMirrorHost))

-- Any package listed here will not trigger rebuilds when updated.
--
myGlobalRelaxInfo =
    ["base-files",
     "bash",
     "bsdutils",
     "cdbs",
     "devscripts",
     "dpkg",
     "dpkg-dev",
     "gcc",
     "g++",
     "make",
     "mount",
     "base-passwd",
     "mktemp",
     "sed",
     "util-linux",
     "sysvinit-utils",
     "autoconf",
     "debhelper",
     "debianutils",
     "diff",
     "e2fsprogs",
     "findutils",
     "flex",
     "login",
     "coreutils",
     "grep",
     "gs",
     "gzip",
     "hostname",
     "intltool",
     "ncurses-base",
     "ncurses-bin",
     "perl",
     "perl-base",
     "tar",
     "sysvinit",
     "libc6-dev",
     "haskell-devscripts"]

----------------------- BUILD RELEASE ----------------------------

-- Given a release name, return the subdirectory of myUploadURI which
-- contains the repository.
--
releaseRepoName rname
    | elem rname (debianReleases ++ oldDebianReleases) = "debian"
    | elem rname (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | True = case filter (`isSuffixOf` rname) myReleaseSuffixes of
               [suffix] -> releaseRepoName (dropSuffix suffix rname)
               [] -> error $ "Release name has unknown suffix: " ++ show rname
               suffixes -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show suffixes
