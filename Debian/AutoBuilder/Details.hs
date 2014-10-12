-- THIS FILE IS WHERE YOU DO ALL THE CUSTOMIZATIONS REQUIRED FOR THE
-- REPOSTORIES YOU ARE BUILDING.  The Targets.hs file may also be
-- modified to reflect the sources for the packages you will be
-- building.  If you find yourself modifying other files I would like
-- to hear about it.

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details
    ( myParams
    ) where

import Data.List as List (map)
import Data.Maybe
import Data.Monoid (mappend)
-- import Data.Set as Set (Set, empty)
import Debian.AutoBuilder.Details.Distros (Release(..), BaseRelease(..), allReleases, baseReleaseString,
                                           releaseString, parseReleaseName, isPrivateRelease,
                                           baseRelease, baseReleaseDistro, Distro(..), distroString)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.DefaultParams (defaultParams)
import Debian.AutoBuilder.Types.Packages (Packages(NoPackage))
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Sources (DebSource, parseSourceLine)
import Debian.URI
import Debian.Version (parseDebianVersion)
import qualified Debian.AutoBuilder.Details.Targets as Targets
import Prelude hiding (map)
import System.FilePath ((</>))

myParams :: FilePath -> Release -> ParamRec
myParams home myBuildRelease =
    let myUploadURIPrefix = "ssh://upload@deb.seereason.com/srv"
        myBuildURIPrefix = "http://deb.seereason.com" in
    (\ params -> params {knownPackages = myKnownTargets home params}) $
    (defaultParams (releaseString myBuildRelease)
                   myUploadURIPrefix
                   myBuildURIPrefix
                   myDevelopmentReleaseNames)
    { vendorTag = myVendorTag
    , oldVendorTags = ["seereason"]
    , autobuilderEmail = "SeeReason Autobuilder <partners@seereason.com>"
    , releaseSuffixes = myReleaseSuffixes
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    , sources = mySources myBuildRelease myDebianMirrorHost myUbuntuMirrorHost
    , globalRelaxInfo = myGlobalRelaxInfo
    , includePackages = myIncludePackages myBuildRelease
    , optionalIncludePackages = myOptionalIncludePackages myBuildRelease
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
    , requiredVersion = [(parseDebianVersion ("6.64" :: String), Nothing)]
    , hackageServer = myHackageServer
    }

-- This section has all the definitions relating to the particular
-- suffixes we will use on our build releases.
--
myReleaseSuffixes = ["-seereason", "-private"]

-- Our private releases are always based on our public releases, not
-- directly on upstream releases.
--
derivedReleaseNames :: Release -> BaseRelease -> [String]
derivedReleaseNames myBuildRelease baseRelease' = map releaseString (derivedReleases myBuildRelease baseRelease')

derivedReleases :: Release -> BaseRelease -> [Release]
derivedReleases myBuildRelease baseRelease' =
    [ExtendedRelease (Release baseRelease') SeeReason] ++
    if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease (Release baseRelease') SeeReason)] else []

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI :: Release -> Maybe URI
myUploadURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateUploadURI else myPublicUploadURI)
    where
      myPrivateUploadURI = myPrivateURIPrefix </> "deb-private" </> distroString (releaseRepoName (baseRelease myBuildRelease))
      myPublicUploadURI = myPrivateURIPrefix </> "deb" </> distroString (releaseRepoName (baseRelease myBuildRelease))


-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
myBuildURI :: Release -> Maybe URI
myBuildURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateBuildURI else myPublicBuildURI)
    where
      myPrivateBuildURI = myPrivateURIPrefix </> "deb-private" </> distroString (releaseRepoName (baseRelease myBuildRelease))
      myPublicBuildURI = "http://deb.seereason.com/" ++ distroString (releaseRepoName (baseRelease myBuildRelease))

-- myUploadURIPrefix = "ssh://upload@deb.seereason.com/srv"
myPrivateURIPrefix = "ssh://upload@deb.seereason.com/srv"

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

--myDiscards :: Set.Set String
--myDiscards = Set.empty

-- The set of all known package targets.  The targets we will
-- actually build are chosen from these.  The myBuildRelease argument
-- comes from the autobuilder argument list.
--
myKnownTargets :: FilePath -> ParamRec -> P.Packages
myKnownTargets home params =
    if isPrivateRelease rel
    then Targets.private home rel
    else mappend (Targets.public home rel) (if testWithPrivate params then Targets.private home rel else NoPackage)
    where
      rel = parseReleaseName (buildRelease params)

-- These host names are used to construct the sources.list lines to
-- access the Debian and Ubuntu repositories.  The anl.gov values here
-- probably won't work outside the United States.
--
--myDebianMirrorHost = "http://mirror.anl.gov"
myDebianMirrorHost = "http://ftp.debian.org"
--myUbuntuMirrorHost = "http://mirror.anl.gov"
--myUbuntuMirrorHost = "http://mirror.calvin.edu/ubuntu" --  "mirror://mirrors.ubuntu.com/mirrors.txt" -- "http://us.archive.ubuntu.com/ubuntu"
--myUbuntuMirrorHost = "http://ubuntu.cs.utah.edu"
--myDebianMirrorHost = "http://mirrors.usc.edu/pub/linux/distributions"
--myUbuntuMirrorHost = "http://mirrors.usc.edu/pub/linux/distributions"
myUbuntuMirrorHost = "http://ubuntu.mirrors.tds.net/pub/ubuntu"

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
     ("squeeze", "bpo60+"),
     ("squeeze-seereason", "bpo60+"),
     ("squeeze-seereason-private", "bpo60+")] ++
    concatMap (\ rel -> List.map (\ der -> (der, baseReleaseString rel)) (derivedReleaseNames myBuildRelease rel)) allReleases

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
myIncludePackages :: Release -> [String]
myIncludePackages myBuildRelease =
    [ "debian-archive-keyring"
    , "build-essential"         -- This is required by autobuilder code that opens the essential-packages list
    , "pkg-config"              -- Some packages now depend on this package via new cabal options.
    , "debian-keyring"
    , "locales"
    -- , "perl-base"
    -- , "gnupg"
    -- , "dpkg"
    -- , "locales"
    -- , "language-pack-en"
    -- , "ghc6"
    -- , "ghc6-doc"
    -- , "ghc6-prof"
    -- , "makedev"
    ] ++
    -- Private releases generally have ssh URIs in their sources.list,
    -- I have observed that this solves the "ssh died unexpectedly"
    -- errors.
    (if isPrivateRelease myBuildRelease then ["ssh"] else []) ++
    case releaseRepoName (baseRelease myBuildRelease) of
      Debian -> []
      Ubuntu ->
          ["ubuntu-keyring"] ++
          case baseRelease myBuildRelease of
            Trusty -> []
            Saucy -> []
            Raring -> []
            Quantal -> []
            Precise -> []
            Oneiric -> []
            Natty -> []
            Maverick -> []
            Lucid -> []
            Karmic -> [{-"upstart"-}]
            Jaunty -> [{-"upstart-compat-sysv"-}]
            Intrepid -> [{-"upstart-compat-sysv", "belocs-locales-bin"-}]
            Hardy -> [{-"upstart-compat-sysv", "belocs-locales-bin"-}]
            _ -> [{-"belocs-locales-bin"-}]
      _ -> error $ "Invalid base distro: " ++ show myBuildRelease

-- This will not be available when a new release is created, so we
-- have to make due until it gets built and uploaded.
myOptionalIncludePackages _myBuildRelease =
    [ "seereason-keyring" ]

myExcludePackages _ = []

myComponents :: Release -> [String]
myComponents myBuildRelease =
    case releaseRepoName (baseRelease myBuildRelease) of
      Debian -> ["main", "contrib", "non-free"]
      Ubuntu -> ["main", "restricted", "universe", "multiverse"]
      _ -> error $ "Invalid base distro: " ++ show myBuildRelease

myHackageServer = "hackage.haskell.org"
-- myHackageServer = "hackage.factisresearch.com"

------------------------- SOURCES --------------------------------

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: Release -> String -> String -> [DebSource]
releaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case release of
      PrivateRelease r ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
          List.map parseSourceLine [ "deb " ++ uri ++ " " ++ releaseString release ++ " main"
                                   , "deb-src " ++ uri ++ " " ++ releaseString release ++ " main" ]
      ExtendedRelease r d ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
          List.map parseSourceLine [ "deb " ++ uri ++ " " ++ releaseString release ++ " main"
                                   , "deb-src " ++ uri ++ " " ++ releaseString release ++ " main" ]
      Release b -> baseReleaseSourceLines b debianMirrorHost ubuntuMirrorHost
    where
      uri = show (fromJust (myBuildURI release))

baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case releaseRepoName release of
      Debian -> debianSourceLines debianMirrorHost release
      Ubuntu -> ubuntuSourceLines ubuntuMirrorHost release
      x -> error $ "Unknown release repository: " ++ show x

debianSourceLines debianMirrorHost release =
    List.map parseSourceLine $
    [ "deb " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString release ++ " main contrib non-free"
    , "deb-src " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString release ++ " main contrib non-free" ]

ubuntuSourceLines ubuntuMirrorHost release =
    List.map parseSourceLine $
    [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse"
    , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse"
    , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
    , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
    , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
    , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
    , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse"
    , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse" ]

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
--
mySources :: Release -> String -> String -> [(String, [DebSource])]
mySources myBuildRelease debianMirrorHost ubuntuMirrorHost =
    List.map releaseSources
            (map Release baseReleases ++
             concatMap (derivedReleases myBuildRelease) baseReleases) ++
    [(baseReleaseString Experimental, debianSourceLines debianMirrorHost Experimental),
{-   ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])), -}
      (distroString Kanotix,
       (List.map parseSourceLine
                ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))]
    where
      baseReleases = filter (/= Experimental) allReleases
      releaseSources release =
          (releaseString release, releaseSourceLines release debianMirrorHost ubuntuMirrorHost)

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
     "python-minimal",
     "tar",
     "sysvinit",
     "libc6-dev",
     "haskell-devscripts"]

----------------------- BUILD RELEASE ----------------------------

-- Given a release name, return the subdirectory of myUploadURI which
-- contains the repository.
--
releaseRepoName = baseReleaseDistro
{-
releaseRepoName rname
    | elem rname (debianReleases ++ oldDebianReleases) = "debian"
    | elem rname (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | True = case filter (`isSuffixOf` rname) myReleaseSuffixes of
               [suffix] -> releaseRepoName (dropSuffix suffix rname)
               [] -> error $ "Release name has unknown suffix: " ++ show rname
               suffixes -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show suffixes
-}
