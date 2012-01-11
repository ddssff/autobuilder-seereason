-- THIS FILE IS WHERE YOU DO ALL THE CUSTOMIZATIONS REQUIRED FOR THE
-- REPOSTORIES YOU ARE BUILDING.  The Targets.hs file may also be
-- modified to reflect the sources for the packages you will be
-- building.  If you find yourself modifying other files I would like
-- to hear about it.

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Config 
    ( myBuildURI
    , myDebianMirrorHost
    , myDevelopmentReleaseNames
    , myDiscards
    , myDoNewDist
    , myDoSSHExport
    , myDoUpload
    , myIncludePackages
    , myExcludePackages
    , myComponents
    , myCompilerVersion
    , myFlushPool
    , myForceBuild
    , myBuildTrumped
    , myGlobalRelaxInfo
    , myGoals
    , myReleaseAliases
    , myReleaseSuffixes
    , mySources
    , myTargets
    , myUbuntuMirrorHost
    , myUploadURI
    , myVendorTag
    , myVerbosity
    ) where

-- Import the symbols we use below.
import Data.List (isSuffixOf, isPrefixOf)
import Data.Maybe
import Data.Monoid (mappend)
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Params as P
import Debian.URI

import qualified Targets

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
myDevelopmentReleaseNames = ["sid", "oneiric"]

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
myTargets :: FilePath -> (P.Packages -> Bool) -> String -> P.Packages
myTargets home p myBuildRelease =
    filterPackages p $
           if isPrivateRelease myBuildRelease
           then Targets.private home
           else Targets.public home myBuildRelease

filterPackages :: (P.Packages -> Bool) -> P.Packages -> P.Packages
filterPackages p xs = P.foldPackages (\ name spec flags xs' -> if p (P.Package name spec flags) then mappend (P.Package name spec flags) xs' else xs') P.NoPackage xs

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
myDebianMirrorHost = "mirror.anl.gov"
myUbuntuMirrorHost = "mirror.anl.gov"
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
    concatMap (\ rel -> map (\ der -> (der, rel)) (derivedReleaseNames myBuildRelease rel)) ubuntuReleases

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
            _ | isPrefixOf "precise-" myBuildRelease -> []
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
-- are bundled with the compiler.  We could perhaps do some chroot thing
-- to compute this.
--myCompilerVersion "natty-seereason" = Just "7.2.1"
myCompilerVersion "natty-seereason" = Just "7.4.0.20111219"
myCompilerVersion "lucid-seereason" = Just "7.0.4"
myCompilerVersion _myBuildRelease = Nothing

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
ubuntuReleases = ["precise", "oneiric", "natty", "maverick", "lucid", "karmic", "jaunty", "intrepid", "hardy", "edgy", "feisty", "dapper"]

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
    map releaseSources
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
     "libc6-dev"]

----------------------- BUILD RELEASE ----------------------------

-- Given a release name, return the subdirectory of myUploadURI which
-- contains the repository.
--
releaseRepoName name
    | elem name (debianReleases ++ oldDebianReleases) = "debian"
    | elem name (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | True = case filter (`isSuffixOf` name) myReleaseSuffixes of
               [suffix] -> releaseRepoName (dropSuffix suffix name)
               [] -> error $ "Release name has unknown suffix: " ++ show name
               suffixes -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show suffixes
