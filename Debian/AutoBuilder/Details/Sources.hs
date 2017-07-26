-- | The sources.list and everything required to build it:
--     1. Repo URIs
--     2. Release names
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Sources
    ( mySources
    , myUploadURI
    , myBuildURI
    , myReleaseAliases
    -- , releaseRepoName
    ) where

import Data.List as List (map)
import Data.Maybe
import Data.Set as Set (delete, fromList, member, Set, toAscList)
import Debian.Release (ReleaseName(..))
import Debian.Releases (Vendor(..), Release(..), BaseRelease(..), allReleases, ubuntu,
                        releaseString, isPrivateRelease,
                        baseRelease, {-baseReleaseDistro,-} Distro(..), distroString)
import Debian.Sources (DebSource(..), parseSourceLine)
import Debian.AutoBuilder.Details.Common (MyDistro(..))
import Debian.URI
import Prelude hiding (map)
import System.FilePath ((</>))

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI :: Release MyDistro -> Maybe URI
myUploadURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateUploadURI else myPublicUploadURI)
    where
      myPrivateUploadURI = myPrivateURIPrefix myBuildRelease </> "deb-private" </> vendorString myBuildRelease
      myPublicUploadURI = myPrivateURIPrefix myBuildRelease </> (myPoolDir myBuildRelease) </> vendorString myBuildRelease

myPoolDir (PrivateRelease release) = myPoolURI release
myPoolDir (ExtendedRelease (Release (BaseRelease {_releaseName = (ReleaseName "xenial")})) SeeReason8) = "deb8"
myPoolDir _ = "deb"

myPoolURI (PrivateRelease release) = myPoolURI release
myPoolURI (ExtendedRelease (Release (BaseRelease {_releaseName = (ReleaseName "xenial")})) SeeReason8) = "http://deb8.seereason.com/"
myPoolURI _ = "http://deb.seereason.com/"

-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
myBuildURI :: Release MyDistro -> Maybe URI
myBuildURI myBuildRelease =
    parseURI (if isPrivateRelease myBuildRelease then myPrivateBuildURI else myPublicBuildURI)
    where
      myPrivateBuildURI = myPrivateURIPrefix myBuildRelease </> "deb-private" </> vendorString myBuildRelease
      myPublicBuildURI = myPoolURI myBuildRelease ++ vendorString myBuildRelease

vendorString :: Release MyDistro -> String
vendorString = _unVendor . _vendorName . baseRelease

-- myUploadURIPrefix = "ssh://upload@deb.seereason.com/srv"
myPrivateURIPrefix :: Release MyDistro -> String
myPrivateURIPrefix (PrivateRelease (ExtendedRelease (Release (BaseRelease {_releaseName = (ReleaseName "xenial")})) SeeReason8)) = "ssh://upload@deb8.seereason.com/srv"
myPrivateURIPrefix _ = "ssh://upload@deb.seereason.com/srv"

----------------------- BUILD RELEASE ----------------------------

-- Given a release name, return the subdirectory of myUploadURI which
-- contains the repository.
--
-- releaseRepoName = baseReleaseDistro
{-
releaseRepoName rname
    | elem rname (debianReleases ++ oldDebianReleases) = "debian"
    | elem rname (ubuntuReleases ++ oldUbuntuReleases) = "ubuntu"
    | True = case filter (`isSuffixOf` rname) myReleaseSuffixes of
               [suffix] -> releaseRepoName (dropSuffix suffix rname)
               [] -> error $ "Release name has unknown suffix: " ++ show rname
               suffixes -> error $ "Redundant suffixes in myReleaseSuffixes: " ++ show suffixes
-}

derivedReleases :: Release MyDistro -> BaseRelease -> [Release MyDistro]
derivedReleases myBuildRelease baseRelease' =
    [ExtendedRelease (Release baseRelease') SeeReason7] ++
    (if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease (Release baseRelease') SeeReason7)] else []) ++
    [ExtendedRelease (Release baseRelease') SeeReason8] ++
    (if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease (Release baseRelease') SeeReason8)] else [])

-- Our private releases are always based on our public releases, not
-- directly on upstream releases.
--
derivedReleaseNames :: Release MyDistro -> BaseRelease -> [String]
derivedReleaseNames myBuildRelease baseRelease' = map releaseString (derivedReleases myBuildRelease baseRelease')

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
    concatMap (\ rel -> List.map (\ der -> (der, relName (_releaseName rel))) (derivedReleaseNames myBuildRelease rel)) allReleases

----------------------- SOURCES ----------------------------

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
--
mySources :: Release MyDistro -> [(String, [DebSource])]
mySources myBuildRelease =
    List.map releaseSources
            (map Release (toAscList allReleases) ++
             concatMap (derivedReleases myBuildRelease) allReleases)
    where
      releaseSources release =
          (releaseString release, releaseSourceLines release myDebianMirrorHost myUbuntuMirrorHost)

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: Release MyDistro -> String -> String -> [DebSource]
releaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case release of
      PrivateRelease r ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
           (List.map parseSourceLine
              [ "deb [trusted=yes] " ++ uri ++ " " ++ releaseString release ++ " main"
              , "deb-src [trusted=yes] " ++ uri ++ " " ++ releaseString release ++ " main" ])
      ExtendedRelease r _d ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
          List.map parseSourceLine
                  [ "deb [trusted=yes] " ++ uri ++ " " ++ releaseString release ++ " main"
                  , "deb-src [trusted=yes] " ++ uri ++ " " ++ releaseString release ++ " main" ]
      Release b -> baseReleaseSourceLines b debianMirrorHost ubuntuMirrorHost ++
                   hvrSourceLines b

    where
      uri = show (fromJust (myBuildURI release))

baseReleaseSourceLines :: BaseRelease -> String -> String -> [DebSource]
baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case _vendorName release of
      Vendor "debian" -> debianSourceLines debianMirrorHost release
      Vendor "ubuntu" -> ubuntuSourceLines ubuntuMirrorHost release
      x -> error $ "Unknown release repository: " ++ show release

baseReleaseString :: Release MyDistro -> String
baseReleaseString = relName . _releaseName . baseRelease

baseReleaseString' :: BaseRelease -> String
baseReleaseString' = relName . _releaseName

debianSourceLines :: String -> BaseRelease -> [DebSource]
debianSourceLines debianMirrorHost release =
    List.map parseSourceLine
      [ "deb " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString' release ++ " main contrib non-free"
      , "deb-src " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString' release ++ " main contrib non-free" ]

ubuntuSourceLines :: String -> BaseRelease -> [DebSource]
ubuntuSourceLines ubuntuMirrorHost release =
    List.map parseSourceLine $
      [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ " main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ " main restricted universe multiverse" ] ++
      if _releaseName release == ReleaseName "artful"
      then []
      else [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-updates main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-updates main restricted universe multiverse"
           , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-backports main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-backports main restricted universe multiverse"
           , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-security main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString' release ++ "-security main restricted universe multiverse" ]

hvrSourceLines :: BaseRelease -> [DebSource]
hvrSourceLines release | release `member` hvrReleases =
    List.map parseSourceLine $
    ["deb http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString' release ++ " main",
     "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString' release ++ " main"]
hvrSourceLines (BaseRelease {_releaseName = ReleaseName "jessie"}) =
    List.map parseSourceLine ["deb http://downloads.haskell.org/debian jessie main"]
hvrSourceLines _ = []

hvrReleases :: Set BaseRelease
hvrReleases = Set.fromList [BaseRelease ubuntu (ReleaseName "precise"),
                            BaseRelease ubuntu (ReleaseName "trusty"),
                            BaseRelease ubuntu (ReleaseName "utopic"),
                            BaseRelease ubuntu (ReleaseName "vivid"),
                            BaseRelease ubuntu (ReleaseName "wily"),
                            BaseRelease ubuntu (ReleaseName "xenial"),
                            BaseRelease ubuntu (ReleaseName "yakkety")]

-- These host names are used to construct the sources.list lines to
-- access the Debian and Ubuntu repositories.  The anl.gov values here
-- probably won't work outside the United States.
--
--myDebianMirrorHost = "http://mirror.anl.gov"
myDebianMirrorHost = "http://ftp.debian.org"
--myUbuntuMirrorHost = "http://mirror.anl.gov"
--myUbuntuMirrorHost = "http://mirror.calvin.edu/ubuntu" --  "mirror://mirrors.ubuntu.com/mirrors.txt" -- "http://us.archive.ubuntu.com/ubuntu"
--myDebianMirrorHost = "http://mirrors.usc.edu/pub/linux/distributions"
--myUbuntuMirrorHost = "http://mirror.umd.edu" -- sources line looks like: deb http://mirror.umd.edu/ubuntu/ trusty main restricted universe multiverse
--myUbuntuMirrorHost = "http://ubuntu.mirrors.tds.net/pub/ubuntu"
--myUbuntuMirrorHost = "http://ubuntu.cs.utah.edu"  -- Very slow!
myUbuntuMirrorHost = "http://archive.ubuntu.com/ubuntu"
--myUbuntuMirrorHost = "http://mirror.picosecond.org/ubuntu"
