-- | The sources.list and everything required to build it:
--     1. Repo URIs
--     2. Release names
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Sources
    ( mySources
    , myUploadURI
    , myBuildURI
    , myReleaseAliases
    , releaseRepoName
    ) where

import Data.List as List (map)
import Data.Maybe
import Data.Set as Set (delete, fromList, member, Set, toAscList)
import Debian.Releases (Release(..), BaseRelease(..), allReleases, baseReleaseString,
                        releaseString, isPrivateRelease,
                        baseRelease, baseReleaseDistro, Distro(..), distroString)
import Debian.Sources (DebSource(..), SourceOption(..), SourceOp(..), parseSourceLine)
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

derivedReleases :: Release -> BaseRelease -> [Release]
derivedReleases myBuildRelease baseRelease' =
    [ExtendedRelease (Release baseRelease') SeeReason] ++
    if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease (Release baseRelease') SeeReason)] else []

-- Our private releases are always based on our public releases, not
-- directly on upstream releases.
--
derivedReleaseNames :: Release -> BaseRelease -> [String]
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
    concatMap (\ rel -> List.map (\ der -> (der, baseReleaseString rel)) (derivedReleaseNames myBuildRelease rel)) allReleases

----------------------- SOURCES ----------------------------

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
--
mySources :: Release -> [(String, [DebSource])]
mySources myBuildRelease =
    List.map releaseSources
            (map Release (toAscList baseReleases) ++
             concatMap (derivedReleases myBuildRelease) baseReleases) ++
    [(baseReleaseString Experimental, debianSourceLines myDebianMirrorHost Experimental),
{-   ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])), -}
      (distroString Kanotix,
       (List.map parseSourceLine
                ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))]
    where
      baseReleases = Set.delete Experimental allReleases
      releaseSources release =
          (releaseString release, releaseSourceLines release myDebianMirrorHost myUbuntuMirrorHost)

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: Release -> String -> String -> [DebSource]
releaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case release of
      PrivateRelease r ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
           (List.map (trustMe . parseSourceLine)
              [ "deb " ++ uri ++ " " ++ releaseString release ++ " main"
              , "deb-src " ++ uri ++ " " ++ releaseString release ++ " main" ])
      ExtendedRelease r _d ->
          releaseSourceLines r debianMirrorHost ubuntuMirrorHost ++
          List.map (trustMe . parseSourceLine)
                  [ "deb " ++ uri ++ " " ++ releaseString release ++ " main"
                  , "deb-src " ++ uri ++ " " ++ releaseString release ++ " main" ]
      Release b -> baseReleaseSourceLines b debianMirrorHost ubuntuMirrorHost ++
                   hvrSourceLines b

    where
      uri = show (fromJust (myBuildURI release))
      trustMe x = x {sourceOptions = [SourceOption "trusted" OpSet ["yes"]]}

baseReleaseSourceLines release debianMirrorHost ubuntuMirrorHost =
    case releaseRepoName release of
      Debian -> debianSourceLines debianMirrorHost release
      Ubuntu -> ubuntuSourceLines ubuntuMirrorHost release
      x -> error $ "Unknown release repository: " ++ show x

debianSourceLines :: String -> BaseRelease -> [DebSource]
debianSourceLines debianMirrorHost release =
    List.map parseSourceLine
      [ "deb " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString release ++ " main contrib non-free"
      , "deb-src " ++ debianMirrorHost ++ "/debian " ++ baseReleaseString release ++ " main contrib non-free" ]

ubuntuSourceLines :: String -> BaseRelease -> [DebSource]
ubuntuSourceLines ubuntuMirrorHost release =
    List.map parseSourceLine
      [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse"
      , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
      , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
      , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse" ]

hvrSourceLines :: BaseRelease -> [DebSource]
hvrSourceLines release | release `member` hvrReleases =
    List.map parseSourceLine $
    ["deb http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString release ++ " main",
     "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString release ++ " main"]
hvrSourceLines Jessie =
    List.map parseSourceLine ["deb http://downloads.haskell.org/debian jessie main"]
hvrSourceLines _ = []

hvrReleases :: Set BaseRelease
hvrReleases = Set.fromList [Precise, Trusty, Utopic, Vivid, Wily, Xenial, Yakkety]

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
