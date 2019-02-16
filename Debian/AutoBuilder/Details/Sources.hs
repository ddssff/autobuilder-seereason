-- | The sources.list and everything required to build it:
--     1. Repo URIs
--     2. Release names
{-# LANGUAGE CPP, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Sources
    ( mySources
    , myUploadURI'
    , myVendorURI
    , myReleaseURI
    , myDownloadURI
    , releaseURI
    , myReleaseAliases
    , releaseFileURI
    -- , releaseRepoName
    , tests
    ) where

import Control.Lens (over, review, view)
import Control.Monad.Except (MonadError, throwError)
import Data.Foldable
import Data.List as List (map)
import Data.Set as Set (fromList, member, Set, toAscList)
import Debian.Releases
    (allReleases, baseVendor, baseVendorString, DebianRelease(Jessie, Experimental),
     ExtendedRelease(..), isPrivateRelease, HasBaseRelease(baseReleaseString), ReleaseTree(..),
     releaseString, UbuntuRelease(..), ReleaseURI, releaseURI)
import Debian.Repo.DebError (DebError)
import Debian.Sources (DebSource(..), parseSourceLine)
import Debian.TH (here, Loc)
import Debian.URI (HasURIError, parseURIReference', URI, uriPathLens)
import Debian.VendorURI (VendorURI, vendorURI)
import Distribution.Pretty (prettyShow)
import Extra.Except
import Prelude hiding (map)
import System.FilePath ((</>))
import Test.HUnit

tests :: Test
tests =
    TestList
    [ let rel = (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason84)) in
      TestCase (assertEqual ("uriPrefix " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb-private")
                  (fmap show (uriPrefix [$here] rel :: Either DebError URI)))

    , let rel = (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason84)) in
      TestCase (assertEqual ("myUploadURI' " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb-private/ubuntu/dists/bionic-seereason-private")
                  (fmap (show . view releaseURI) $ (myUploadURI' [$here] rel :: Either DebError ReleaseURI)))
    , let rel = (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason86)) in
      TestCase (assertEqual ("myUploadURI' " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb86-private/ubuntu/dists/bionic-seereason-private")
                  (fmap (show . view releaseURI) $ (myUploadURI' [$here] rel :: Either DebError ReleaseURI)))
    , let rel = (ExtendedRelease (UbuntuRelease Bionic) SeeReason84) in
      TestCase (assertEqual ("myUploadURI' " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb/ubuntu/dists/bionic-seereason")
                  (fmap (show . view releaseURI) $ (myUploadURI' [$here] rel :: Either DebError ReleaseURI)))
    , let rel = (ExtendedRelease (UbuntuRelease Bionic) SeeReason86) in
      TestCase (assertEqual ("myUploadURI' " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb86/ubuntu/dists/bionic-seereason")
                  (fmap (show . view releaseURI) $ (myUploadURI' [$here] rel :: Either DebError ReleaseURI)))
#if 0
    , let rel = (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason84)) in
      TestCase (assertEqual ("myDownloadURI " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb-private/ubuntu")
                  (fmap show $ myDownloadURI rel))
    , let rel = (PrivateRelease (ExtendedRelease (UbuntuRelease Bionic) SeeReason86)) in
      TestCase (assertEqual ("myDownloadURI " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb86-private/ubuntu")
                  (fmap show $ myDownloadURI rel))
    , let rel = (ExtendedRelease (UbuntuRelease Bionic) SeeReason84) in
      TestCase (assertEqual ("myDownloadURI " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb/ubuntu")
                  (fmap show $ myDownloadURI rel))
    , let rel = (ExtendedRelease (UbuntuRelease Bionic) SeeReason86) in
      TestCase (assertEqual ("myDownloadURI " ++ show rel)
                  (Right "ssh://upload@deb8.seereason.com/srv/deb86/ubuntu")
                  (fmap show $ myDownloadURI rel))
#endif
    ]

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI' :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => [Loc] -> ReleaseTree -> m ReleaseURI
myUploadURI' = myReleaseURI

myVendorURI :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => [Loc] -> ReleaseTree -> m VendorURI
myVendorURI locs r = (review vendorURI . over uriPathLens (\p -> p </> review baseVendorString (baseVendor r))) <$> uriPrefix ($here : locs) r

myReleaseURI :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => [Loc] -> ReleaseTree -> m ReleaseURI
myReleaseURI locs r = (review releaseURI . over uriPathLens (\p -> p </> review baseVendorString (baseVendor r) </> "dists" </> releaseString r)) <$> uriPrefix ($here : locs) r

-- | The download uri may have scheme http in addition to those of
-- upload uri.  But right now there's no http server running on
-- deb8.seereason.com.
myDownloadURI :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => [Loc] -> ReleaseTree -> m ReleaseURI
myDownloadURI = myReleaseURI

releaseFileURI :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => ReleaseTree -> m URI
releaseFileURI r = do
  uri <- myDownloadURI [$here] r
  let uri' = over (releaseURI . uriPathLens) (</> "Release") uri
  return (view releaseURI uri')

#if 0
myPoolDir :: MonadError URIError m => ReleaseTree -> m URI
myPoolDir = parseRelativeReference' . myPoolDir'
    where
      myPoolDir' (PrivateRelease release) = myPoolDir' release ++ "-private"
      myPoolDir' (ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) distro) | distro == SeeReason86 = "deb86"
      myPoolDir' (ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) distro) | distro == SeeReason84 = "deb"
      myPoolDir' rel@(ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) _distro) = error ("myPoolDir - unexpected distro: " ++ show rel)
      myPoolDir' rel = error $ "unexpected: myPoolDir " ++ show rel

-- | URI used to download packages.
myPoolURI  :: MonadError URIError m => ReleaseTree -> m URI
myPoolURI = myUploadURI
myPoolURI (PrivateRelease release) = myPoolURI release
myPoolURI (ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) distro) | distro == SeeReason86 = parseURI' "ssh://upload@deb8.seereason.com/srv/deb86/"
myPoolURI (ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) distro) | distro == SeeReason84 = parseURI' "ssh://upload@deb8.seereason.com/srv/deb/"
-- No http server running on genie right now
--myPoolURI _ = "http://deb8.seereason.com/"
myPoolURI rel@(ExtendedRelease (Foundation (BaseRelease {_releaseName = (ReleaseName _)})) _distro) = error ("Debian.AutoBuilder.Details.Sources.myPoolURI - unexpected distro: " ++ show rel)
myPoolURI rel = error ("myPoolURI - unexpected: " ++ show rel)
#endif

#if 0
-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
myBuildURI :: forall m. MonadError URIError m => ReleaseTree -> m URI
myBuildURI myBuildRelease = myUploadURI' myBuildRelease
    appendURIs =<<
      (sequence
         [(if isPrivateRelease myBuildRelease then myPrivateBuildURI else myPublicBuildURI),
          myPoolURI myBuildRelease,
          parseRelativeReference' (releaseURI myBuildRelease)])
    where
      myPrivateBuildURI :: m URI
      myPrivateBuildURI = appendURIs =<< (sequence [uriPrefix myBuildRelease, myPoolDir myBuildRelease, parseRelativeReference' (vendorString myBuildRelease)])
      myPublicBuildURI :: m URI
      myPublicBuildURI = appendURIs =<< (sequence [myPoolURI myBuildRelease, parseRelativeReference' (vendorString myBuildRelease)])
#endif
{-
    parseURI =<< (if isPrivateRelease myBuildRelease then myPrivateBuildURI else myPublicBuildURI)
    where
      t1 x = trace ("Debian.AutoBuilder.Details.Sources.myBuildURI " ++ show ss ++ " " ++ show myBuildRelease ++ " -> " ++ show x) x
-}

-- vendorString :: ReleaseTree -> String
-- vendorString = _unVendor . _vendorName . baseRelease

-- | URI of the directory containing the base vendor directories (typically debian and ubuntu.)
uriPrefix :: (MonadError e m, HasLoc e, HasURIError e, HasIOException e) => [Loc] -> ReleaseTree -> m URI
uriPrefix locs (PrivateRelease rel) = over uriPathLens (++ "-private") <$> uriPrefix ($here : locs) rel
uriPrefix _ (ExtendedRelease (UbuntuRelease Bionic) distro) | distro == SeeReason86 = parseURIReference' "ssh://upload@deb8.seereason.com/srv/deb86"
uriPrefix _ (ExtendedRelease (UbuntuRelease Bionic) distro) | distro == SeeReason84 = parseURIReference' "ssh://upload@deb8.seereason.com/srv/deb"
uriPrefix locs r = withError (withLoc $here) $ throwError $ fromIOException $ userError ("uriPrefix " <> prettyShow ($here : locs) <>  "- no URI for release " <> show r)

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

derivedReleases :: ReleaseTree -> ReleaseTree -> [ReleaseTree]
derivedReleases myBuildRelease baseRelease' =
    [ExtendedRelease baseRelease' SeeReason84] ++
    (if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease baseRelease' SeeReason84)] else []) ++
    [ExtendedRelease baseRelease' SeeReason86] ++
    (if isPrivateRelease myBuildRelease then [PrivateRelease (ExtendedRelease baseRelease' SeeReason86)] else [])

-- Our private releases are always based on our public releases, not
-- directly on upstream releases.
--
derivedReleaseNames :: ReleaseTree -> ReleaseTree -> [String]
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
    concatMap
      (\ rel -> List.map (\ der -> (der, baseReleaseString rel)) (derivedReleaseNames myBuildRelease rel))
      allReleases

----------------------- SOURCES ----------------------------

-- Build a map assigning names to text for every sources.list we might
-- use.  These names can be used in Apt targets.  It is also assumed
-- that we can use any base release or build release name to look up a
-- sources.list.
--
mySources :: [String] -> ReleaseTree -> [(String, [DebSource])]
mySources ss myBuildRelease =
    List.map releaseSources
            (toAscList allReleases <>
             concatMap (derivedReleases myBuildRelease) allReleases)
    where
      releaseSources release =
          (releaseString release, releaseSourceLines (("Debian.AutoBuilder.Details.Sources.mySources myBuildRelease=" ++ show myBuildRelease ++ " release=" ++ show release) : ss) release myDebianMirrorHost myUbuntuMirrorHost)

-- Build a sources.list for one of our build relases.
--
releaseSourceLines :: [String] -> ReleaseTree -> String -> String -> [DebSource]
releaseSourceLines ss release debianMirrorHost ubuntuMirrorHost =
    case release of
      PrivateRelease r ->
        releaseSourceLines (("Debian.AutoBuilder.Details.Sources.releaseSourceLines release=" ++ show release) : ss) r debianMirrorHost ubuntuMirrorHost ++
          case myVendorURI [$here] release of
            Left (_ :: DebError) -> []
            Right uri ->
              List.map (parseSourceLine [$here])
                  [ "deb [trusted=yes] " ++ show (view vendorURI uri) ++ " " ++ releaseString release ++ " main"
                  , "deb-src [trusted=yes] " ++ show (view vendorURI uri) ++ " " ++ releaseString release ++ " main" ]
      ExtendedRelease r _d ->
          releaseSourceLines (("Debian.AutoBuilder.Details.Sources.releaseSourceLines release=" ++ show release) : ss) r debianMirrorHost ubuntuMirrorHost ++
          case myVendorURI [$here] release of
            Left (_ :: DebError) -> []
            Right uri ->
              List.map (parseSourceLine [$here])
                  [ "deb [trusted=yes] " ++ show (view vendorURI uri) ++ " " ++ releaseString release ++ " main"
                  , "deb-src [trusted=yes] " ++ show (view vendorURI uri) ++ " " ++ releaseString release ++ " main" ]
      b@(DebianRelease Experimental) -> debianSourceLines ["[trusted=yes]"] debianMirrorHost Experimental ++ hvrSourceLines b
      b@(DebianRelease release') -> debianSourceLines [] debianMirrorHost release' ++ hvrSourceLines b
      b@(UbuntuRelease release') -> ubuntuSourceLines ubuntuMirrorHost release' ++ hvrSourceLines b

debianSourceLines :: [String] -> String -> DebianRelease -> [DebSource]
debianSourceLines trusted debianMirrorHost release =
    List.map (parseSourceLine [$here])
      [ unwords (["deb"] ++ trusted ++ [debianMirrorHost ++ "/debian", baseReleaseString release, "main", "contrib", "non-free"])
      , unwords (["deb-src"] ++ trusted ++ [debianMirrorHost ++ "/debian", baseReleaseString release, "main", "contrib", "non-free" ]) ]

ubuntuSourceLines :: String -> UbuntuRelease -> [DebSource]
ubuntuSourceLines ubuntuMirrorHost release =
    List.map (parseSourceLine [$here]) $
      [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse"
      , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ " main restricted universe multiverse" ] ++
      if baseReleaseString release == "artful"
      then []
      else [ "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-updates main restricted universe multiverse"
           , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-backports main restricted universe multiverse"
           , "deb " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse"
           , "deb-src " ++ ubuntuMirrorHost ++ "/ubuntu/ " ++ baseReleaseString release ++ "-security main restricted universe multiverse" ]

hvrSourceLines :: ReleaseTree -> [DebSource]
hvrSourceLines release | release `member` hvrReleases =
    List.map (parseSourceLine [$here]) $
    ["deb http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString release ++ " main",
     "deb-src http://ppa.launchpad.net/hvr/ghc/ubuntu " ++ baseReleaseString release ++ " main"]
hvrSourceLines (DebianRelease Jessie) =
    List.map (parseSourceLine [$here]) ["deb http://downloads.haskell.org/debian jessie main"]
hvrSourceLines _ = []

hvrReleases :: Set ReleaseTree
hvrReleases = Set.fromList [UbuntuRelease Precise,
                            UbuntuRelease Trusty,
                            UbuntuRelease Utopic,
                            UbuntuRelease Vivid,
                            UbuntuRelease Wily,
                            UbuntuRelease Xenial,
                            UbuntuRelease Yakkety]

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
myUbuntuMirrorHost = "http://archive.ubuntu.com"
--myUbuntuMirrorHost = "http://mirror.picosecond.org/ubuntu"
