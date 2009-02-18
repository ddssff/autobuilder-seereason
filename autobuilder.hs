#!/usr/bin/env runhaskell -package=base-3.0.3.0
-- Currently this will not run as a script even with the line above.
-- The reason is unclear.  Either use the wrapper script in
-- /usr/bin/autobuilder or run
--   sudo runhaskell -package=base-3.0.3.0 <path to this configuration file> release1 release2 ...

-- Import the symbols we use below.
import Data.List (isSuffixOf)
import Data.Maybe
import qualified Debian.AutoBuilder.Main as M
import qualified Debian.AutoBuilder.ParamClass as P
import Debian.AutoBuilder.ParamClass (Target(..))
import Debian.AutoBuilder.ParamRec
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.Repo.Types (ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.URI
import Debian.Version (parseDebianVersion)
import System.Environment (getArgs)
import System.IO (hPutStrLn, hFlush, stderr)

-- The known suffixes for build releases.
myReleaseSuffixes = ["-seereason", "-private"]

-- The current list of development releases.  The version numbers for
-- these releases do not need to be tagged with the base release name,
-- only with the vendor tag.  Sid is always a development release,
-- Ubuntu creates a new one for each cycle.
--
myDevelopmentReleaseNames = ["sid", "jaunty"]

-- This tag is used to construct the customized part of the version
-- number for any package the autobuilder builds.
--
myVendorTag = "seereason"

-- Put the names of any source packages you wish to rebuild whether or
-- not they appear to need it.  If you modified the package source but
-- did not modify the version number in the changelog this will force
-- a build.  This can lead to problems if you build the package for
-- multiple release or multiple architectures - you can end up with
-- different source for seemingly identical uploaded versions.
--
myForceBuild = []

-- Make the output more or less chatty.  Zero is normal, -1 is
-- quieter, and so on.
--
myVerbosity = 0

-- The list of packages we want to build.  The myBuildRelease argument
-- comes from the autobuilder argument list.
--
myTargets myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> myPublicTargets
      True -> myPrivateTargets

-- The list of targets which are uploaded to the public repository
--
myPublicTargets = ghc610CoreTargets ++ autobuilderTargets ++ ghc610Targets ++ otherTargets

-- The list of targets which are uploaded to the private repository
--
myPrivateTargets = privateTargets

-- If you are not interested in building everything, put one or more
-- source package names you want to build in this list.  Only these
-- packages and their build dependencies will be considered for
-- building.
--
myGoals = []

-- Helper function to identify private releases.
--
isPrivateRelease release = isSuffixOf "-private" release

-- Use this to find one or more targets by Debian source package name,
-- for use as the value of myTargets.
--
findTargets :: [String] -> [Target] -> [Target]
findTargets names targets = filter (\ target -> elem (sourcePackageName target) names) targets

-- These host names are used to construct the sources.list lines to
-- access the Debian and Ubuntu repositories.  The anl.gov values here
-- probably won't work outside the United States.
--
myDebianMirrorHost = "mirror.anl.gov"
myUbuntuMirrorHost = "mirror.anl.gov"
--myDebianMirrorHost = "mirrors.usc.edu"
--myUbuntuMirrorHost = "mirrors.usc.edu"

-- If true, upload the packages after a successful build
--
myDoUpload = True

-- If true, run newdist on the upload repository after a successful
-- build and upload, making them available to apt-get install.
--
myDoNewDist = True

-- If true, try to set up ssh access to the upload host if necessary.
--
myDoSSHExport = True

-- There is a debian standard for constructing the version numbers of
-- packages backported to older releases.  To follow this standard we
-- use bpo40+ for Debian 4.0, aka etch.
--
myReleaseAliases =
    [("etch", "bpo40+"),
     ("lenny", "bpo50+"),
     ("hardy-seereason", "hardy"),
     ("intrepid-seereason", "intrepid"),
     ("jaunty-seereason", "jaunty")]

-- This URI is the address of the remote repository to which packages
-- will be uploaded after a run with no failures, when the myDoUpload
-- flag is true.  Packages are uploaded to the directory created by
-- appending '/incoming' to this URI.  This is distinct from the
-- local repository, where each packages is uploaded immediately after
-- it is built for use as build dependencies of other packages during
-- the same run.
--
myUploadURI myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> parseURI $ myPublicUploadURI myBuildRelease
      True -> parseURI $ myPrivateUploadURI myBuildRelease

-- An alternate url for the same repository the upload-uri points to,
-- used for downloading packages that have already been installed
-- there.
--
myBuildURI myBuildRelease =
    case isPrivateRelease myBuildRelease of
      False -> parseURI $ myPublicBuildURI myBuildRelease
      True -> parseURI $ myPrivateBuildURI myBuildRelease

myPrivateUploadURI release = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName release
myPrivateBuildURI release = "ssh://upload@deb.seereason.com/srv/deb-private/" ++ releaseRepoName release

myPublicUploadURI release = "ssh://upload@deb.seereason.com/srv/deb/" ++ releaseRepoName release
myPublicBuildURI release = "http://deb.seereason.com/" ++ releaseRepoName release

-- Additional packages to include in the clean build environment.
-- Adding packages here can speed things up when you are building many
-- packages, because for each package it reverts the build environment
-- to the clean environment and then installs all the build
-- dependencies.  This only affects newly created environments, so if
-- you change this value use the flushRoot option to get it to take
-- effect.
--
myExtraPackages myBuildRelease =
    ["debian-archive-keyring", "seereason-keyring"] ++
    -- Private releases generally have ssh URIs in their sources.list,
    -- I have observed that this solves the "ssh died unexpectedly"
    -- errors.
    (if isPrivateRelease myBuildRelease then ["ssh"] else []) ++
    case releaseRepoName myBuildRelease of
      "debian" -> []
      "ubuntu" -> ["ubuntu-keyring"]
      _ -> error $ "Invalid build release: " ++ myBuildRelease

-- Specify extra packages to include as essential in the build
-- environment.  This option was provided to add either upstart or
-- sysvinit to the build when they ceased to be 'Required' packages.
--
myExtraEssential myBuildRelease =
    ["belocs-locales-bin", "gnupg", "dpkg"] ++
    case releaseRepoName myBuildRelease of
      "debian" -> []
      "ubuntu" -> ["upstart-compat-sysv"]
      _ -> error $ "Unknown build release: " ++ myBuildRelease

------------------------- SOURCES --------------------------------

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

debianReleases = ["sid", "lenny"]
ubuntuReleases = ["jaunty", "intrepid", "hardy"]

dropSuffix suff x = take (length x - length suff) x

allSources debianMirrorHost ubuntuMirrorHost includePrivate =
    map releaseSources
            (debianReleases ++ ubuntuReleases ++
             map (++ "-seereason") (debianReleases ++ ubuntuReleases) ++
             if includePrivate then map (++ "-seereason-private") (debianReleases ++ ubuntuReleases) else []) ++
    [("debian-experimental", unlines (debianSourceLines debianMirrorHost "experimental")),
     ("debian-multimedia",
      (unlines ["deb http://mirror.home-dn.net/debian-multimedia stable main",
                "deb-src http://mirror.home-dn.net/debian-multimedia stable main"])),
      ("kanotix",
       (unlines ["deb http://kanotix.com/files/debian sid main contrib non-free vdr",
                 "  deb-src http://kanotix.com/files/debian sid main contrib non-free vdr"]))]
    where
      releaseSources release = (release, unlines (releaseSourceLines release))
      releaseSourceLines :: String -> [String]
      releaseSourceLines release =
          case () of
            _ | isSuffixOf "-private" release -> privateSourceLines release
              | isSuffixOf "-seereason" release -> seereasonSourceLines release
              | True -> baseReleaseSourceLines release
      baseReleaseSourceLines release =
          case releaseRepoName release of
            "debian" -> debianSourceLines debianMirrorHost release
            "ubuntu" -> ubuntuSourceLines ubuntuMirrorHost release
            x -> error $ "Unknown release repository: " ++ show x
      seereasonSourceLines release =
          releaseSourceLines (dropSuffix "-seereason" release) ++
                                 [ "deb " ++ myPublicBuildURI release ++ " " ++ release ++ " main"
                                 , "deb-src " ++ myPublicBuildURI release ++ " " ++ release ++ " main" ]
      privateSourceLines release =
          releaseSourceLines (dropSuffix "-private" release) ++
                                 [ "deb " ++ myPrivateUploadURI release ++ " " ++ release ++ " main"
                                 , "deb-src " ++ myPrivateUploadURI release ++ " " ++ release ++ " main" ]

-- Any package listed here will not trigger rebuilds when updated.
--
myGlobalRelaxInfo =
    ["base-files",
     "bash",
     "bsdutils",
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
    | isSuffixOf "-seereason" name = releaseRepoName (dropSuffix "-seereason" name)
    | isSuffixOf "-private" name = releaseRepoName (dropSuffix "-private" name)
    | True = error $ "Unknown release name: " ++ show name

-- These are releases which are not unsupported for building, but from
-- which we could, if we had to, pull source from using an Apt target.
--
oldDebianReleases = ["etch", "sarge"]
oldUbuntuReleases = ["gutsy", "feisty"]

------------------------ TARGETS ---------------------

-- Information about how to obtain and assemble the source code for
-- the packages we want to build. | 

ghc610CoreTargets =
    [ Target { sourcePackageName = "haskell-bzlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/ghc610/debian/haskell-bzlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zlib/0.5.0.0/zlib-0.5.0.0.tar.gz:22fa6d394c42c8584b234799b923f860):(darcs:http://src.seereason.com/ghc610/debian/haskell-zlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cdbs"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-cdbs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cpphs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cpphs/1.6/cpphs-1.6.tar.gz:8a7565ff3b2d7bdb594af4c10c594951):(darcs:http://src.seereason.com/ghc610/debian/cpphs-debian)"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haxml"
             , sourceSpec = "quilt:(apt:sid:haxml):(darcs:http://src.seereason.com/ghc610/quilt/haxml-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-extra"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-extra"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-debian"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-debian-3"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-debian-repo"
             , relaxInfo = [] }
    , Target { sourcePackageName = "ghc6"
             , sourceSpec = "deb-dir:(uri:http://www.haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src.tar.bz2:54c676a632b3d73cf526b06347522c32):(darcs:http://src.seereason.com/ghc610/debian/ghc610-debian)"
             , relaxInfo = ["ghc6"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"haddock"] }
    , Target { sourcePackageName = "haskell-devscripts"
             , sourceSpec = "quilt:(uri:http://ftp.de.debian.org/debian/pool/main/h/haskell-devscripts/haskell-devscripts_0.6.15.tar.gz:996acac2c6fb2da2be9c5016f93a3c67):(darcs:http://src.seereason.com/ghc610/quilt/haskell-devscripts-quilt)"
             , relaxInfo = [] }
    ]

autobuilderTargets =
    [ Target { sourcePackageName = "build-env"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/build-env"
             , relaxInfo = [] }
    , Target { sourcePackageName = "autobuilder"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/autobuilder"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cgi"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cgi/3001.1.7.1/cgi-3001.1.7.1.tar.gz:02b1d2fe6f271a17c1eb8b897fbd1d7f):(darcs:http://src.seereason.com/ghc610/debian/haskell-cgi-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mime"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-mime"
             , relaxInfo = [] }
    , Target { sourcePackageName = "magic-haskell"
             , sourceSpec = "quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/ghc610/quilt/magic-haskell-quilt)"
             , relaxInfo = [] }
    ]

ghc610Targets =
    [ Target { sourcePackageName = "haskell-utils"
             , sourceSpec = "quilt:(apt:sid:haskell-utils):(darcs:http://src.seereason.com/ghc610/quilt/haskell-utils-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.3/applicative-extras-0.1.3.tar.gz:50fa4c61e89654ea9858c304b4682680):(darcs:http://src.seereason.com/ghc610/debian/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-binary"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/binary/0.4.4/binary-0.4.4.tar.gz:48fc6454e82e0aec7f648be107bfc0b8):(darcs:http://src.seereason.com/ghc610/debian/binary-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-extensible-exceptions"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/extensible-exceptions/0.1.1.0/extensible-exceptions-0.1.1.0.tar.gz:7aba82acc64fa2f2dc89d8ac27e24a43):(darcs:http://src.seereason.com/ghc610/debian/extensible-exceptions-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-util"
             , sourceSpec = "cd:happstack-util:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             , sourceSpec = "cd:happstack-data:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-ixset"
             , sourceSpec = "cd:happstack-ixset:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-server"
             , sourceSpec = "cd:happstack-server:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-state"
             , sourceSpec = "cd:happstack-state:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-contrib"
             , sourceSpec = "cd:happstack-contrib:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/ghc610/debian/hinotify-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hspread"
             , sourceSpec = "quilt:(apt:sid:haskell-hspread):(darcs:http://src.seereason.com/ghc610/quilt/haskell-hspread-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-utf8-string"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utf8-string/0.3.4/utf8-string-0.3.4.tar.gz:72d13d9453cdf721dd95bc18144a120a):(darcs:http://src.seereason.com/ghc610/debian/utf8-string-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happy/1.18.2/happy-1.18.2.tar.gz:adb1679a1fa8cec74a6e621a4a277e98):(darcs:http://src.seereason.com/ghc610/debian/happy-debian)"
             , relaxInfo = ["happy"] }
    , Target { sourcePackageName = "haskell-src-exts"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/haskell-src-exts/0.4.3.1/haskell-src-exts-0.4.3.1.tar.gz:4ff97fdae2bca0da0194fcb80974b188):(darcs:http://src.seereason.com/ghc610/debian/haskell-src-exts-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/ghc610/debian/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/iconv"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hslogger"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc610/debian/hslogger-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HTTP/4000.0.4/HTTP-4000.0.4.tar.gz:6526c1ee59cd3aedc7aa380673c80ef1):(darcs:http://src.seereason.com/ghc610/debian/haskell-http-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/syb-with-class"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-util"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Util"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-data"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Data"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-ixset"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-IxSet"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-state"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-State"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-server"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Server"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-harp"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/harp):(darcs:http://src.seereason.com/ghc610/debian/harp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjavascript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjavascript):(darcs:http://src.seereason.com/ghc610/debian/hjavascript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/hsx"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsp"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/ghc610/hsp):(darcs:http://src.seereason.com/ghc610/debian/hsp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/happs-hsp-formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hsx-xhtml):(darcs:http://src.seereason.com/ghc610/debian/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjscript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjscript):(darcs:http://src.seereason.com/ghc610/debian/hjscript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happs-extra"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/ghc610/debian/shellac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-frisby"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/frisby"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cabal-install"
             , sourceSpec = "deb-dir:(darcs:http://darcs.haskell.org/cabal-install):(darcs:http://src.seereason.com/ghc610/debian/cabal-install-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uniplate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/uniplate/1.2.0.3/uniplate-1.2.0.3.tar.gz:e0e10700870f5b9756d4097e640164ca):(darcs:http://src.seereason.com/ghc610/debian/uniplate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/ghc610/debian/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/ghc610/debian/stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/ghc610/debian/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delconto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/ghc610/debian/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = "darcs:http://src.seereason.com/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = "darcs:http://src.seereason.com/archive"
             , relaxInfo = [] }

{-
  "quilt:(apt:sid:hs-plugins):(darcs:http://src.seereason.com/ghc610/quilt/hs-plugins-quilt)"
    - Needs an older cabal
  "deb-dir:(darcs:http://src.seereason.com/HSP/happs-hsp-template):(darcs:http://src.seereason.com/debian/happs-hsp-template-debian)"
    - Depends on hs-plugins
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cabal-install/0.6.0/cabal-install-0.6.0.tar.gz:ddce0bda54a99d816091e77ab6e4b39f):(darcs:http://src.seereason.com/ghc610/debian/cabal-install-debian)"
    - Requires 3000 < HTTP < 3002
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/readline/1.0.1.0/readline-1.0.1.0.tar.gz:eade9576def53ed293628a2f8580007e):(darcs:http://src.seereason.com/ghc610/debian/readline-debian)"
    - Can't find HsReadline.h
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac-readline/0.9/Shellac-readline-0.9.tar.gz:ffea10846cc5f40b84d6a4fe97c35ec9):(darcs:http://src.seereason.com/ghc610/debian/shellac-readline-debian)"
    - Requires readline
  "quilt:(apt:sid:darcs):(darcs:http://src.seereason.com/ghc610/quilt/darcs-quilt)"
    - Version 2.2.0 hangs when compiled with ghc 6.10 
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/regex-pcre-builtin/0.94.2.0.7.7/regex-pcre-builtin-0.94.2.0.7.7.tar.gz:1e7f7ca729d344caa20c8f57d18239dd):(darcs:http://src.seereason.com/ghc610/debian/regex-pcre-builtin-debian)"
    - setup-bin: At least the following dependencies are missing: regex-base >=0.93
  "darcs:http://src.seereason.com/seereason-keyring"
    - This fails during an arch only build, because it has no architecture dependent files.
-}
    ]

otherTargets = [Target { sourcePackageName = "tree-widget"
                       , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
                       , relaxInfo = [] }
               ]

privateTargets =
    [ Target { sourcePackageName = "haskell-filecache"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-filecache"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-document"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-document"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-appraisal"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-appraisal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-mailinglist"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/mailingList"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-generic-formlets"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/generic-formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-algebrazam"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/AlgebraZam"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-senioritymatters"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/SeniorityMatters"
             , relaxInfo = [] }
    ]
    where privateDarcsURI = "ssh://upload@deb.seereason.com/srv/darcs"


{-
  deb-dir:(darcs:http://code.haskell.org/checkers):(darcs:http://src.seereason.com/debian/checkers-debian)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/MemoTrie/0.0/MemoTrie-0.0.tar.gz):(darcs:http://src.seereason.com/debian/MemoTrie-debian)
  deb-dir:(darcs:http://darcs.haskell.org/packages/TypeCompose):(darcs:http://src.seereason.com/debian/TypeCompose-debian)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/Cabal/1.4.0.0/Cabal-1.4.0.0.tar.gz:5d8f10b95c42ac7419ac9673bfb6a607):(darcs:http://src.seereason.com/debian/cabal-debianization)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/ghc-paths/0.1.0.4/ghc-paths-0.1.0.4.tar.gz:a8f36dcb5407de9907b7d78b31fc24a1):(darcs:http://src.seereason.com/debian/ghc-paths-debian)
  quilt:(darcs:http://www.cs.york.ac.uk/fp/darcs/hscolour):(darcs:http://src.seereason.com/quilt/hscolour-quilt)
  darcs:http://src.seereason.com/haskell-ugly
  darcs:http://src.seereason.com/mirror
  darcs:http://src.seereason.com/backups
  quilt:(apt:sid:xtla):(darcs:http://src.seereason.com/xtla-quilt)
  proc:apt:gutsy:neko
  proc:apt:gutsy:haxe
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/colour/1.0.0/colour-1.0.0.tar.gz:97b0802abbf3a71a3606642850fe46c7):(darcs:http://src.seereason.com/debian/colour-debian)
  apt:sid:alex
  apt:sid:bnfc
  deb-dir:(darcs:http://darcs.haskell.org/crypto):(darcs:http://src.seereason.com/debian/haskell-crypto-debian)
  apt:sid:darcs
  apt:sid:darcs-monitor
  apt:sid:drift
  apt:sid:frown
  darcs:http://www.n-heptane.com/nhlab/repos/haskell-agi
  quilt:(apt:hardy:haskell-binary):(darcs:http://src.seereason.com/quilt/haskell-binary-quilt)
  apt:sid:haskell-doc
  quilt:(apt:sid:haskell-edison):(darcs:http://src.seereason.com/quilt/edison-quilt)
  apt:sid:haskell-hlist
  quilt:(apt:sid:haskell-http):(darcs:http://src.seereason.com/quilt/haskell-http-quilt)
  apt:sid:haskell-mode
  apt:sid:haskell-uulib
  apt:sid:helium
  apt:hardy:hmake
  quilt:(apt:sid:hslogger):(darcs:http://src.seereason.com/quilt/hslogger-quilt)
  quilt:(apt:sid:ldap-haskell):(darcs:http://src.seereason.com/quilt/ldap-haskell-quilt)
  apt:sid:lhs2tex
  quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/quilt/magic-haskell-quilt)
  quilt:(apt:sid:pandoc):(darcs:http://src.seereason.com/quilt/pandoc-quilt)
  apt:sid:uuagc
  apt:sid:whitespace
  sourcedeb:darcs:http://src.seereason.com/haskell-wordnet
  quilt:(apt:sid:xmonad):(darcs:http://src.seereason.com/quilt/xmonad-quilt)
  darcs:http://src.seereason.com/hlibrary
  apt:sid:haskelldb
    - Needs doc architecture fix
  apt:hardy:haskell-hsql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-mysql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-odbc
    - Needs doc architecture fix
  apt:sid:haskell-hsql-postgresql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-sqlite3
    - Needs doc architecture fix
  apt:sid:c2hs
    - Needs doc architecture fix
  apt:sid:washngo
    - Needs patch
  apt:sid:ftphs
    - Needs patch
  apt:sid:haskell-anydbm
    - Needs patch
  apt:sid:haskell-configfile
    - Needs patch
  apt:sid:haskell-hsh
    - Needs patch
  apt:sid:listlike
    - Needs patch
  quilt:(apt:sid:missingh):(darcs:http://src.seereason.com/quilt/missingh-quilt)
    - Needs patch
  apt:sid:gtkrsync
    - Needs patch
  quilt:(apt:sid:gtk2hs):(darcs:http://src.seereason.com/quilt/gtk2hs-quilt)
    - Needs patch
  apt:sid:arch2darcs
    - Needs patch
  apt:sid:hg-buildpackage
    - Needs patch
  apt:sid:srcinst
    - Needs patch
  apt:sid:dfsbuild
    - Needs patch
  apt:sid:darcs-buildpackage
    - Needs patch
  apt:sid:hat
    - Needs patch
  gtkrsync
    - depends on gtk2hs
  arch2darcs
    - depends on missingh
  darcs-buildpackage
    - depends on missingh and haskell-configfile
  dfsbuild
    - depends on missingh, haskell-configfile, haskell-hsh
  hg-buildpackage
    - depends on ???
  srcinst
    - depends on ???
  deb-dir:(darcs:http://code.haskell.org/vector-space):(darcs:http://src.seereason.com/debian/vector-space-debian)
    - broken
  apt:sid:ghc-cvs
    - broken
  apt:sid:haskell98-report
    - broken
  quilt:(apt:sid:hdbc):(darcs:http://src.seereason.com/quilt/hdbc-quilt)
    - broken
  apt:sid:hdbc-odbc
    - broken
  apt:sid:hdbc-postgresql
    - broken
  apt:sid:hdbc-sqlite3
    - broken
  apt:sid:hpodder
    - broken
  quilt:(darcs:http://code.haskell.org/encoding):(darcs:file:///home/david/darcs/haskell-encoding)
    - Patches won't apply:
  apt:sid:hdbc-missingh
    - Depends on ghc6 (<< 6.6+) or (<< 6.6-999)
  apt:sid:kaya
    - Error parsing build depends (unexpected #):
  apt:sid:missingpy
    - Disabled due to flaw in the autobuilder's build dependency parser:
  sourcedeb:tla:dsf@foxthompson.net--2004/haskell-binary--dsf--0.3.0
  tla:dsf@foxthompson.net--2004/hxt--dsf--7.0
  sourcedeb:tla:dsf@foxthompson.net--2004/cpphs--dsf--1.3
  quilt:(apt:feisty:haskell-http):(tla:dsf@foxthompson.net--2004/haskell-http-quilt--dsf--0)
  sourcedeb:tla:dsf@foxthompson.net--2004/yhc--dsf--0.7.0
Name: kernel-targets
Targets:
  apt:gutsy:linux-source-2.6.22
  apt:gutsy:linux-meta
  quilt:(apt:gutsy:linux-restricted-modules-2.6.22):(tla:tos@linspire.com--skipjack/linux-restricted-modules-quilt--ubuntu--0)
Comment: Here are some more proposed targets
  tla:tos@linspire.com--skipjack/forward-oss-kernel-module--cnr--20070605
  tla:tos@linspire.com--skipjack/forward-oss--build-skipjack--0.3
  quilt:(apt:${base}:bcm43xx-fwcutter):(tla:tos@linspire.com--skipjack/bcm43xx-fwcutter-quilt--cnr--0)
-}

-- (BinPkgName "module-init-tools",Just (SrcPkgName "linux-2.6"))

-- Nothing below here should need to be modified.

---------------------------- THE PARAMETERS RECORD ---------------------------------
-- Assemble all the configuration info above.

-- |See Documentation in "Debian.AutoBuilder.ParamClass".
params myBuildRelease =
    ParamRec
    { vendorTag = myVendorTag
    , autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    , releaseSuffixes = myReleaseSuffixes
    , buildRelease = ReleaseName {relName = myBuildRelease}
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    , targets = myTargets myBuildRelease
    , doUpload = myDoUpload
    , doNewDist = myDoNewDist
    , flushPool = False
    , useRepoCache = True
    , forceBuild = myForceBuild
    , doSSHExport = myDoSSHExport
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
    , sources = allSources myDebianMirrorHost myUbuntuMirrorHost (isPrivateRelease myBuildRelease)
    , globalRelaxInfo = myGlobalRelaxInfo
    , strictness = P.Moderate
    , extraPackages = myExtraPackages myBuildRelease
    , extraEssential = myExtraEssential myBuildRelease
    , omitEssential = []
    , omitBuildEssential = False
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = myReleaseAliases
    , archList = [Binary "i386",Binary "amd64"]
    , newDistProgram = "newdist -v"
    -- Things that are probably obsolete
    , requiredVersion = [(parseDebianVersion "4.41", Nothing)]
    , debug = False
    , omitTargets = []
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    }

main =
    hPutStrLn stderr "Autobuilder starting..." >>
    hFlush stderr >>
    getArgs >>= M.main . map params
