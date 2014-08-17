{-# LANGUAGE MultiWayIf, OverloadedStrings, RecordWildCards, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Public ( targets ) where

import Data.Char (toLower)
import Data.FileEmbed (embedFile)
import Data.List (intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Set as Set (empty, singleton, fromList)
import Data.Text as Text (Text, lines, unlines)
import Debian.AutoBuilder.Details.Common (repo, named)
import Debian.AutoBuilder.Details.Distros (Release, baseRelease, BaseRelease(..), Release(..))
import Debian.AutoBuilder.Details.GHC (ghc)
import Debian.AutoBuilder.Types.Packages as P (RetrieveMethod(Uri, DataFiles, Patch, Cd, Darcs, Debianize, Debianize', Hackage, Apt, DebDir, Proc, Git, Dir),
                                               PackageFlag(AptPin, CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision, Maintainer,
                                                           ModifyAtoms, UDeb, OmitLTDeps, SkipVersion, SkipPackage, NoDoc, NoHoogle, GitBranch, KeepRCS),
                                               GitSpec(..), DebSpec(..), Packages(..), flags, spec,
                                               hackage, debianize, flag, patch, darcs, apt, git, cd, proc, debdir, dir)
import Debian.Debianize (compat, doExecutable, execDebM, installData, installTo, link, rulesFragments, rulesHead, InstallFile(..), (+=), (~=), (+++=), (%=))
import Debian.Debianize.Goodies (makeRulesHead)
import Debian.Relation (BinPkgName(..), SrcPkgName(..))
import System.FilePath((</>))

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"
seereason :: String
seereason = "http://src.seereason.com"
localRepo :: String
localRepo = "file:///home/dsf/darcs/"

-- Stick new packages here to get an initial build, then move
-- them to a suitable group.
new :: P.Packages
new = named "new" [ debianize (darcs (repo </> "showplease"))
                  , debianize (hackage "pseudomacros")
                  , debianize (hackage "aeson-pretty")
                  , debianize (hackage "wai-middleware-static")
                  , debianize (hackage "data-r-tree")
                  , debianize (hackage "wai-extra")
                  , debianize (hackage "wai-logger")
                  , debianize (hackage "easy-file")
                  , debianize (hackage "fast-logger")
                  , debianize (hackage "http-date")
                  , debianize (hackage "simple-sendfile")
                  , debianize (hackage "auto-update")
                  , debianize (hackage "warp")
                  , debianize (hackage "shakespeare")
                  , debianize (hackage "monad-parallel")
                  , debianize (hackage "data-stringmap")
                  , debianize (hackage "shakespeare-js")
                  , debianize (hackage "scotty")
                  , debianize (hackage "ekg-core")
                  , debianize (hackage "hamlet")
                  , debianize (git "https://github.com/hunt-framework/hunt.git" []
                                 `cd` "hunt-searchengine" )
                  , debianize (git "https://github.com/hunt-framework/hunt.git" []
                                 `cd` "hunt-server"
                                 `patch` $(embedFile "patches/hunt-server.diff")
                                 `flag` P.CabalDebian ["--default-package=hunt-server"]) ]

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> Release -> P.Packages
targets _home release =
    named "all" $
    [ new
    , main _home release
    , autobuilder _home
    , clckwrks _home release
    , sunroof release
    , haste
    , ghcjs release
    , idris release
    -- , authenticate _home release
    -- , happstackdotcom _home
    -- , happstack release
    , digestiveFunctors

    , algebra release
    -- , units
    -- , diagrams
    , fixme
    -- , higgsset
    -- , jsonb
    -- , glib
    -- , plugins
    -- , frisby
    -- , failing
    -- , agda
    -- , other
    ]

rel :: Release -> a -> a -> a
rel release precise quantal =
    case baseRelease release of
      Quantal -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Packages -> P.PackageFlag -> P.Packages
ghc74flag p _ = p

fixme :: P.Packages
fixme =
    named "fixme" $
    [ debianize (hackage "test-framework-smallcheck")
    , debianize (darcs "http://hub.darcs.net/kowey/GenI" `patch` $(embedFile "patches/GenI.diff"))
    ]

autobuilder :: FilePath -> P.Packages
autobuilder home =
    -- let repo = localRepo home in
    named "autobuilder-group" $
    [ unixutils home
    , darcs (repo </> "cabal-debian")
    , darcs (repo ++ "/haskell-debian") `flag` P.RelaxDep "cabal-debian"
    , darcs (repo </> "mirror")
    , darcs (repo </> "debian-repo")
    , darcs (repo </> "archive")
    , skip $ -- we don't want this in our repository because we need everything to work
             -- with process-listlike.
      debianize (hackage "process-extras")
        `flag` P.CabalDebian [ "--conflicts=libghc-process-extras-dev:libghc-process-listlike-dev"
                             , "--provides=libghc-process-extras-dev:libghc-process-listlike-dev"
                             , "--replaces=libghc-process-extras-dev:libghc-process-listlike-dev"
                             , "--conflicts=libghc-process-extras-prof:libghc-process-listlike-prof"
                             , "--provides=libghc-process-extras-prof:libghc-process-listlike-prof"
                             , "--replaces=libghc-process-extras-prof:libghc-process-listlike-prof"
                             , "--conflicts=libghc-process-extras-doc:libghc-process-listlike-doc"
                             , "--provides=libghc-process-extras-doc:libghc-process-listlike-doc"
                             , "--replaces=libghc-process-extras-doc:libghc-process-listlike-doc" ]
    , debianize (darcs (repo </> "process-listlike"))
        `flag` P.CabalDebian [ "--conflicts=libghc-process-listlike-dev:libghc-process-extras-dev"
                             , "--provides=libghc-process-listlike-dev:libghc-process-extras-dev"
                             , "--replaces=libghc-process-listlike-dev:libghc-process-extras-dev"
                             , "--conflicts=libghc-process-listlike-prof:libghc-process-extras-prof"
                             , "--provides=libghc-process-listlike-prof:libghc-process-extras-prof"
                             , "--replaces=libghc-process-listlike-prof:libghc-process-extras-prof"
                             , "--conflicts=libghc-process-listlike-doc:libghc-process-extras-doc"
                             , "--provides=libghc-process-listlike-doc:libghc-process-extras-doc"
                             , "--replaces=libghc-process-listlike-doc:libghc-process-extras-doc" ]
    , darcs (repo </> "process-progress")
    , debianize (darcs (repo </> "autobuilder"))
        `flag` P.CabalDebian [ "--source-package-name", "autobuilder" ]
    , debianize (darcs (repo </> "autobuilder-seereason"))
        -- It would be nice if these dependencies were in the cabal file
        `flag` P.CabalDebian [ "--depends=autobuilder-seereason:libghc-autobuilder-seereason-dev"
                             , "--depends=autobuilder-seereason:ghc"
                             , "--depends=autobuilder-seereason:debhelper"
                             , "--depends=autobuilder-seereason:apt-file"
                             , "--depends=autobuilder-seereason:apt-utils"
                             , "--depends=autobuilder-seereason:debootstrap"
                             , "--depends=autobuilder-seereason:rsync"
                             , "--depends=autobuilder-seereason:dupload"
                             , "--depends=autobuilder-seereason:darcs"
                             , "--depends=autobuilder-seereason:git"
                             , "--depends=autobuilder-seereason:tla"
                             , "--depends=autobuilder-seereason:mercurial"
                             , "--depends=autobuilder-seereason:subversion"
                             , "--depends=autobuilder-seereason:apt"
                             , "--depends=autobuilder-seereason:build-essential"
                             , "--depends=autobuilder-seereason:quilt"
                             , "--depends=autobuilder-seereason:curl"
                             , "--depends=autobuilder-seereason:debian-archive-keyring"
                             , "--depends=autobuilder-seereason:seereason-keyring"
                             -- Pull in the autobuilder-seereason library so the target's
                             -- debian/Debianize.hs scripts can run.
                             -- This is needed if the release vendor is ubuntu.  I need
                             -- to use a real type for the release value instead of a string,
                             -- then I can just ask the release who its vendor is.
                             -- , "--depends=autobuilder-seereason:ubuntu-keyring"
                             -- These are dependencies used by certain debian/Debianize.hs scripts
                             , "--recommends=autobuilder-seereason:libghc-text-dev"
                             , "--recommends=autobuilder-seereason:libghc-seereason-ports-dev" -- used by most (all?) of our web apps
                             , "--recommends=autobuilder-seereason:libghc-happstack-authenticate-dev" -- used by mimo
                             , "--recommends=autobuilder-seereason:libghc-happstack-foundation-dev" -- used by mimo
                             , "--recommends=autobuilder-seereason:libghc-safecopy-dev" -- used by mimo
                             , "--recommends=autobuilder-seereason:libghc-hsp-dev" -- used by mimo
                             , "--recommends=autobuilder-seereason:libghc-utility-ht-dev" -- used by mimo
                             ]
        `flag` P.CabalDebian [ "--conflicts=autobuilder-seereason:autobuilder"
                             , "--replaces=autobuilder-seereason:autobuilder" ]
        `flag` P.CabalDebian [ "--executable", "autobuilder-seereason" ]
        `flag` P.CabalDebian [ "--executable", "seereason-darcs-backups" ]
        `flag` P.CabalDebian [ "--source-package-name", "autobuilder-seereason" ]
    ]

unixutils :: FilePath-> P.Packages
unixutils _home =
    named "Unixutils"
    [ darcs (repo ++ "/haskell-unixutils")
    , darcs (repo </> "haskell-extra") `flag` P.RelaxDep "cabal-debian"
    , darcs (repo </> "haskell-help") ]

main :: FilePath-> Release -> P.Packages
main _home release =
    let qflag = case baseRelease release of Quantal -> flag; _ -> \ p _ -> p
        wflag = case baseRelease release of Wheezy -> flag; _ -> \ p _ -> p
        wskip t = case baseRelease release of Wheezy -> P.NoPackage; _ -> t
        wonly t = case baseRelease release of Wheezy -> t; _ -> P.NoPackage
        pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p
        sflag = case baseRelease release of Squeeze -> flag; _ -> \ p _ -> p in
    named "main" $
    [ compiler release
    , darcs "http://hub.darcs.net/ddssff/haskell-devscripts" `flag` P.RelaxDep "python-minimal"
    , platform release
    , debianize (hackage "hashtables")
    , broken $ apt "squeeze" "bugzilla" -- requires python-central (>= 0.5)
    , debianize (hackage "fmlist")
    , debianize (hackage "ListLike")
    -- Merged into ListLike-4.0
    -- , debianize (hackage "listlike-instances")
    , debianize (hackage "cpphs") -- apt (rel release "wheezy" "quantal") "cpphs"
    -- No longer available
    -- , apt "sid" "debian-keyring=2014.03.03" -- The current version (2014.04.25) seems to be missing some keys that we need
    , apt "sid" "debootstrap" `flag` P.UDeb "debootstrap-udeb"
    -- Build fails due to some debianization issue
    -- , apt "wheezy" "geneweb"
    , debianize (hackage "gtk2hs-buildtools"
                   `flag` P.CabalDebian ["--build-dep", "alex",
                                         "--build-dep", "happy",
                                         "--revision", ""])
    -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize (hackage "aeson")
    , darcs (repo </> "haskell-agi")
    , debianize (hackage "ansi-terminal")
    , debianize (hackage "ansi-wl-pprint" `tflag` P.DebVersion "0.6.7.1-1")
    , debianize (hackage "wl-pprint")
    , debianize (hackage "wl-pprint-text")
    -- Our applicative-extras repository has several important patches.
    , debianize (hackage "applicative-extras" `flag` P.DebVersion "0.1.8-1")
    , debianize (hackage "asn1-data" `tflag` P.DebVersion "0.7.1-4build1")
    , wskip $ debianize (hackage "attempt")
    , debianize (hackage "errors")
    , debianize (hackage "failure")
    , debianize (hackage "attoparsec")
    , debianize (hackage "scientific")
    , debianize (hackage "arithmoi" `flag` P.BuildDep "llvm-dev")
    , debianize (hackage "attoparsec-enumerator")
    -- This was merged into attoparsec
    -- , debianize (hackage "attoparsec-text" `patch` $(embedFile "patches/attoparsec-text.diff") `flag` P.Revision "")
    -- Deprecated
    -- , debianize (hackage "attoparsec-text-enumerator")
    , debianize (hackage "base16-bytestring")
    , debianize (hackage "base-unicode-symbols" `tflag` P.DebVersion "0.2.2.4-3")
    , debianize (hackage "bimap" `flag` P.DebVersion "0.2.4-1~hackage1")
    , debianize (hackage "Validation" `patch` $(embedFile "patches/validation.diff"))
    , debianize (hackage "template-default" `patch` $(embedFile "patches/template-default.diff"))
    , debianize (hackage "bitmap")
    , debianize (hackage "bitset")
    , debianize (hackage "bytestring-nums") -- apt (rel release "wheezy" "quantal") "haskell-bytestring-nums"
    , debianize (hackage "bytestring-trie")
    , debianize (hackage "bzlib" `flag` P.DevelDep "libbz2-dev" `tflag` P.DebVersion "0.5.0.4-2")
    -- , debianize (hackage "cairo-pdf")
    , debianize (hackage "case-insensitive")
    -- Here is an example of creating a debian/Debianize.hs file with an
    -- autobuilder patch.  The autobuilder then automatically runs this
    -- script to create the debianization.
    , broken $
      debianize (if | ghc release >= 708 ->
                        hackage "cabal-install"
                                    `flag` P.CabalPin "1.18.0.3"
                                    `patch` $(embedFile "patches/cabal-install.diff")
                    | otherwise ->
                        hackage "cabal-install"
                                    -- Waiting for Cabal 1.18.0, shipped with ghc-7.8
                                    `flag` P.CabalPin "1.16.0.2"
                                    `patch` $(embedFile "patches/cabal-install.diff"))
    -- , debianize (git "haskell-cabal-install" "https://github.com/haskell/cabal"
    --                      `cd` "cabal-install"
    --                      `patch` $(embedFile "patches/cabal-install.diff"))
    , debianize (hackage "CC-delcont" `flag` P.DebVersion "0.2-1~hackage1")
    -- , apt (rel release "wheezy" "quantal") "haskell-cereal"
    , debianize (hackage "cereal")
    , debianize (hackage "citeproc-hs" `tflag` P.DebVersion "0.3.9-1build2")
    , debianize (hackage "hexpat")
    , debianize (hackage "List")
    , debianize (hackage "network-info")
    , debianize (hackage "uuid")
    , debianize (hackage "maccatcher"
                   `pflag` P.DebVersion "2.1.5-3"
                   `tflag` P.DebVersion "2.1.5-5build1")
    , debianize (hackage "colour"
                   `pflag` P.DebVersion "2.3.3-1build1"
                   `qflag` P.DebVersion "2.3.3-1build1"
                   `sflag` P.DebVersion "2.3.3-1"
                   `tflag` P.DebVersion "2.3.3-4")
    -- , apt "wheezy" "haskell-configfile"
    , debianize (hackage "ConfigFile")
    , darcs (repo </> "haskell-consumer")
    , debianize (darcs (repo </> "module-management") `flag` P.BuildDep "rsync")
    , debianize (hackage "securemem" `tflag` P.DebVersion "0.1.3-1")
    , debianize (hackage "cipher-aes")
    , debianize (hackage "cipher-des" `tflag` P.DebVersion "0.0.6-1")
    , debianize (hackage "cprng-aes" `tflag` P.DebVersion "0.5.2-1build1")
    , debianize (hackage "crypto-random" `tflag` P.DebVersion "0.0.7-1")
    , debianize (hackage "crypto-random-api" `tflag` P.DebVersion "0.2.0-2")
    , debianize (hackage "Crypto")
    , debianize (hackage "crypto-api" `qflag` P.DebVersion "0.10.2-1build3")
    -- The certificate package may need to be updated for version 0.4
    , debianize (hackage "crypto-pubkey-types")
    -- crypto-pubkey-types-0.3.2 depends on older asn1-types
    , debianize (hackage "asn1-types")
    , debianize (hackage "byteable" `tflag` P.DebVersion "0.1.1-1")
    , debianize (hackage "cryptohash")
    , wskip $ debianize (hackage "cpu" `tflag` P.DebVersion "0.1.2-1")
    , debianize (hackage "css" `flag` P.DebVersion "0.1-1~hackage1")
    , debianize (hackage "css-text")
    , debianize (hackage "curl" `tflag` P.DebVersion "1.3.8-2") -- apt (rel release "wheezy" "quantal") "haskell-curl"
    , debianize (hackage "data-accessor")
    , debianize (hackage "data-accessor-template")
    , debianize (hackage "data-default")
    , debianize (hackage "data-default-class" `tflag` P.DebVersion "0.0.1-1")
    , debianize (hackage "data-default-instances-base")
    , debianize (hackage "data-default-instances-containers")
    , debianize (hackage "data-default-instances-dlist")
    , debianize (hackage "data-default-instances-old-locale")
    , debianize (hackage "data-object" `patch` $(embedFile "patches/data-object.diff"))
    , debianize (hackage "dataenc" {- `patch` $(embedFile "patches/dataenc.diff") -})
    , debianize (hackage "Diff" `tflag` P.DebVersion "0.3.0-1")
    , debianize (hackage "executable-path"
                   `pflag` P.DebVersion "0.0.3-1"
                   `tflag` P.DebVersion "0.0.3-3")
    -- , apt (rel release "wheezy" "quantal") "haskell-digest"
    -- , apt (rel release "wheezy" "quantal") "haskell-dlist"
    , debianize (hackage "digest")
    , debianize (hackage "dlist")
    -- Natty only(?)
    , debianize (hackage "double-conversion")
    , debianize (hackage "groom")
    -- Retired
    -- , apt "wheezy" "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.spec = Debianize' (Hackage "EdisonAPI") []
                , P.flags = rel release [] [P.DebVersion "1.2.1-18build2"] }
    , (debianize (hackage "EdisonCore" `qflag` P.DebVersion "1.2.1.3-9build2"))
    , debianize (hackage "entropy" {- `tflag` P.DebVersion "0.2.1-5" -}) -- apt (rel release "wheezy" "quantal") "haskell-entropy"
    , debianize (hackage "enumerator" `qflag` P.DebVersion "0.4.19-1build2")
    , debianize (git "https://github.com/ddssff/hdaemonize" [])
    , debianize (hackage "hsyslog")
    , debianize (hackage "erf"
                   `pflag` P.DebVersion "2.0.0.0-3"
                   `wflag` P.DebVersion "2.0.0.0-3"
                   `tflag` P.DebVersion "2.0.0.0-5")
    , debianize (hackage "feed" `tflag` P.DebVersion "0.3.9.2-1")
    -- Darcs 2.8.1 won't build with the current version of haskeline.
    -- , apt "wheezy" "darcs" `patch` $(embedFile "patches/darcs.diff")
    , debianize (hackage "file-embed")
    , debianize (hackage "filemanip" `tflag` P.DebVersion "0.3.6.2-3")
    , debianize (hackage "indents")
    , debianize (hackage "concatenative")
    , debianize (hackage "either")
    , debianize (hackage "MonadRandom")
    , debianize (hackage "formlets"
                    `patch` $(embedFile "patches/formlets.diff")
                    `flag` P.DebVersion "0.8-1~hackage1")
    , debianize (hackage "gd"
                    `patch` $(embedFile "patches/gd.diff")
                    `flag` P.DevelDep "libgd-dev"
                    `flag` P.DevelDep "libc6-dev"
                    `flag` P.DevelDep "libfreetype6-dev"
                    `wflag` P.DebVersion "3000.7.3-1"
                    `qflag` P.DebVersion "3000.7.3-1build2"
                    `tflag` P.DebVersion "3000.7.3-3")
    -- , debianize (flags [P.BuildDep "libm-dev", P.BuildDep "libfreetype-dev"] (hackage "gd"))
    , debianize (hackage "cabal-macosx" `patch` $(embedFile "patches/cabal-macosx.diff"))
    , debianize (hackage "ghc-paths" `tflag` P.DebVersion "0.1.0.9-3") -- apt (rel release "wheezy" "quantal") "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , debianize (hackage "harp"
                   `pflag` P.DebVersion "0.4-3"
                   `tflag` P.DebVersion "0.4-6") -- apt (rel release "wheezy" "quantal") "haskell-harp"
    , debianize (hackage "hashable")
    , debianize (hackage "hashed-storage")
    -- Built into ghc-7.8.3
    -- , debianize (hackage "haskeline")
    , debianize (hackage "th-orphans")
    , debianize (hackage "haskell-src-meta")
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize (hackage "HaXml")
    , debianize (hackage "heap" `flag` P.DebVersion "1.0.0-1~hackage1")
    , debianize (hackage "heist" `patch` $(embedFile "patches/heist.diff"))
    , debianize (hackage "xmlhtml")
    , debianize (hackage "directory-tree")
    , debianize (hackage "MonadCatchIO-transformers" `qflag` P.DebVersion "0.3.0.0-2build2")
    , debianize (hackage "MonadCatchIO-mtl" `patch` $(embedFile "patches/monadcatchio-mtl.diff"))
    , debianize (hackage "haskell-lexer"
                   `pflag` P.DebVersion "1.0-3build2"
                   `wflag` P.DebVersion "1.0-3+b1"
                   `tflag` P.DebVersion "1.0-5")
    , debianize (hackage "hinotify")
    , debianize (hackage "HJavaScript"
                   `patch` $(embedFile "patches/hjavascript.diff")
                   `pflag` P.DebVersion "0.4.7-3++1"
                   `tflag` P.DebVersion "0.4.7-6")
    -- Not used, and not building.
    -- , debianize (hackage "hoauth")
    , debianize (hackage "hostname"
                   `wflag` P.DebVersion "1.0-4"
                   `pflag` P.DebVersion "1.0-4build1"
                   `qflag` P.DebVersion "1.0-4build3"
                   `sflag` P.DebVersion "1.0-1~hackage1"
                   `tflag` P.DebVersion "1.0-6")
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize (hackage "HPDF")
    , debianize (hackage "hs-bibutils")
    , debianize (hackage "hsemail") -- (rel release [] [P.DebVersion "1.7.1-2build2"])
    , debianize (hackage "HsOpenSSL"
                   `flag` P.DevelDep "libssl-dev"
                   `flag` P.DevelDep "libcrypto++-dev")
    , debianize (hackage "HsSyck")
    , debianize (hackage "HStringTemplate")
    , darcs (repo </> "html-entities")
    , debianize (hackage "http-types")
    , debianize (hackage "i18n" `flag` P.DebVersion "0.3-1~hackage1")
    , debianize (hackage "iconv")
    , P.Package { P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs (repo </> "haskell-incremental-sat-solver-debian"))
                , P.flags = [] }
    , broken $ debianize (hackage "instant-generics" `flag` P.SkipVersion "0.3.7")
    , debianize (hackage "generic-deriving")
    , debianize (hackage "irc")
    , debianize (hackage "ixset" `patch` $(embedFile "patches/ixset.diff") `tflag` P.DebVersion "1.0.5-1")
    , debianize (hackage "json") -- darcs "haskell-json" (repo ++ "/haskell-json")
    , debianize (hackage "language-css" `flag` P.DebVersion "0.0.4.1-1~hackage1")
    , debianize (hackage "largeword")
    -- No cabal file
    -- , debianize (git "haskell-logic-hs" "https://github.com/smichal/hs-logic")
{-  , apt "wheezy" "haskell-leksah"
    , apt "wheezy" "haskell-leksah-server" -- for leksah -}
    , darcs (repo ++ "/haskell-logic")
    , debianize (hackage "pointed")
    , P.Package { P.spec = Debianize' (Patch (Hackage "logic-TPTP") $(embedFile "patches/logic-TPTP.diff")) []
                , P.flags = [ P.BuildDep "alex", P.BuildDep "happy" ] }
    -- , apt "sid" "haskell-maybet"
    , debianize (hackage "MaybeT" `flag` P.DebVersion "1.2-6")
    , darcs (repo </> "haskell-mime")
    , debianize (hackage "mmap")
    , debianize (hackage "monad-control" `flag` P.CabalPin "0.3.2.3")
    , debianize (hackage "monad-par-extras")
    , debianize (hackage "abstract-deque")
    , debianize (hackage "abstract-par")
    , debianize (hackage "monad-par")
    , debianize (hackage "IORefCAS" `flag` P.SkipVersion "0.2.0.1")
    , debianize (hackage "bits-atomic")
    , debianize (hackage "monadLib")
    -- Putting this in our repo can cause problems, because when it is
    -- installed some packages can't compile unless you add package
    -- qualifiers to their imports.  For this reason, when we run the
    -- autobuilder with the --lax flag we usually get a failure from
    -- some package that builds after monads-tf got installed.  On the
    -- other hand, without monads-tf we lose this dependency chain:
    -- monads-tf -> options -> fay.
    , debianize (hackage "monads-tf")
    , debianize (hackage "monoid-transformer") -- apt (rel release "wheezy" "quantal") "haskell-monoid-transformer"
    , debianize (hackage "murmur-hash")
    , debianize (hackage "mwc-random")
    , P.Package { P.spec = Debianize' (Patch (Hackage "nano-hmac") $(embedFile "patches/nano-hmac.diff")) []
                , P.flags = [P.DebVersion "0.2.0ubuntu1"] }
    , debianize (hackage "openid" `patch` $(embedFile "patches/openid.diff"))
{-  , P.Package { P.spec = Debianize' (Patch (Hackage "openid") $(embedFile "patches/openid-ghc76.diff")) []
                , P.flags = [] } -}
    , P.Package { P.spec = Debianize' (Hackage "operational") []
                , P.flags = [P.OmitLTDeps] }
--    , debianize (hackage "options")
    , debianize (hackage "optparse-applicative")
    , debianize (hackage "ordered")
    , debianize (hackage "multiset" `ghc74flag` P.CabalPin "0.2.1") -- 0.2.2 requires containers >= 0.5, which comes with ghc 7.6.
    , debianize (hackage "exceptions")
    , debianize (hackage "temporary")
    , debianize (hackage "pandoc-types")
    , debianize (hackage "deepseq-generics")
    , debianize (hackage "parse-dimacs")
    , debianize (hackage "parseargs")
    , apt (rel release "wheezy" "quantal") "haskell-parsec2" `patch` $(embedFile "patches/parsec2.diff")
    , debianize (hackage "PBKDF2")
    -- , apt (rel release "wheezy" "quantal") "haskell-pcre-light"
    , debianize (hackage "pcre-light"
                   `patch` $(embedFile "patches/pcre-light.diff")
                   `flag` P.DevelDep "libpcre3-dev")
    , debianize (hackage "permutation")
    , debianize (hackage "pipes")
    , debianize (hackage "polyparse")
    , debianize (hackage "primitive")
    , debianize (hackage "PropLogic")
    , wskip $
      debianize (hackage "PSQueue"
                   `pflag` P.DebVersion "1.1-2"
                   `qflag` P.DebVersion "1.1-2build2"
                   `sflag` P.DebVersion "1.1-1"
                   `tflag` P.DebVersion "1.1-4")
    , debianize (hackage "pwstore-purehaskell"
                   -- `patch` $(embedFile "patches/pwstore-purehaskell.diff")
                   -- `flag` P.DebVersion "2.1-1~hackage1"
                )
    -- Retired
    -- , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
    , debianize (hackage "regex-tdfa")
    , darcs (repo </> "haskell-revision")
    , debianize (hackage "RJson"
                   `patch` $(embedFile "patches/RJson.diff")
                   `wflag` P.DebVersion "0.3.7-1~hackage1")
    , debianize (hackage "safe" `flag` P.CabalPin "0.3.7") -- avoid a rebuild
    , debianize (hackage "safecopy")
    , debianize (hackage "SafeSemaphore")
    , debianize (hackage "sat"
                   `patch` $(embedFile "patches/sat.diff")
                   `flag` P.DebVersion "1.1.1-1~hackage1")
    , debianize (hackage "semigroups" `flag` P.CabalPin "0.15.1")
    , debianize (hackage "nats")
    , debianize (hackage "sendfile" `tflag` P.DebVersion "0.7.9-1")
    , darcs (repo </> "set-extra")
    -- I don't think we use this any more
    -- , debianize (darcs "haskell-old-exception" (repo ++ "/old-exception"))
    , debianize (hackage "SHA") -- apt (rel release "wheezy" "quantal") "haskell-sha"
    , debianize (hackage "shake")
    , debianize (hackage "byteorder" `tflag` P.DebVersion "1.0.4-1")
    , debianize (hackage "word8")
    , debianize (hackage "system-fileio")
    , debianize (hackage "SMTPClient")
    , debianize (hackage "socks")
    , debianize (hackage "split" `tflag` P.DebVersion "0.2.2-1")
    -- Version 1.14, which is in darcs, is too new for the current haskell-src-meta and haskell-derive
    , debianize (-- darcs "haskell-haskell-src-exts" "http://code.haskell.org/haskell-src-exts"
                 hackage "haskell-src-exts"
                   `flag` P.BuildDep "happy")
    , debianize (hackage "stb-image")
    , debianize (hackage "strict"
                   `pflag` P.DebVersion "0.3.2-2"
                   `tflag` P.DebVersion "0.3.2-7") -- apt (rel release "wheezy" "quantal") "haskell-strict" -- for leksah
    -- , debianize (hackage "strict-concurrency" `wflag` P.DebVersion "0.2.4.1-2")
    , debianize (hackage "strict-io") -- for GenI
    , debianize (hackage "smallcheck")
    , debianize (hackage "syb-with-class"
                             `patch` $(embedFile "patches/syb-with-class.diff")
                             `tflag` P.DebVersion "0.6.1.4-2"
                             `flag` P.CabalPin "0.6.1.4") -- Version 0.6.1.5 tries to derive typeable instances when building rjson, which is an error for ghc-7.8
    , broken $
      debianize (hackage "syb-with-class-instances-text"
                   `pflag` P.DebVersion "0.0.1-3"
                   `wflag` P.DebVersion "0.0.1-3"
                   `wflag` P.SkipVersion "0.0.1-3"
                   `tflag` P.DebVersion "0.0.1-6build1")
    , debianize (hackage "tagged")
    , debianize (hackage "tagsoup")
    , debianize (hackage "tar" `tflag` P.DebVersion "0.4.0.1-3")
{-  -- This is built into ghc-7.8.3
    , debianize (hackage "terminfo"
                             `flag` P.DevelDep "libncurses5-dev"
                             `flag` P.DevelDep "libncursesw5-dev") -}
    , debianize (hackage "test-framework" `tflag` P.DebVersion "0.8.0.3-1build3")
    , debianize (hackage "test-framework-hunit" `tflag` P.DebVersion "0.3.0.1-1build4")
    -- Retired
    -- , debianize (hackage "test-framework-quickcheck")
    , debianize (hackage "test-framework-quickcheck2" `flag` P.SkipVersion "0.3.0.2") -- waiting for quickcheck2-2.7 support
    , debianize (hackage "test-framework-th" `tflag` P.DebVersion "0.2.4-1build4")
    --
    -- , debianize (hackage "testpack" `patch` $(embedFile "patches/testpack.diff"))
    , debianize (hackage "th-expand-syns")
    , debianize (hackage "th-lift")
    , debianize (hackage "transformers-base"
                             `flag` P.CabalPin "0.4.1" -- prevent rebuilds
                             `pflag` P.DebVersion "0.4.1-2"
                             `qflag` P.DebVersion "0.4.1-2build2"
                             `wflag` P.DebVersion "0.4.1-2"
                             `tflag` P.DebVersion "0.4.1-5")
    , debianize (hackage "unicode-names" `flag` P.DebVersion "3.2.0.0-1~hackage1")
    , debianize (hackage "unicode-properties"
                   `patch` $(embedFile "patches/unicode-properties.diff")
                   `flag` P.DebVersion "3.2.0.0-1~hackage1")
    , debianize (hackage "uniplate")
    , debianize (hackage "cmdargs")
    , debianize (hackage "language-javascript"
                   `flag` P.BuildDep "happy"
                   `flag` P.BuildDep "alex"
                   -- `flag` P.CabalPin "0.5.12" -- 0.5.13 needs alex>=3.0.5
                )
    , debianize (hackage "utf8-light")
    , debianize (hackage "language-haskell-extract")
    , debianize (hackage "pretty-show" `flag` P.BuildDep "happy")
    , debianize (hackage "language-ecmascript")
    , debianize (hackage "testing-feat")
    , debianize (hackage "tagshare")
    , debianize (hackage "charset")
    , debianize (hackage "union-find")
    -- , debianize (hackage "Elm")
    -- , debianize (hackage "elm-server" {- `patch` $(embedFile "patches/elm-server.diff") -})
    , debianize (hackage "gdiff")
    , debianize (hackage "hjsmin")
    , debianize (hackage "unix-compat")
    , debianize (hackage "Unixutils-shadow")
    , debianize (hackage "unordered-containers")
    , debianize (hackage "utf8-prelude" `flag` P.DebVersion "0.1.6-1~hackage1")
    -- The GHC in wheezy conflicts with libghc-containers-dev, so we can't build this.
    -- , wonly $ debianize (hackage "containers")
    , debianize (hackage "utf8-string"
                   `flag` P.RelaxDep "hscolour"
                   `flag` P.RelaxDep "cpphs")
    -- , P.Package { P.spec = Apt (rel release "wheezy" "quantal") "haskell-utf8-string"
    --             , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize (hackage "unification-fd" `flag` P.SkipVersion "0.8.0")
    , debianize (hackage "newtype" `wflag` P.DebVersion "0.2-1" `tflag` P.DebVersion "0.2-3")
    , P.Package { P.spec = Debianize' (Hackage "logict") []
                , P.flags = [] }
    , debianize (hackage "utility-ht")
    , debianize (hackage "vacuum" `flag` P.SkipVersion "2.1.0.1")
    , debianize (hackage "vector" `patch` $(embedFile "patches/vector.diff"))
    , debianize (hackage "vector-algorithms")
    , P.Package { P.spec = Debianize' (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff")) []
                , P.flags =  [] }
    , debianize (hackage "vault")
    , debianize (hackage "wai" {- `patch` $(embedFile "patches/wai.diff") -})
    , P.Package { P.spec = Debianize' (Patch (Hackage "web-encodings") $(embedFile "patches/web-encodings.diff")) []
                , P.flags = [] }
    , debianize (hackage "boomerang")
    , debianize (hackage "xml") -- apt (rel release "wheezy" "quantal") "haskell-xml"
    , debianize (hackage "cookie")
    , debianize (hackage "lifted-base")
    , debianize (hackage "system-filepath" `flag` P.CabalPin "0.4.11") -- avoid a rebuild
    , P.Package { P.spec = Debianize' (Patch (Hackage "xml-enumerator") $(embedFile "patches/xml-enumerator.diff")) []
                , P.flags = [] }
    , debianize (hackage "xml-types" `tflag` P.DebVersion "0.3.4-1")
    , debianize (hackage "xss-sanitize" `qflag` P.DebVersion "0.3.2-1build1")
    , debianize (hackage "yaml")
    , debianize (hackage "yaml-light"
                   `wflag` P.DebVersion "0.1.4-2"
                   `pflag` P.DebVersion "0.1.4-2"
                   `qflag` P.DebVersion "0.1.4-2build1"
                   `tflag` P.DebVersion "0.1.4-5build1")
    , debianize (hackage "zip-archive")
    , debianize (hackage "regex-pcre-builtin"
                   -- Need to email Audrey Tang <audreyt@audreyt.org> about this.
                   `patch` $(embedFile "patches/regex-pcre-builtin.diff")
                   `flag` P.DevelDep "libpcre3-dev")
    , debianize (hackage "hscolour") `flag` P.RelaxDep "hscolour"
    , debianize (hackage "hslogger")
    , debianize (hackage "extensible-exceptions" -- required for ghc-7.6.  Conflicts with ghc-7.4 in wheezy.
                   `tflag` P.DebVersion "0.1.1.4-2")
    , case baseRelease release of
        Quantal -> P.NoPackage -- This build hangs when performing tests
        Wheezy -> P.NoPackage -- This build hangs when performing tests
        _ -> apt "sid" "html-xml-utils"
    , apt "sid" "jquery"
    , apt "sid" "jquery-goodies" `patch` $(embedFile "patches/jquery-goodies.diff")
    -- We want to stick with jqueryui-1.8 for now, so create
    -- packages with the version number embedded in the name.
    , darcs (repo </> "jqueryui18")
    , case baseRelease release of
        Precise -> proc (apt "trusty" "libjs-jcrop")
        _ -> P.NoPackage
{-
    , P.Package { P.spec = DebDir (Uri (repo </> "jcrop/Jcrop.tar.gz") "028feeb9b6415af3b7fd7d9471c92469") (Darcs (repo ++ "/jcrop-debian"))
                , P.flags = [] }
-}
    , debianize (hackage "magic" `flag` P.DevelDep "libmagic-dev" `flag` P.DebVersion "1.0.8-12")
{-  , P.Package { P.spec = Quilt (Apt "wheezy" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] } -}
    , debianize (hackage "MissingH")
    , darcs (repo </> "seereason-keyring") `flag` P.UDeb "seereason-keyring-udeb"
    , debianize (darcs (repo </> "seereason-ports"))
    , apt "wheezy" "tinymce"
    , P.Package { P.spec = Darcs (repo </> "vc-darcs")
                , P.flags = [] }
    , debianize (hackage "wl-pprint-extras")
    , debianize (hackage "HaTeX")
    , debianize (hackage "matrix")
    , debianize (hackage "hlatex")
    , debianize (hackage "latex")
    , debianize (hackage "texmath" `flag` P.CabalPin "0.6.6.3") -- waiting for pandoc > 1.12.4.2
    , debianize (hackage "derive")
    , debianize (hackage "frquotes")
    -- Usable versions of this package are available in some dists -
    -- e.g. trusty and wheezy.
    -- , apt "trusty" "foo2zjs"
    , debianize (hackage "stringsearch")
    , debianize (hackage "rss")
    , debianize (hackage "async")
    -- Waiting for a newer GHC
    -- , debianize (hackage "units" `flag` P.CabalPin "1.0.0" {- `patch` $(embedFile "patches/units.diff") -})
    , debianize (hackage "csv"
                   `pflag` P.DebVersion "0.1.2-2"
                   `tflag` P.DebVersion "0.1.2-5build1")
{-
    -- Needs a build dependency on libXrandr-dev and the cabal package x11.
    , P.Package { P.spec = Debianize' (Hackage "xmobar") []
                , P.flags = [] }
-}
    -- Needs update for current http-conduit
    -- , debianize $ (hackage "dropbox-sdk") `patch` $(embedFile "patches/dropbox-sdk.diff")
    , debianize (darcs (repo ++ "/hS3"))
                `tflag` P.DebVersion "0.5.7-3build1"
                `pflag` P.DebVersion "0.5.6-2"
                `wflag` P.DebVersion "0.5.6-2"
                `flag` P.ModifyAtoms (execDebM $ doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
    , debianize (hackage "urlencoded")
    , debianize (hackage "hxt")
    , debianize (hackage "hxt-charproperties")
    , debianize (hackage "hxt-regex-xmlschema")
    , debianize (hackage "hxt-unicode")
    -- , debianize (darcs "haskell-tiny-server" (repo </> "tiny-server") `flag` P.BuildDep "hsx2hs"
    --                `flag` P.SkipPackage {- has a "derives SafeCopy" -})
    , debianize (hackage "stringable") -- this can be done with listlike-instances
    , debianize (hackage "currency")
    , debianize (hackage "iso3166-country-codes")
    , debianize (hackage "memoize")
    ]

relax :: P.Packages -> String -> P.Packages
relax p@(P.Package {}) x = p {P.flags = P.flags p ++ [P.RelaxDep x]}
relax (P.Named n p) x = P.Named n (relax p x)
relax (P.Packages ps) x = P.Packages (map (\ p -> relax p x) ps)
relax p@P.NoPackage _ = p

compiler :: Release -> P.Packages
compiler release =
    named "ghc" [ghc]
    where
      ghc = case baseRelease release of
              Squeeze ->
                  -- This might also work for 7.8
                  ghcFlags ghc76 `patch` $(embedFile "patches/ghc.diff") <>
                  apt "wheezy" "po4a" <>
                  apt "wheezy" "debhelper" `patch` $(embedFile "patches/debhelper.diff") <>
                  apt "wheezy" "dpkg" `patch` $(embedFile "patches/dpkg.diff") <>
                  apt "wheezy" "makedev"
              _ -> ghcFlags ghc78
      -- Pin ghc to revision 3, revision 4 still conflicts with
      -- libghc-cabal-dev so it doesn't buy us anything.  Watch for
      -- revision 5.
      ghc78 = case baseRelease release of
                -- Just to avoid a rebuild for now
                Trusty -> P.NoPackage
                Precise -> P.NoPackage
                _ -> proc (apt "experimental" "ghc")
      ghc76 = apt "sid" "ghc" -- up to revision 13 now
      ghcFlags p = p `relax` "ghc"
                     `relax` "happy"
                     `relax` "alex"
                     `relax` "xsltproc"
                     `relax` "debhelper"
                     `relax` "quilt"
                     `relax` "python-minimal"
                     `relax` "libgmp-dev"
      squeezeRelax = case baseRelease release of Squeeze -> relax; _ -> \ p _ -> p
      squeezePatch = case baseRelease release of Squeeze -> patch; _ -> \ p _ -> p
      wskip t = case baseRelease release of Wheezy -> P.NoPackage; _ -> t

platform :: Release -> P.Packages
platform release =
    let qflag = case baseRelease release of Quantal -> flag; _ -> \ p _ -> p
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p
        wflag = case baseRelease release of Wheezy -> flag; _ -> \ p _ -> p
        wskip t = case baseRelease release of Wheezy -> P.NoPackage; _ -> t
        pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p
        sflag = case baseRelease release of Squeeze -> flag; _ -> \ p _ -> p in
    named "platform" $
    [ -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      broken $
      P.Package { P.spec = DebDir (Hackage "happy") (Darcs (repo </> "happy-debian")),
                  P.flags = [P.RelaxDep "happy", P.CabalDebian ["--executable", "happy"],
                             P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize (hackage "stm")
    , debianize (hackage "stm-chans")
    , debianize (hackage "zlib" `flag` P.DevelDep "zlib1g-dev" `tflag` P.DebVersion "0.5.4.1-1")
    , debianize (hackage "mtl" `flag` P.CabalPin "2.1.3.1") -- minimize rebuild
    , wskip $ debianize (hackage "transformers" `flag` P.CabalPin "0.3.0.0" `tflag` P.DebVersion "0.3.0.0-5" `qflag` P.DebVersion "0.3.0.0-1build3")
    , debianize (hackage "parallel")
    , debianize (hackage "syb")
    , debianize (hackage "fgl" `flag` P.CabalPin "5.5.0.1") -- Minimizing rebuild
    , debianize (hackage "text")
    , P.Package { P.spec = Debianize' (Hackage "alex") []
                  -- alex shouldn't rebuild just because alex seems newer, but alex does require
                  -- an installed alex binary to build
                , P.flags = [P.RelaxDep "alex",
                             P.BuildDep "alex",
                             P.BuildDep "happy",
                             P.CabalDebian ["--executable", "alex"],
                             P.ModifyAtoms (execDebM $ do compat ~= Just 9
                                                          mapM_ (\ name -> installData +++= (BinPkgName "alex", singleton (name, name)))
                                                               [ "AlexTemplate"
                                                               , "AlexTemplate-debug"
                                                               , "AlexTemplate-ghc"
                                                               , "AlexTemplate-ghc-debug"
                                                               , "AlexTemplate-ghc-nopred"
                                                               , "AlexWrapper-basic"
                                                               , "AlexWrapper-basic-bytestring"
                                                               , "AlexWrapper-gscan"
                                                               , "AlexWrapper-monad"
                                                               , "AlexWrapper-monad-bytestring"
                                                               , "AlexWrapper-monadUserState"
                                                               , "AlexWrapper-monadUserState-bytestring"
                                                               , "AlexWrapper-posn"
                                                               , "AlexWrapper-posn-bytestring"
                                                               , "AlexWrapper-strict-bytestring"]) ] }
    , opengl release
    -- , haddock release
    , debianize (hackage "haskell-src" `flag` P.BuildDep "happy")
    -- Versions 2.4.1.1 and 2.4.1.2 change unEscapeString in a way
    -- that breaks our apps: https://github.com/haskell/network/issues/86
    , debianize (hackage "network" `flag` P.CabalPin "2.4.2.2") -- Waiting for newer rss, hslogger
    , debianize (hackage "publicsuffixlist" `tflag` P.DebVersion "0.1-1build4")
    , debianize (hackage "HTTP")
    , debianize (hackage "cgi" `flag` P.SkipVersion "3001.1.8.5") -- incompatible with current Typeable
    -- This is bundled with the compiler
    -- , debianize (hackage "process")
    , debianize (hackage "random"
                   `pflag` P.DebVersion "1.0.1.1-1"
                   `qflag` P.DebVersion "1.0.1.1-1build2"
                   `wflag` P.DebVersion "1.0.1.1-1"
                   `tflag` P.DebVersion "1.0.1.1-3")
    , debianize (hackage "HUnit" `tflag` P.DebVersion "1.2.5.2-1")
    , debianize (hackage "tf-random")
    , debianize (hackage "QuickCheck" `flag` P.BuildDep "libghc-random-prof")
    , debianize (hackage "parsec" `flag` P.CabalDebian (replacementLibrary "parsec2" "parsec3"))
    , debianize (hackage "html"
                   `tflag` P.DebVersion "1.0.1.2-7"
                   `pflag` P.DebVersion "1.0.1.2-5") -- apt (rel release "wheezy" "quantal") "haskell-html"
    , debianize (hackage "regex-compat"
                   `pflag` P.DebVersion "0.95.1-2"
                   `tflag` P.DebVersion "0.95.1-4") -- apt (rel release "wheezy" "quantal") "haskell-regex-compat"
    , debianize (hackage "regex-base"
                   `tflag` P.DebVersion "0.93.2-4"
                   `pflag` P.DebVersion "0.93.2-2") -- apt (rel release "wheezy" "quantal") "haskell-regex-base"
    , debianize (hackage "regex-posix" `tflag` P.DebVersion "0.95.2-3")
    -- Built into ghc-7.8.3
{-  , debianize (hackage "xhtml"
                   `wflag` P.DebVersion "3000.2.1-1"
                   `qflag` P.DebVersion "3000.2.1-1build2"
                   `tflag` P.DebVersion "3000.2.1-4") -}
    ]

clckwrks :: String -> Release -> P.Packages
clckwrks _home release =
    let repo = "http://src.seereason.com/mirrors/clckwrks-dev"
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
    -- let repo = "http://hub.darcs.net/stepcut/clckwrks-dev" in
    named "clckwrks" $
        [ happstack _home release
        , authenticate _home release
        , happstackdotcom _home
        , plugins
        , P.Package { P.spec = Debianize'
                                         (Patch
                                          (DataFiles
                                           (DataFiles
                                            (Cd "clckwrks" (Darcs repo))
                                            (Uri "http://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                 "e211065e573ea0239d6449882c9d860d")
                                            "jstree")
                                           (Uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                                                "5eecb009ae16dc54f261f31da01dbbac")
                                           "json2")
                                          $(embedFile "patches/clckwrks.diff")) []
                    , P.flags = [P.BuildDep "hsx2hs"] }
        , debianize (darcs repo `cd` "clckwrks-cli" {- `patch` $(embedFile "patches/clckwrks-cli.diff") -})
        , debianize (darcs repo
                       `cd` "clckwrks-plugin-bugs"
                       `flag` P.BuildDep "hsx2hs")
        , debianize (darcs repo
                       `cd` "clckwrks-plugin-media"
                       `flag` P.BuildDep "hsx2hs")
        , debianize (darcs repo
                       `cd` "clckwrks-plugin-ircbot"
                       `flag` P.BuildDep "hsx2hs")
        , debianize (darcs repo `cd` "clckwrks-theme-bootstrap" `flag` P.BuildDep "hsx2hs")
        , debianize (darcs repo `cd` "clckwrks-dot-com"
                           -- This is a change that only relates to the autobuilder
                           `patch` $(embedFile "patches/clckwrks-dot-com.diff"))
        , debianize (darcs repo `cd` "clckwrks-theme-clckwrks" `flag` P.BuildDep "hsx2hs")
        , debianize (hackage "jmacro")
        , debianize (hackage "hsx-jmacro")
        , debianize (hackage "monadlist")
        , debianize (darcs repo
                       `cd` "clckwrks-plugin-page"
                       `patch` $(embedFile "patches/clckwrks-plugin-page.diff")
                       `flag` P.BuildDep "hsx2hs")
        ]

fay :: String -> Release -> P.Packages
fay _home _release =
    named "fay"
    [ debianize (hackage "happstack-fay" `patch` $(embedFile "patches/happstack-fay.diff"))
    , debianize (hackage "type-eq")
    , debianize (hackage "haskell-names")
    , debianize (hackage "happstack-fay-ajax" `patch` $(embedFile "patches/happstack-fay-ajax.diff"))
    -- , debianize (hackage "fay-hsx" `patch` $(embedFile "patches/fay-hsx.diff"))
    , debianize (hackage "fay" {- `patch` $(embedFile "patches/fay.diff") -}) `flag` P.CabalDebian [ "--depends=haskell-fay-utils:cpphs" ]
    , debianize (hackage "fay-base")
    , debianize (hackage "fay-text")
    , debianize (git "https://github.com/faylang/fay-jquery" [])
    -- , debianize (hackage "fay-jquery" `flag` P.CabalPin "0.3.0.0")
{-  , debianize (darcs "mastermind" (darcsHub ++ "/mastermind")
                   `flag` P.CabalDebian ["--build-dep=hsx2hs",
                                         "--build-dep=haskell-fay-utils",
                                         "--build-dep=haskell-fay-base-utils",
                                         "--build-dep=haskell-fay-hsx-utils",
                                         "--build-dep=haskell-fay-jquery-utils",
                                         "--build-dep=haskell-happstack-fay-ajax-utils"]) -} -- waiting for a fix
    ]

happstack :: String -> Release -> P.Packages
happstack _home release =
    let privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String
        pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
    named "happstack"
    [ plugins
    , debianize (darcs (repo ++ "/seereason-base"))
    , debianize (hackage "happstack")
    , debianize (darcs (darcsHub ++ "/happstack") `cd` "happstack-foundation")
    , debianize (hackage "cryptohash-cryptoapi")
    , debianize (hackage "hsx2hs" `flag` P.CabalDebian ["--executable", "hsx2hs",
                                                        "--conflicts=hsx2hs:haskell-hsx-utils",
                                                        "--replaces=hsx2hs:haskell-hsx-utils",
                                                        "--provides=hsx2hs:haskell-hsx-utils"])
    -- maybe obsolete, src/HTML.hs:60:16: Not in scope: `selectElement'
    , debianize (hackage "sourcemap")
    , debianize (hackage "haskell-packages")
    , debianize (hackage "hse-cpp")
    , debianize (darcs (repo ++ "/happstack-extra"))
{-  , debianize (git "haskell-haskell-names" "https://github.com/haskell-suite/haskell-names")
    , debianize (git "haskell-haskell-packages" "https://github.com/haskell-suite/haskell-packages")
    , debianize (git "haskell-hse-cpp" "https://github.com/haskell-suite/hse-cpp")
      -- These will break everything
    , debianize (git "haskell-haskell-src-exts" "https://github.com/haskell-suite/haskell-src-exts")
    , debianize (git "haskell-cabal" "https://github.com/haskell/cabal" `cd` "Cabal") -}
    -- , debianize (hackage "cabal-install" `patch` $(embedFile "patches/cabal-install.diff"))
    , debianize (hackage "EitherT")
    , debianize (hackage "traverse-with-class")
    , debianize (hackage "happstack-hsp"
                   -- `patch` $(embedFile "patches/happstack-hsp.diff")
                   `flag` P.BuildDep "hsx2hs")
    , debianize (hackage "happstack-jmacro")
    , broken $ debianize (hackage "jmacro-rpc-happstack" `flag` P.SkipVersion "0.2.1") -- Really just waiting for jmacro-rpc
    , broken $ debianize (hackage "jmacro-rpc")
    , darcs (repo ++ "/happstack-search")
    -- , debianize (hackage "happstack-server")
    , debianize (darcs (repo ++ "/happstack-server-debug") `cd` "happstack-server")
    , debianize (hackage "happstack-lite")
--    , debianize (darcs "haskell-happstack-server-tls" "http://hub.darcs.net/stepcut/happstack" `cd` "happstack-server-tls")
    , debianize (darcs (repo ++ "/happstack-server-debug") `cd` "happstack-server-tls")
    , debianize (hackage "time-compat")
    , debianize (hackage "base64-bytestring" `tflag` P.DebVersion "1.0.0.1-1")
    , debianize (hackage "threads")
    , debianize (hackage "list-tries" `patch` $(embedFile "patches/list-tries.diff"))
    , debianize (hackage "happstack-static-routing")
    , debianize (hackage "happstack-util"
                   `patch` $(embedFile "patches/happstack-util-ghc76.diff")
                   `flag` P.DebVersion "6.0.3-1")
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize (hackage "hsp" `flag` P.BuildDep "hsx2hs")
    , debianize (hackage "hslua")
    , debianize (hackage "pandoc"
                   `patch` $(embedFile "patches/pandoc.diff")
                   `flag` P.RelaxDep "libghc-pandoc-doc"
                   `flag` P.BuildDep "alex"
                   `flag` P.BuildDep "happy")
    , debianize (hackage "markdown" `patch` $(embedFile "patches/markdown.diff"))
    , debianize (hackage "highlighting-kate")
    , debianize (hackage "web-routes")
    , debianize (hackage "web-routes-boomerang")
    , debianize (hackage "web-routes-happstack")
    , debianize (hackage "web-routes-hsp")
    , debianize (hackage "web-routes-mtl" `flag` P.DebVersion "0.20.1-1~hackage1")
    , debianize (hackage "web-routes-th")
    , debianize (darcs (repo ++ "/happstack-scaffolding")
                   `flag` P.BuildDep "hsx2hs")
    , debianize (hackage "HJScript")
    , debianize (darcs (seereason ++ "/reform") `cd` "reform")
    , debianize (darcs (seereason </> "reform") `cd` "reform-blaze")
    , debianize (darcs (darcsHub ++ "/reform") `cd` "reform-happstack")
    -- , debianize (darcs (darcsHub ++ "/reform") `cd` "reform-heist")
    , debianize (darcs (seereason </> "reform") `cd` "reform-hsp" `flag` P.BuildDep "hsx2hs")
    , debianize (hackage "blaze-builder")
    , debianize (hackage "blaze-markup")
    -- , apt (rel release "wheezy" "quantal") "haskell-blaze-builder"
    , debianize (hackage "blaze-builder-enumerator")
    , debianize (hackage "blaze-from-html")
    , debianize (hackage "blaze-html")
    , debianize (hackage "blaze-textual")
    , debianize (hackage "blaze-textual-native"
                   `patch` $(embedFile "patches/blaze-textual-native.diff")
                   `flag` P.Revision "")
    , debianize (darcs (repo ++ "/happstack-clckwrks")
                   `cd` "clckwrks-theme-happstack"
                   -- `patch` $(embedFile "patches/clckwrks-theme-happstack.diff")
                   `flag` P.BuildDep "hsx2hs")
    , debianize (darcs (repo ++ "/happstack-clckwrks")
                   `cd` "happstack-dot-com"
                   -- This is a change that only relates to the autobuilder
                   `patch` $(embedFile "patches/happstack-dot-com.diff"))
    , debianize (hackage "acid-state" {- `patch` $(embedFile "patches/acid-state.diff") -})
    ]

authenticate _home release =
  let pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p
      tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
  named "authenticate" $
    [ conduit release
    , debianize (hackage "pureMD5" `tflag` P.DebVersion "2.1.2.1-3build3")
    , debianize (hackage "monadcryptorandom")
    , debianize (hackage "RSA")
    , debianize (hackage "DRBG")
    , debianize (hackage "prettyclass")
    , debianize (hackage "cipher-aes128")
    , debianize (hackage "resourcet")
    , debianize (hackage "mmorph")
    , debianize (hackage "void" `tflag` P.DebVersion "0.6.1-1build1")
    , debianize (hackage "certificate" `tflag` P.DebVersion "1.3.9-1build4")
    , debianize (hackage "pem")
    , debianize (hackage "zlib-bindings")
    , debianize (hackage "tls")
    -- tls-extra deprecated in favor of tls
    -- , debianize (hackage "tls-extra" `patch` $(embedFile "patches/tls-extra.diff"))
    , debianize (hackage "asn1-encoding")
    , debianize (hackage "asn1-parse")
    , debianize (hackage "x509")
    , debianize (hackage "x509-store")
    , debianize (hackage "x509-system")
    , debianize (hackage "x509-validation")
    , debianize (hackage "cipher-rc4" `tflag` P.DebVersion "0.1.4-1")
    , debianize (hackage "crypto-pubkey" `tflag` P.DebVersion "0.2.4-1build1")
    , debianize (hackage "crypto-numbers"
                   `patch` $(embedFile "patches/crypto-numbers.diff")
                   `tflag` P.DebVersion "0.2.3-1")
    , debianize (hackage "crypto-cipher-types" `tflag` P.DebVersion "0.0.9-1")
    , debianize (hackage "authenticate")
    , debianize (hackage "zlib-enum")
    , debianize (darcs (darcsHub ++ "/happstack") `cd` "happstack-authenticate" `patch` $(embedFile "patches/happstack-authenticate.diff"))
    , digestiveFunctors
    , debianize (hackage "fb")
    , debianize (hackage "monad-logger")
    , debianize (hackage "monad-loops")
    , debianize (hackage "fast-logger")
    , debianize (hackage "auto-update")
    , debianize (hackage "date-cache" `tflag` P.DebVersion "0.3.0-3")
    , debianize (hackage "unix-time")
    ]

digestiveFunctors =
    named "digestive-functors"
    [ debianize (hackage "digestive-functors" `flag` P.CabalPin "0.2.1.0")  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
    -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
    , debianize (hackage "digestive-functors-happstack"
                   `patch` $(embedFile "patches/digestive-functors-happstack.diff")
                   `flag` P.CabalPin "0.1.1.5"
                   `flag` P.DebVersion "0.1.1.5-2")
    , debianize (darcs (repo ++ "/digestive-functors-hsp") `flag` P.BuildDep "hsx2hs") ]

-- | We need new releases of all the conduit packages before we can move
-- from conduit 0.4.2 to 0.5.
conduit release =
  let tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
  named "conduit"
    [ debianize (hackage "streaming-commons" `flag` P.CabalPin "0.1.4") -- avoid rebuild
    , debianize (hackage "conduit" {- `flag` P.CabalPin "1.0.17.1" -})
    , debianize (hackage "text-stream-decode" `patch` $(embedFile "patches/text-stream-decode.diff"))
    , debianize (hackage "connection")
    , debianize (hackage "http-conduit" `flag` P.CabalPin "2.1.3") -- avoid rebuild
    , debianize (hackage "http-client" `flag` P.CabalPin "0.3.5") -- avoid rebuild
    , debianize (hackage "http-client-tls")
    -- Deprecated in favor of http-conduit
    -- , debianize (hackage "http-client-conduit" {- `flag` P.CabalPin "0.2.0.1" -})
    -- Deprecated in favor of conduit-extra
    -- , debianize (hackage "attoparsec-conduit" {- `flag` P.CabalPin "1.0.1.2" `tflag` P.DebVersion "1.0.1.2-1build2" -})
    -- , debianize (hackage "blaze-builder-conduit" {- `flag` P.CabalPin "1.0.0" `tflag` P.DebVersion "1.0.0-2build4" -})
    -- , debianize (hackage "zlib-conduit" {- `flag` P.CabalPin "1.0.0" `tflag` P.DebVersion "1.0.0-2build3" -})
    , debianize (hackage "xml-conduit" `patch` $(embedFile "patches/xml-conduit.diff") `flag` P.CabalPin "1.2.0.3") -- avoid rebuild
    , debianize (hackage "tagstream-conduit" `patch` $(embedFile "patches/tagstream-conduit.diff"))
    , debianize (hackage "conduit-extra"
                   -- `flag` P.CabalPin "1.0.0"
                   -- `patch` $(embedFile "patches/conduit-extra.diff")
                )
    , debianize (hackage "mime-types")
    ]

-- ircbot needs a dependency on containers
happstackdotcom _home =
    named "happstackdotcom" $
    [ debianize (hackage "ircbot")
    , debianize (hackage "SafeSemaphore")
    , darcs (repo </> "happstackDotCom-doc") ]

shakespeare =
    named "shakespeare-group" $
    [ debianize (hackage "wai-extra")
    , debianize (hackage "warp")
    , debianize (hackage "cryptohash-conduit")
    , debianize (hackage "wai-app-static")
    , debianize (hackage "network-conduit")
    , debianize (hackage "simple-sendfile")
    , debianize (hackage "streaming-commons")
    , debianize (hackage "wai-logger")
    , debianize (hackage "http-date")
    , debianize (hackage "shakespeare")
    ]


-- May work with these added dependencies (statevar thru openglraw)
opengl release = named "opengl" $
    let qflag = case baseRelease release of Quantal -> flag; _ -> \ p _ -> p
        wskip t = case baseRelease release of Wheezy -> P.NoPackage; _ -> t
        wflag = case baseRelease release of Wheezy -> flag; _ -> \ p _ -> p
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p
        pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p in
    [ debianize (hackage "OpenGL"
                   `flag` P.DevelDep "libglu1-mesa-dev")
{-  , P.Package { P.spec =  Debianize' (Patch (Hackage "vacuum-opengl")) []
                                 (unlines
                                  [ "--- old/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:26:14.000000000 -0700"
                                  , "+++ new/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:32:17.027953252 -0700"
                                  , "@@ -34,7 +34,7 @@"
                                  , " "
                                  , " import Network"
                                  , " "
                                  , "-import Foreign"
                                  , "+import Foreign (shiftR)"
                                  , " import Foreign.C"
                                  , " "
                                  , " --------------------------------------------------------------------------------" ])
                , P.flags = [ P.DebVersion "0.0.3-1~hackage2" ] } -}
    , debianize (hackage "bitmap-opengl"
                   `flag` P.DevelDep "libglu1-mesa-dev")
    , debianize (hackage "GLUT"
                   `flag` P.DevelDep "freeglut3-dev")
    , debianize (hackage "StateVar"
                   `pflag` P.DebVersion "1.0.0.0-2build1"
                   `qflag` P.DebVersion "1.0.0.0-2build3"
                   `tflag` P.DebVersion "1.0.0.0-4")
    , broken $ debianize (hackage "Tensor" `tflag` P.DebVersion "1.0.0.1-2")
    , debianize (hackage "GLURaw")
    , debianize (hackage "ObjectName" `tflag` P.DebVersion "1.0.0.0-2")
    , debianize (hackage "monad-task")
    , debianize (hackage "GLFW" `flag` P.DevelDep "libglu1-mesa-dev")
    , debianize (hackage "GLFW-b")
    , debianize (hackage "GLFW-b-demo" `flag` P.SkipPackage {- `patch` $(embedFile "patches/GLFW-b-demo.diff") -})
    , debianize (hackage "GLFW-task")
    , debianize (hackage "bindings-GLFW"
                             -- `patch` $(embedFile "patches/bindings-GLFW.diff")
                             -- `flag` P.DevelDep "libxrandr2"
                             `flag` P.DevelDep "libx11-dev"
                             `flag` P.DevelDep "libgl1-mesa-dev"
                             `flag` P.DevelDep "libxi-dev"
                             `flag` P.DevelDep "libxxf86vm-dev")
    , debianize (hackage "bindings-DSL")
--    , debianize (hackage "freetype2")
--    , debianize (hackage "FreeTypeGL") -- Does not build because (freetype2 > 0.1.2) but the lib (haskell, at least) is at 0.1.1.
    , debianize (hackage "FTGL"
                   `patch` $(embedFile "patches/FTGL.diff")
                   `flag` P.DevelDep "libftgl-dev"
                   `flag` P.DevelDep "libfreetype6-dev")
    , debianize (hackage "OpenGLRaw"
                   `flag` P.DevelDep "libgl1-mesa-dev")
    ]

--  Using pkg-config version 0.25 found on system at: /usr/bin/ 2>
--  <interactive>:2:1:
--      Failed to load interface for `Directory'
--      It is a member of the hidden package `haskell98-2.0.0.1'.
--      Use -v to see a list of the files searched for.
--
--  src/System/Plugins/Utils.hs:21:8:
--      Warning: In the use of `catch'
--               (imported from Prelude, but defined in System.IO.Error):
--               Deprecated: "Please use the new exceptions variant, Control.Exception.catch"
--
--  src/System/Plugins/Load.hs:91:35:
--      Module `GHC.Exts' does not export `addrToHValue#'
--  make: *** [build-ghc-stamp] Error 1
plugins :: P.Packages
plugins = named "plugins" $
    [ debianize (hackage "plugins")
    , debianize (hackage "plugins-auto" `patch` $(embedFile "patches/plugins-auto.diff"))
    , debianize (hackage "happstack-plugins" `patch` $(embedFile "patches/happstack-plugins.diff"))
    , debianize (git "http://github.com/clckwrks/web-plugins" [] `cd` "web-plugins")
    ]

algebra :: Release -> P.Packages
algebra release =
    let qflag = case baseRelease release of Quantal -> flag; _ -> \ p _ -> p
        pflag = case baseRelease release of Precise -> flag; _ -> \ p _ -> p
        tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
    named "algebra"
    [ debianize (hackage "data-lens")
    , debianize (hackage "data-lens-template")
    , debianize (hackage "bifunctors")
    , broken $ debianize (hackage "categories" `tflag` P.DebVersion "1.0.6-1")
    -- comonad now includes comonad-transformers and comonads-fd
    , debianize (hackage "comonad"
                   `flag` P.CabalPin "4.0.1" -- 4.2 is too new for data-lens-2.10.4
                   `flag`  P.CabalDebian [ "--conflicts=libghc-comonad-dev:libghc-comonad-transformers-dev"
                                         , "--replaces=libghc-comonad-dev:libghc-comonad-transformers-dev"
                                         , "--provides=libghc-comonad-dev:libghc-comonad-transformers-dev"
                                         , "--conflicts=libghc-comonad-dev:libghc-comonads-fd-dev"
                                         , "--replaces=libghc-comonad-dev:libghc-comonads-fd-dev"
                                         , "--provides=libghc-comonad-dev:libghc-comonads-fd-dev" ])
    , debianize (hackage "control-monad-free")
    , debianize (hackage "transformers-free")
    , debianize (hackage "contravariant" `flag` P.CabalPin "0.5") -- minimize rebuild
    , debianize (hackage "distributive" `flag` P.CabalPin "0.4.4") -- minimize rebuild
    -- This package fails to build in several different ways because it has no modules.
    -- I am just going to patch the packages that use it to require transformers >= 0.3.
    -- Specifically, distributive and lens.
    , debianize (hackage "transformers-compat" `patch` $(embedFile "patches/transformers-compat.diff"))

    -- profuctors now includes profunctor-extras
    , debianize (hackage "profunctors"
                   `flag` P.CabalDebian [ "--conflicts=libghc-profunctors-dev:libghc-profunctors-extras-dev"
                                        , "--replaces=libghc-profunctors-dev:libghc-profunctors-extras-dev"
                                        , "--provides=libghc-profunctors-dev:libghc-profunctors-extras-dev"])
    , debianize (hackage "reflection")
    , debianize (hackage "prelude-extras")
    , debianize (hackage "free")
    , debianize (hackage "keys")
    , debianize (hackage "intervals")
    , debianize (hackage "numeric-extras" `tflag` P.DebVersion "0.0.3-1")
    , debianize (hackage "lens" `flag` P.CabalPin "4.3.2") -- avoid rebuild
    , debianize (hackage "constraints")
    , debianize (hackage "lens-family-core")
    , debianize (hackage "lens-family")
    , debianize (hackage "lens-family-th")

    -- These five fail because representable-functors fails, it wasn't updated
    -- for the consolidation of comonad
    {-
    , debianize (hackage "representable-functors" {- `patch` $(embedFile "patches/representable-functors.diff") -})
    , debianize (hackage "representable-tries")
    , debianize (hackage "algebra")
    , debianize (hackage "universe" {- `patch` $(embedFile "patches/universe.diff") -})
    -}
    , debianize (hackage "adjunctions" `flag` P.CabalPin "4.1.0.1") -- waiting for contravariant>=1
    , debianize (hackage "linear")

    , debianize (hackage "semigroupoids"
                   `flag` P.CabalPin "4.0.4" -- avoid rebuild
                   `flag` P.CabalDebian [ "--conflicts=libghc-semigroupoids-dev:libghc-semigroupoid-extras-dev"
                                        , "--replaces=libghc-semigroupoids-dev:libghc-semigroupoid-extras-dev"
                                        , "--provides=libghc-semigroupoids-dev:libghc-semigroupoid-extras-dev"])
    , debianize (hackage "spine") ]

-- CB I was after units, but it requires ghc 7.8
units :: P.Packages
units = named "units"
    [ debianize (hackage "quickcheck-instances")
    , debianize (hackage "mainland-pretty")
    , debianize (hackage "srcloc")
    , debianize ((hackage "singletons") `flag` P.CabalPin "0.10.0")
    , debianize (hackage "th-desugar")
    , debianize (hackage "processing")
    , debianize (hackage "units") ]

sunroof :: Release -> P.Packages
sunroof release =
  let tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
  named "sunroof"
  [ debianize (git "http://github.com/ku-fpg/sunroof-compiler" []
                 `patch` $(embedFile "patches/sunroof-compiler.diff"))
  -- , debianize (hackage "sunroof-compiler")
  , debianize (hackage "constrained-normal")
  , debianize (hackage "set-monad")
  , debianize (hackage "data-reify")
  , debianize (hackage "Boolean")
  , debianize (hackage "vector-space")
  , debianize (hackage "NumInstances" `tflag` P.DebVersion "1.3-1")
  , debianize (hackage "MemoTrie")
  , debianize (hackage "value-supply")
  , debianize (hackage "reified-records")
  , debianize (darcs (repo </> "seclib"))
  ]
  where repo = "http://src.seereason.com/"

-- | Create a flag that tells cabal debian the package @name@ is a replacement for @orig@,
-- so that when it is installed the @orig@ package is uninstalled.  (This may be buggy, the
-- use in semigroupoids caused problems.)
replacementLibrary :: String -> String -> [String]
replacementLibrary orig name =
    ["--conflicts", deps, "--provides", deps, "--replaces", deps]
    where
      deps = intercalate "," [dev name ++ ":" ++ dev orig,
                              prof name ++ ":" ++ prof orig,
                              doc name ++ ":" ++ prof orig]
      dev x = "libghc-" ++ x ++ "-dev"
      prof x = "libghc-" ++ x ++ "-dev"
      doc x = "libghc-" ++ x ++ "-dev"

idris :: Release -> P.Packages
idris release =
    let tflag = case baseRelease release of Trusty -> flag; _ -> \ p _ -> p in
    named "idris"
        [ debianize (hackage "idris"
                       `flag` P.BuildDep "libgc-dev"
                       `flag` P.CabalDebian ["--default-package=idris"]
                       `patch` $(embedFile "patches/idris.diff"))
        , hack "vector-binary-instances"
        , hack "trifecta"
        , debianize (hackage "parsers" {- `patch` $(embedFile "patches/parsers.diff") -})
        , debianize (hackage "language-java" `flag` P.BuildDep "alex")
        , hack "cheapskate"
        , hack "annotated-wl-pprint"
        , hack "fingertree" `tflag` P.DebVersion "0.1.0.0-1"
        , hack "reducers"
        ]
    where hack = debianize . hackage

haste :: P.Packages
haste = named "haste"
  [ hack "haste-compiler" `flag` P.CabalDebian ["--default-package=haste-compiler"]
  , git' "https://github.com/RudolfVonKrugstein/haste-ffi-parser" []
  , hack "data-binary-ieee754"
  , hack "shellmate"
  , debianize (git "https://github.com/cliffordbeshers/websockets" [] `patch` $(embedFile "patches/websockets.diff"))
  , hack "io-streams"
  ]
    where hack = debianize . hackage
          git' r c = debianize $ git r c

-- agda = P.Packages (singleton "agda")
--   [ hack "agda"
--   ]
--     where hack = debianize . hackage
--           git' n r = debianize $ git n r


-- ghcjs TO DO:
--   1. fix cabal-debian so it really knows which packages ghc
--      conflicts with and which it just provides
-- x 2. Merge ghcjs and ghcjs-tools
-- * 3. Don't hard code the version numbers in the wrapper scripts (or haskell-devscripts)
--   4. Make it so we don't have to set $HOME in Setup.hs
-- * 5. Figure out how to require the version of Cabal bundled with ghc (done)
--   6. Build everything into a prefix directory instead of into /usr
--   7. Build cabal-debian with Cabal >= 1.21 - otherwise there's no GHCJS constructor.  Remove ifdefs.  Add note about where to find cabal-ghcjs.
--   8. Enable documentation packages in haskell-devscripts
--   9. Enable -prof packages(?)

ghcjs :: Release -> P.Packages
ghcjs release =
  named "ghcjs-group" $
  [ named "ghcjs" $
    [ case baseRelease release of
        Precise -> named "ghcjs-deps"
                   [ apt "sid" "c-ares"
                   , apt "sid" "gyp"
                   , apt "sid" "libv8-3.14" ]
        _ -> P.NoPackage
    , debianize (hackage "shelly")
    , debianize (hackage "text-binary")
    , debianize (hackage "enclosed-exceptions")
    , apt "sid" "nodejs"
    , debdir (git "https://github.com/ghcjs/ghcjs-prim.git" [])
                 (Git "https://github.com/ddssff/ghcjs-prim-debian" [])
    , debianize (git "https://github.com/ghcjs/haddock-internal" []
                         `flag` P.ModifyAtoms (execDebM $ rulesFragments += Text.unlines
                                                            [ "# Force the Cabal dependency to be the version provided by GHC"
                                                            , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"]))
    , debianize (hackage "lifted-async")
    -- haskell-devscripts modified to support ghcjs packaging.
    -- , darcs "http://hub.darcs.net/ddssff/haskell-devscripts" `flag` P.RelaxDep "python-minimal"
    -- Cabal library with ghcjs support.  The debs are named cabal-ghcjs
    -- so packages that require ghcjs suppport can specify this.
    -- Options used to debianize: `flag` P.DebVersion "1.21.0.0-2"
    , debdir (git "https://github.com/ddssff/cabal" [P.Branch "ghcjs"] `cd` "Cabal")
             (Git "https://github.com/ddssff/cabal-ghcjs-debian" [])
    -- Options used to generate debianization:
    --                       `flag` P.DebVersion "1.21.0.0-2"
    --                       `flag` P.CabalDebian ["--default-package=cabal-install-ghcjs",
    --                                             "--conflicts=cabal-install-ghcjs:cabal-install",
    --                                             "--replaces=cabal-install-ghcjs:cabal-install",
    --                                             "--provides=cabal-install-ghcjs:cabal-install",
    --                                             "--conflicts=cabal-install-ghcjs:haskell-cabal-install-ghcjs-utils",
    --                                             "--replaces=cabal-install-ghcjs:haskell-cabal-install-ghcjs-utils",
    --                                             "--provides=cabal-install-ghcjs:haskell-cabal-install-ghcjs-utils"]
    , debdir (git "https://github.com/ghcjs/cabal" [P.Branch "ghcjs"]
                      `cd` "cabal-install")
             (Git "https://github.com/ddssff/cabal-install-ghcjs-debian" [])
    -- Options used to debianize:
    --  let deps p0 = foldl (\ p s -> p `flag` P.BuildDep s)
    --                      p0 ["alex", "happy", "make", "patch", "autoconf",
    --                          "cpp", "git", "cabal-install-ghcjs"] in
    , debdir (git "https://github.com/ghcjs/ghcjs" []
                      -- `patch` $(embedFile "patches/ghcjs-tools.diff")
                      `flag` P.CabalDebian ["--source-package-name=ghcjs-tools"]
                      `flag` P.CabalDebian ["--default-package=ghcjs-tools"]
                      `flag` P.ModifyAtoms (execDebM $ rulesFragments += Text.unlines
                                                                         [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                         , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"])
                      `flag` P.KeepRCS)
             (Git "https://github.com/ddssff/ghcjs-tools-debian" []) -- (Dir "/home/dsf/git/ghcjs-tools-debian")
    , git "https://github.com/ddssff/ghcjs-debian" [] ]
  , named "ghcjs-libs" $
    [ ghcjs_flags (debianize (hackage "ghcjs-dom"))
    , ghcjs_flags (debianize (hackage "ghcjs-dom-hello"
                                `flag` P.CabalDebian ["--default-package=ghcjs-dom-hello"]
                                `patch` $(embedFile "patches/ghcjs-dom-hello.diff")))
    , ghcjs_flags (debianize (hackage "blaze-builder"))
    , ghcjs_flags (debianize (hackage "blaze-markup"))
    , ghcjs_flags (debianize (hackage "blaze-html"))
    , ghcjs_flags (debianize (hackage "data-default-class"))
    , ghcjs_flags (debianize (hackage "data-default-instances-base"))
    , ghcjs_flags (debianize (hackage "data-default-instances-dlist"))
    , ghcjs_flags (debianize (hackage "data-default-instances-containers"))
    , ghcjs_flags (debianize (hackage "data-default-instances-old-locale"))
    , ghcjs_flags (debianize (hackage "data-default"))
    -- We can't build the source package name from a git target
    -- (without doing IO) so we set it here explicitly.
    , ghcjs_flags (debianize (git "https://github.com/ghcjs/ghcjs-jquery" []) `srcDebName` "ghcjs-ghcjs-jquery")
    ] ]
   where srcDebName :: Packages -> String -> Packages
         srcDebName p@(Package {spec = Debianize' s xs}) name = p {spec = Debianize' s (SrcDeb (SrcPkgName name) : xs)}
         srcDebName p@(Package {spec = Debianize s}) name = p {spec = Debianize' s [SrcDeb (SrcPkgName name)]}
         srcDebName spec _ = error $ "srcDebName - invalid argument.  Can't set source package name of " ++ show spec
         ghcjs_flags NoPackage = NoPackage
         ghcjs_flags p@(Named {..}) = p {packages = ghcjs_flags packages}
         ghcjs_flags p@(Packages {..}) = p {list = map ghcjs_flags list}
         ghcjs_flags p@(Package {..}) =
             let srcName :: RetrieveMethod -> String
                 srcName (Debianize' p fs) = case fs of
                                              (SrcDeb (SrcPkgName name) : _) -> name
                                              [] -> srcName (Debianize p)
                 srcName (Debianize p) = srcName p
                 srcName (Patch x _) = srcName x
                 srcName m = "ghcjs-" <> map toLower (fromMaybe (cabName m) (dropPrefix "haskell-" (cabName m)))
                 cabName :: RetrieveMethod -> String
                 cabName (Hackage n) = n
                 cabName _ = error $ "ghcjs_flags - unsupported target type: " ++ show spec in
             p `flag` P.CabalDebian ["--hc=ghcjs"]
               `flag` P.CabalDebian ["--source-package-name=" <> srcName spec]
               `flag` P.BuildDep "libghc-cabal-ghcjs-dev"
               `flag` P.BuildDep "ghcjs"
               `flag` P.NoDoc
               `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.1)"

broken :: P.Packages -> P.Packages
broken _ = P.NoPackage

skip :: P.Packages -> P.Packages
skip _ = P.NoPackage

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str
