{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Targets.Public ( targets ) where

import Data.FileEmbed (embedFile)
import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Set as Set (empty, singleton, union)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (RetrieveMethod(..), PackageFlag(CabalPin),
                                          hackage, debianize, flag, patch, darcs, apt)
import Debian.Debianize (compat)
import Debian.Relation (BinPkgName(..))
import Debian.Debianize (installData, doExecutable, InstallFile(..))
import System.FilePath((</>))
import Targets.Common (repo, {-localRepo,-} happstackRepo)

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> P.Packages
targets _home release =
    P.Packages empty $
    [ main _home release
    , autobuilder _home
    , clckwrks _home release
    -- , authenticate _home release
    -- , happstackdotcom _home
    -- , happstack release
    , digestiveFunctors

    , algebra release
    -- , diagrams
    -- , fixme
    -- , higgsset
    -- , jsonb
    -- , glib
    -- , plugins
    -- , frisby
    -- , failing
    -- , agda
    -- , other
    ]

ghc :: String -> a -> a -> a
ghc release ghc74 ghc76 =
    case release of
      "quantal-seereason" -> ghc76
      "precise-seereason" -> ghc76
      _ -> ghc74

rel :: String -> a -> a -> a
rel release precise quantal =
    case release of
      "quantal-seereason" -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Packages -> P.PackageFlag -> P.Packages
ghc74flag p _ = p

fixme =
    P.Packages (singleton "fixme") $
    [ debianize (hackage "test-framework-smallcheck")
    , P.Package { P.name = "haskell-geni"
                , P.spec = DebDir (Darcs "http://code.haskell.org/GenI") (Darcs (repo ++ "/haskell-geni-debian"))
                , P.flags = [] }
    ]

unixutils _home =
    P.Packages (singleton "Unixutils")
    [ P.Package { P.name = "haskell-unixutils"
                , P.spec = Darcs (repo ++ "/haskell-unixutils")
                , P.flags = [] }
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra"
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help"
                , P.flags = [] } ]

autobuilder home =
    -- let repo = localRepo home in
    P.Packages (singleton "autobuilder-group") $
    [ unixutils home
    , P.Package { P.name = "autobuilder"
                , P.spec = Cd "autobuilder" (Darcs (repo </> "debian-tools"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-cabal-debian"
                , P.spec = Cd "cabal-debian" (Darcs (repo </> "debian-tools"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian"
                , P.spec = Darcs (repo ++ "/haskell-debian")
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror"
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Cd "debian-repo" (Darcs (repo </> "debian-tools"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive"
                , P.flags = [] }
    , P.Package { P.name = "haskell-process-extras"
                , P.spec = Debianize (Darcs "http://src.seereason.com/process-extras")
                , P.flags = [] }
    , P.Package { P.name = "haskell-process-progress"
                , P.spec = Cd "process-progress" (Darcs (repo </> "debian-tools-stable"))
                , P.flags = [] }
    ]

digestiveFunctors =
    P.Packages (singleton "digestive-functors")
    [ debianize (hackage "digestive-functors" `flag` P.CabalPin "0.2.1.0")  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
    -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
    , P.Package { P.name = "haskell-digestive-functors-happstack"
                , P.spec = Debianize (Patch (Hackage "digestive-functors-happstack") $(embedFile "patches/digestive-functors-happstack.diff"))
                , P.flags = [P.CabalPin "0.1.1.5", P.DebVersion "0.1.1.5-1~hackage1"] }
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Debianize (Darcs (repo ++ "/digestive-functors-hsp"))
                , P.flags = [P.DebVersion "0.5.0-1~hackage1"] } ]

main _home release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "main") $
    [ compiler release
    , platform release
    , debianize (hackage "hashtables")
    , P.Package { P.name = "bugzilla"
                , P.spec = Apt "squeeze" "bugzilla"
                , P.flags = [] }
    , P.Package { P.name = "haskell-listlike"
                , P.spec = Debianize (Hackage "ListLike")
                , P.flags = [] }
    , P.Package { P.name = "haskell-listlike-instances"
                , P.spec = Debianize (Hackage "listlike-instances")
                , P.flags = [] }
    , apt (rel release "sid" "quantal") "cpphs"
    , P.Package { P.name = "debootstrap"
                , P.spec = Apt "sid" "debootstrap"
                , P.flags = [P.UDeb "debootstrap-udeb"] }
-- Build fails due to some debianization issue
--    , apt "sid" "geneweb"
    , P.Package { P.name = "gtk2hs-buildtools"
                , P.spec = Debianize (Hackage "gtk2hs-buildtools")
                , P.flags =
                    [ P.ExtraDep "alex"
                    , P.ExtraDep "happy"
                    , P.Revision "" ] }
    -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize (hackage "aeson")
    , P.Package { P.name = "haskell-agi"
                , P.spec = Darcs "http://src.seereason.com/haskell-agi"
                , P.flags = [] }
    , debianize (hackage "ansi-terminal")
    , debianize (hackage "ansi-wl-pprint")
    , debianize (hackage "wl-pprint-text")
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = Debianize (Hackage "applicative-extras"),
                  P.flags = [P.DebVersion "0.1.8-1"] }
    , debianize (hackage "asn1-data")
    , debianize (hackage "attempt" `pflag` P.DebVersion "0.4.0-1" `qflag` P.DebVersion "0.4.0-1build2")
    , debianize (hackage "errors")
    , debianize (hackage "failure" `qflag` P.DebVersion "0.2.0.1-1build2")
    , debianize (hackage "attoparsec")
    , debianize (hackage "attoparsec-enumerator")
    , P.Package { P.name = "haskell-attoparsec-text"
                , P.spec = Debianize (Patch (Hackage "attoparsec-text") $(embedFile "patches/attoparsec-text.diff"))
                , P.flags = [P.Revision ""] }
    , debianize (hackage "attoparsec-text-enumerator")
    , debianize (hackage "base16-bytestring")
    , debianize (hackage "base-unicode-symbols")
    , debianize (hackage "bimap" `flag` P.DebVersion "0.2.4-1~hackage1")
    , debianize (hackage "data-default")
    , P.Package { P.name = "haskell-template-default"
                , P.spec = Debianize (ghc release (Hackage "template-default")
                                                      (Patch (Hackage "template-default") $(embedFile "patches/template-default.diff")))
                , P.flags = [] }
    , debianize (hackage "bitmap")
    , debianize (hackage "bitset" `flag` P.DebVersion "1.1-1~hackage1")
    , apt (rel release "sid" "quantal") "haskell-bytestring-nums"
    , debianize (hackage "bytestring-trie")
    , ghc release (P.Package { P.name = "haskell-bzlib"
                                 , P.spec = Quilt (Apt "sid" "haskell-bzlib") (Darcs "http://src.seereason.com/haskell-bzlib-quilt")
                                 , P.flags = [] })
                      (debianize (hackage "bzlib"))
    -- , debianize (hackage "cairo-pdf")
    , debianize (hackage "case-insensitive")
    , P.Package { P.name = "haskell-cabal-install"
                , P.spec = Debianize (Patch (Hackage "cabal-install") $(embedFile "patches/cabal-install.diff"))
                , P.flags = [] }
    , debianize (hackage "CC-delcont" `flag` P.DebVersion "0.2-1~hackage1")
    , apt (rel release "sid" "quantal") "haskell-cereal"
    , debianize (hackage "citeproc-hs")
    , P.Package {P.name = "haskell-hexpat",
                 P.spec = Debianize (Hackage "hexpat"),
                 P.flags = []}
    , debianize (hackage "List")
    , debianize (hackage "uuid")
    , debianize (hackage "maccatcher" `flag` P.DebVersion "2.1.5-3")
    , debianize (hackage "colour" `flag` P.DebVersion "2.3.3-1build1")
    -- , apt "sid" "haskell-configfile"
    , debianize (hackage "ConfigFile")
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer"
                , P.flags = [] }
    , debianize (hackage "cipher-aes")
    , debianize (hackage "cprng-aes")
    , debianize (hackage "crypto-random-api")
    , P.Package { P.name = "haskell-crypto"
                , P.spec = Debianize (Hackage "Crypto")
                , P.flags = [] }
    , debianize (hackage "crypto-api" `qflag` P.DebVersion "0.10.2-1build3")
    , debianize (hackage "crypto-pubkey-types")
    , debianize (hackage "cryptocipher")
    , debianize (hackage "cryptohash")
    , debianize (hackage "cpu" `qflag` P.DebVersion "0.1.1-1build1")
    , debianize (hackage "css" `flag` P.DebVersion "0.1-1~hackage1")
    , debianize (hackage "css-text" `pflag` P.DebVersion "0.1.1-3" `qflag` P.DebVersion "0.1.1-3build1")
    , apt (rel release "sid" "quantal") "haskell-curl"
    , debianize (hackage "data-accessor")
    , debianize (hackage "data-accessor-template")
    , debianize (hackage "data-default")
    , P.Package { P.name = "haskell-data-object"
                , P.spec = Debianize (Patch (Hackage "data-object") $(embedFile "patches/data-object.diff"))
                , P.flags = [] }
    , debianize (hackage "dataenc")
    , debianize (hackage "Diff")
    , apt (rel release "sid" "quantal") "haskell-digest"
    , apt (rel release "sid" "quantal") "haskell-dlist"
    -- Natty only(?)
    , debianize (hackage "double-conversion")
    , debianize (hackage "groom")
    , apt "sid" "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = ghc release (Apt "sid" "haskell-edison-api") (Debianize (Hackage "EdisonAPI"))
                , P.flags = rel release [] [P.DebVersion "1.2.1-18build2"] }
    , ghc release (apt "sid" "haskell-edison-core" `qflag` P.DebVersion "1.2.1.3-9build2")
                  (debianize (hackage "EdisonCore" `qflag` P.DebVersion "1.2.1.3-9build2"))
    , apt (rel release "sid" "quantal") "haskell-entropy"
    , debianize (hackage "enumerator" `qflag` P.DebVersion "0.4.19-1build2")
    , P.Package { P.name = "haskell-hdaemonize"
                , P.spec = Debianize (Patch (Hackage "hdaemonize") $(embedFile "patches/hdaemonize.diff"))
                , P.flags = [P.DebVersion "0.4.4.1-1~hackage1"] }
    , debianize (hackage "hsyslog")
    , debianize (hackage "erf" `flag` P.DebVersion "2.0.0.0-3")
    , ghc release (apt "sid" "haskell-feed" `pflag` P.DebVersion "0.3.8-3" `qflag` P.DebVersion "0.3.8-3build2")
                  (debianize (hackage "feed") `flag` (rel release (P.DebVersion "0.3.8-3") (P.DebVersion "0.3.8-3build2")))
    -- Darcs 2.8.1 won't build with the current version of haskeline.
{-
    , P.Package { P.name = "darcs"
                , P.spec = Patch
                             (Apt "sid" "darcs")
                             (unlines
                              [ "--- old/darcs.cabal\t2012-05-14 04:10:13.000000000 -0700"
                              , "+++ new/darcs.cabal\t2012-10-15 20:53:28.742634352 -0700"
                              , "@@ -368,10 +368,10 @@"
                              , "                      parsec       >= 2.0 && < 3.2,"
                              , "                      html         == 1.0.*,"
                              , "                      filepath     >= 1.1.0.0 && < 1.5.0.0,"
                              , "-                     haskeline    >= 0.6.3 && < 0.7,"
                              , "+                     haskeline    >= 0.6.3,"
                              , "                      hashed-storage >= 0.5.6 && < 0.6,"
                              , "                      vector       >= 0.7,"
                              , "-                     tar          == 0.3.*"
                              , "+                     tar          >= 0.3"
                              , " "
                              , "     if !os(windows)"
                              , "       build-depends: unix >= 1.0 && < 2.6"
                              , "@@ -529,10 +529,10 @@"
                              , "                    parsec       >= 2.0 && < 3.2,"
                              , "                    html         == 1.0.*,"
                              , "                    filepath     >= 1.1.0.0 && < 1.5.0.0,"
                              , "-                   haskeline    >= 0.6.3 && < 0.7,"
                              , "+                   haskeline    >= 0.6.3,"
                              , "                    hashed-storage >= 0.5.6 && < 0.6,"
                              , "                    vector       >= 0.7,"
                              , "-                   tar          == 0.3.*"
                              , "+                   tar          >= 0.3"
                              , " "
                              , "   if !os(windows)"
                              , "     build-depends: unix >= 1.0 && < 2.6"
                              , "@@ -720,7 +720,7 @@"
                              , "       build-depends: unix >= 1.0 && < 2.6"
                              , " "
                              , "     build-depends: bytestring >= 0.9.0 && < 0.10,"
                              , "-                   haskeline    >= 0.6.3 && < 0.7,"
                              , "+                   haskeline    >= 0.6.3,"
                              , "                    text       >= 0.11.0.6 && < 0.12.0.0,"
                              , "                    old-time   >= 1.0 && < 1.2,"
                              , "                    directory  >= 1.0.0.0 && < 1.2.0.0,"
                              , "@@ -729,7 +729,7 @@"
                              , "                    array      >= 0.1 && < 0.5,"
                              , "                    hashed-storage >= 0.5.6 && < 0.6,"
                              , "                    vector       >= 0.7,"
                              , "-                   tar        == 0.3.*,"
                              , "+                   tar        >= 0.3,"
                              , "                    random     == 1.0.*"
                              , " "
                              , "     if flag(mmap) && !os(windows)"
                              , "--- old/debian/control\t2012-05-15 14:30:36.000000000 -0700"
                              , "+++ new/debian/control\t2012-10-15 21:30:24.386686701 -0700"
                              , "@@ -13,7 +13,6 @@"
                              , "  libghc-hashed-storage-dev (>= 0.5.6),"
                              , "  libghc-hashed-storage-dev (<< 0.6),"
                              , "  libghc-haskeline-dev (>= 0.6.3),"
                              , "- libghc-haskeline-dev (<< 0.7),"
                              , "  libghc-html-dev (>= 1.0),"
                              , "  libghc-html-dev (<< 1.1),"
                              , "  libghc-http-dev,"
                              , "@@ -28,7 +27,6 @@"
                              , "  libghc-random-dev (<< 1.1),"
                              , "  libghc-regex-compat-dev (>= 0.95.1),"
                              , "  libghc-tar-dev (>= 0.3),"
                              , "- libghc-tar-dev (<< 0.4),"
                              , "  libghc-terminfo-dev (>= 0.3),"
                              , "  libghc-terminfo-dev (<< 0.4),"
                              , "  libghc-text-dev (>= 0.11.0.6)," ])
                , P.flags = [] }
-}
    , debianize (hackage "file-embed")
    , debianize (hackage "indents")
    , debianize (hackage "concatenative")
    , debianize (hackage "either")
    , debianize (hackage "MonadRandom")
    , P.Package { P.name = "haskell-formlets"
                , P.spec = Debianize (Patch (Hackage "formlets") $(embedFile "patches/formlets.diff"))
                , P.flags = [P.DebVersion "0.8-1~hackage1"] }
    , P.Package { P.name = "haskell-gd"
                , P.spec = Debianize (Patch (Hackage "gd") $(embedFile "patches/gd.diff"))
                , P.flags = [ P.ExtraDevDep "libgd-dev", P.ExtraDevDep "libc6-dev", P.ExtraDevDep "libfreetype6-dev" ] ++ rel release [] [P.DebVersion "3000.7.3-1build2"] }
    -- , debianize (flags [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"] (hackage "gd"))
    , debianize (hackage "cabal-macosx")
    , apt (rel release "sid" "quantal") "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , apt (rel release "sid" "quantal") "haskell-harp"
    , debianize (hackage "hashable" `flag` CabalPin "1.1.2.5")
    , debianize (hackage "hashed-storage")
    , P.Package { P.name = "haskell-haskeline"
                , P.spec = Debianize (Hackage "haskeline")
                , P.flags = [P.DebVersion "0.7.0.3-1~hackage1"] }
    , P.Package { P.name = "haskell-th-orphans"
                , P.spec = Debianize (Patch (Hackage "th-orphans") $(embedFile "patches/th-orphans.diff"))
                , P.flags = [P.DebVersion "0.6-1~hackage1"] }
    , debianize (hackage "haskell-src-meta")
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize (hackage "HaXml" `flag` P.DebVersion "1:1.22.5-2")
    , debianize (hackage "heap" `flag` P.DebVersion "1.0.0-1~hackage1")
    , P.Package { P.name = "haskell-heist"
                , P.spec = Debianize (Hackage "heist")
                , P.flags = [] }
    , debianize (hackage "xmlhtml")
    , debianize (hackage "directory-tree")
    , debianize (hackage "MonadCatchIO-transformers" `qflag` P.DebVersion "0.3.0.0-2build2")
    , debianize (hackage "MonadCatchIO-mtl")
    , debianize (hackage "haskell-lexer" `flag` P.DebVersion "1.0-3build2")
    , debianize (hackage "hinotify")
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    -- Not used, and not building.
    -- , debianize (hackage "hoauth")
    , debianize (hackage "hostname" `pflag` P.DebVersion "1.0-4build1" `qflag` P.DebVersion "1.0-4build3")
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize (hackage "HPDF")
    , debianize (hackage "hs-bibutils")
    , ghc release (apt "sid" "haskell-hsemail") (debianize (hackage "hsemail")) -- (rel release [] [P.DebVersion "1.7.1-2build2"])
    , P.Package { P.name = "haskell-hsopenssl"
                , P.spec = Debianize (Patch (Hackage "HsOpenSSL") $(embedFile "patches/HsOpenSSL.diff"))
                , P.flags = [ P.ExtraDevDep "libssl-dev", P.ExtraDevDep "libcrypto++-dev" ] }
    , debianize (hackage "HsSyck" `pflag` P.DebVersion "0.50-2" `qflag` P.DebVersion "0.50-2build2")
    , debianize (hackage "HStringTemplate")
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities"
                , P.flags = [] }
    , debianize (hackage "http-types")
    , debianize (hackage "i18n" `flag` P.DebVersion "0.3-1~hackage1")
    , debianize (hackage "iconv")
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian")
                , P.flags = [] }
    , P.Package { P.name = "haskell-instant-generics"
                , P.spec = Debianize (Hackage "instant-generics")
                , P.flags = [] }
    , debianize (hackage "generic-deriving")
    , debianize (hackage "irc")
    , debianize (hackage "ixset")
    , P.Package { P.name = "haskell-json"
                , P.spec = Darcs (repo ++ "/haskell-json")
                , P.flags = [] }
    , debianize (hackage "language-css" `flag` P.DebVersion "0.0.4.1-1~hackage1")
    , debianize (hackage "largeword")
{-  , apt "sid" "haskell-leksah"
    , apt "sid" "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , P.Package { P.name = "haskell-pointed"
                , P.spec = Debianize (Hackage "pointed")
                , P.flags = [] }
    , P.Package { P.name = "haskell-logic-tptp"
                , P.spec = Debianize (Patch (Hackage "logic-TPTP") $(embedFile "patches/logic-TPTP.diff"))
                , P.flags = [ P.ExtraDep "alex", P.ExtraDep "happy" ] }
    , apt "sid" "haskell-maybet"
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime"
                , P.flags = [] }
    , ghc release (apt "sid" "haskell-mmap") (debianize (hackage "mmap"))
    , debianize (hackage "monad-control")
    , P.Package { P.name = "haskell-monad-par-extras"
                , P.spec = Debianize (Hackage "monad-par-extras")
                , P.flags = [P.DebVersion "0.3.2-1~hackage1"] }
    , debianize (hackage "abstract-deque")
    , debianize (hackage "abstract-par")
    , debianize (hackage "monad-par")
    , debianize (hackage "IORefCAS")
    , debianize (hackage "bits-atomic")
    , debianize (hackage "monadLib")
    -- Putting this in our repo can cause problems, because when it is
    -- installed some packages can't compile unless you add package
    -- qualifiers to their imports.  For this reason, when we run the
    -- autobuilder with the --lax flag we usually get a failure from
    -- some package that builds after monads-tf got installed.  On the
    -- other hand, without monads-tf we lose this dependency chain:
    -- monads-tf -> options -> fay.
    -- , debianize (hackage "monads-tf")
    , apt (rel release "sid" "quantal") "haskell-monoid-transformer"
    , debianize (hackage "murmur-hash")
    , ghc release (apt "sid" "haskell-mwc-random") (debianize (hackage "mwc-random"))
    , P.Package { P.name = "haskell-nano-hmac"
                , P.spec = Debianize (Patch (Hackage "nano-hmac") $(embedFile "patches/nano-hmac.diff"))
                , P.flags = [P.DebVersion "0.2.0ubuntu1"] }
    , P.Package { P.name = "haskell-openid"
                , P.spec = Debianize (Patch (Hackage "openid") (ghc release $(embedFile "patches/openid.diff")
                                                                                $(embedFile "patches/openid-ghc76.diff")))
                , P.flags = [] }
    , P.Package { P.name = "haskell-operational"
                , P.spec = Debianize (Hackage "operational")
                , P.flags = [P.OmitLTDeps] }
--    , debianize (hackage "options")
    , debianize (hackage "optparse-applicative") -- [P.CabalPin "0.4.3"] -- fay requires optparse-applicative < 0.5.
    , debianize (hackage "ordered")
    , debianize (hackage "multiset" `ghc74flag` P.CabalPin "0.2.1") -- 0.2.2 requires containers >= 0.5, which comes with ghc 7.6.
    , debianize (hackage "temporary")
    , debianize (hackage "pandoc-types")
    , debianize (hackage "parse-dimacs")
    , debianize (hackage "parseargs")
    , apt (rel release "sid" "quantal") "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = Debianize (Hackage "PBKDF2"), -- DebDir (Hackage "PBKDF2") (Darcs "http://src.seereason.com/pbkdf2-debian")
                  P.flags = []}
    , apt (rel release "sid" "quantal") "haskell-pcre-light"
    , debianize (hackage "permutation" `flag` P.DebVersion "0.4.1-1~hackage1")
    , debianize (hackage "pipes")
    , debianize (hackage "polyparse")
    , debianize (hackage "primitive")
    , debianize (hackage "PropLogic")
    , debianize (hackage "PSQueue" `pflag` P.DebVersion "1.1-2" `qflag` P.DebVersion "1.1-2build2")
    , P.Package { P.name = "haskell-pwstore-purehaskell"
                , P.spec = Debianize (Patch (Hackage "pwstore-purehaskell") $(embedFile "patches/pwstore-purehaskell.diff"))
                , P.flags = [P.DebVersion "2.1-1~hackage1"] }
    , apt (rel release "sid" "quantal") "haskell-quickcheck1"
    , debianize (hackage "regex-tdfa" `pflag` P.DebVersion "1.1.8-1" `qflag` P.DebVersion "1.1.8-2build2")
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision"
                , P.flags = [] }
    , P.Package { P.name = "haskell-rjson"
                , P.spec = Debianize (Patch (Hackage "RJson") $(embedFile "patches/RJson.diff"))
                , P.flags = [] }
    , debianize (hackage "safe" `flag` P.DebVersion "0.3.3-2")
    , debianize (hackage "safecopy")
    , P.Package { P.name = "haskell-sat"
                , P.spec = Debianize (Patch (Hackage "sat") $(embedFile "patches/sat.diff"))
                , P.flags = [ P.DebVersion "1.1.1-1~hackage1" ] }
    , debianize (hackage "semigroups")
    , debianize (hackage "nats")
    , debianize (hackage "sendfile")
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra"
                , P.flags = [] }
    , P.Package { P.name = "haskell-old-exception"
                , P.spec = Debianize (Darcs (repo ++ "/old-exception"))
                , P.flags = [] }
    , apt (rel release "sid" "quantal") "haskell-sha"
    , debianize (hackage "shake")
    , debianize (hackage "shakespeare")
    , debianize (hackage "shakespeare-css")
    , P.Package { P.name = "haskell-simple-css",
                  P.spec = Debianize (Patch (Hackage "simple-css") $(embedFile "patches/simple-css.diff"))
                , P.flags = [P.DebVersion "0.0.4-1~hackage1"] }
    , debianize (hackage "SMTPClient")
    , debianize (hackage "socks")
    , debianize (hackage "split")
    , debianize (hackage "haskell-src-exts" `flag` P.ExtraDep "happy")
    , debianize (hackage "stb-image")
    , apt (rel release "sid" "quantal") "haskell-strict" -- for leksah
    , P.Package { P.name = "haskell-strict-concurrency"
                , P.spec = Debianize (ghc release (Hackage "strict-concurrency")
                                                      (Patch (Hackage "strict-concurrency")
                                                                 $(embedFile "patches/strict-concurrency.diff")))
                , P.flags = rel release [P.DebVersion "0.2.4.1-3"] [P.DebVersion "0.2.4.1-2build3"] }
    , debianize (hackage "strict-io") -- for GenI
    , debianize (hackage "smallcheck")
    , debianize (hackage "syb-with-class")
    , apt (rel release "sid" "quantal") "haskell-syb-with-class-instances-text"
    , debianize (hackage "tagged")
    , debianize (hackage "tagsoup")
    , debianize (hackage "tar")
    , debianize (hackage "terminfo" `flag` P.ExtraDep "libncurses5-dev" `flag` P.ExtraDevDep "libncurses5-dev")
    , debianize (hackage "test-framework")
    , debianize (hackage "test-framework-hunit")
    , debianize (hackage "test-framework-quickcheck")
    , debianize (hackage "test-framework-quickcheck2")
    , debianize (hackage "test-framework-th")
    , P.Package { P.name = "haskell-testpack"
                , P.spec = Debianize (Patch (Hackage "testpack") $(embedFile "patches/testpack.diff"))
                , P.flags = [] }
    , debianize (hackage "th-expand-syns")
    , debianize (hackage "th-lift")
    , debianize (hackage "transformers-base" `pflag` P.DebVersion "0.4.1-2" `qflag` P.DebVersion "0.4.1-2build2")
    , debianize (hackage "unicode-names" `flag` P.DebVersion "3.2.0.0-1~hackage1")
    , P.Package { P.name = "haskell-unicode-properties"
                , P.spec = Debianize (Patch (Hackage "unicode-properties") $(embedFile "patches/unicode-properties.diff"))
                , P.flags = [ P.DebVersion "3.2.0.0-1~hackage1" ] }
    , debianize (hackage "uniplate")
    , debianize (hackage "cmdargs")
    , P.Package { P.name = "haskell-language-javascript"
                , P.spec = Debianize (Hackage "language-javascript")
                , P.flags = [P.ExtraDep "happy"] }
    , debianize (hackage "utf8-light" `pflag` P.DebVersion "0.4.0.1-2build1" `qflag` P.DebVersion "0.4.0.1-2build3")
    , debianize (hackage "language-haskell-extract")
    , P.Package { P.name = "haskell-pretty-show", P.spec = (Debianize (Hackage "pretty-show")), P.flags = [] }
    , P.Package { P.name = "haskell-language-ecmascript"
                , P.spec = Debianize (Hackage "language-ecmascript")
                , P.flags = [] }
    , debianize (hackage "charset")
    , P.Package { P.name = "haskell-elm"
                , P.spec = Debianize (Patch (Hackage "Elm") $(embedFile "patches/elm.diff"))
                , P.flags = [] }
    , P.Package { P.name = "elm-server"
                , P.spec = Debianize (Patch (Hackage "elm-server") $(embedFile "patches/elm-server.diff"))
                , P.flags = [] }
    , debianize (hackage "hjsmin")
    , debianize (hackage "unix-compat")
    , debianize (hackage "Unixutils-shadow")
    , debianize (hackage "unordered-containers")
    , debianize (hackage "utf8-prelude" `flag` P.DebVersion "0.1.6-1~hackage1")
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt (rel release "sid" "quantal") "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize (hackage "unification-fd")
    , P.Package { P.name = "haskell-logict"
                , P.spec = Debianize (Hackage "logict")
                , P.flags = [] }
    , ghc release (apt "sid" "haskell-utility-ht") (debianize (hackage "utility-ht"))
    , debianize (hackage "vacuum")
    , debianize (hackage "vector")
    , debianize (hackage "vector-algorithms")
    , P.Package { P.name = "haskell-virthualenv"
                , P.spec = Debianize (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff"))
                , P.flags =  [] }
    , debianize (hackage "vault")
    , P.Package { P.name = "haskell-wai"
                , P.spec = Debianize (Hackage "wai")
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-encodings"
                , P.spec = Debianize (Patch (Hackage "web-encodings") $(embedFile "patches/web-encodings.diff"))
                , P.flags = [] }
    , debianize (hackage "boomerang")
    , apt (rel release "sid" "quantal") "haskell-xml"
    , debianize (hackage "cookie")
    , debianize (hackage "lifted-base")
    , debianize (hackage "system-filepath")
    , P.Package { P.name = "haskell-xml-enumerator"
                , P.spec = Debianize (Patch (Hackage "xml-enumerator") $(embedFile "patches/xml-enumerator.diff"))
                , P.flags = [] }
    , debianize (hackage "xml-types")
    , debianize (hackage "xss-sanitize" `qflag` P.DebVersion "0.3.2-1build1")
    , debianize (hackage "yaml-light" `pflag` P.DebVersion "0.1.4-2" `qflag` P.DebVersion "0.1.4-2build1")
    , ghc release (apt "sid" "haskell-zip-archive") (debianize (hackage "zip-archive"))
    , debianize (hackage "regex-pcre-builtin" `flag` P.MapDep "pcre" (BinPkgName "libpcre3-dev"))
    , ghc release (apt "sid" "hscolour" `flag` P.RelaxDep "hscolour")
                      (debianize (hackage "hscolour") `flag` P.RelaxDep "hscolour")
    , ghc release
        (P.Package { P.name = "haskell-hslogger"
                   , P.spec = Patch (Apt "sid" "hslogger") $(embedFile "patches/hslogger.diff")
                   , P.flags = [] })
        (debianize (hackage "hslogger"))
    , ghc release P.NoPackage (debianize (hackage "extensible-exceptions")) -- required for ghc-7.6, not just quantal
    , case release of
      "quantal-seereason" -> P.NoPackage -- This build hangs when performing tests
      _ -> apt "sid" "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery")
                , P.flags = [] }
    , P.Package { P.name = "jquery-goodies"
                , P.spec = Proc (Apt "sid" "jquery-goodies")
                , P.flags = [] }
-- We want to stick with 1.8 for now.
{-  , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "sid" "jqueryui")
                , P.flags = [] } -}
    , P.Package { P.name = "jcrop"
                , P.spec = DebDir (Uri "http://src.seereason.com/jcrop/Jcrop.tar.gz" "028feeb9b6415af3b7fd7d9471c92469") (Darcs (repo ++ "/jcrop-debian"))
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "sid" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-missingh"
                , P.spec = Debianize (Hackage "MissingH")
                , P.flags = [P.Revision ""] }
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring"
                , P.flags = [P.UDeb "seereason-keyring-udeb"] }
    , apt "sid" "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs"
                , P.flags = [] }
    , P.Package { P.name = "hatex"
                , P.spec = Debianize (Hackage "HaTeX")
                , P.flags = [] }
    , P.Package { P.name = "hlatex"
                , P.spec = Debianize (Hackage "hlatex")
                , P.flags = [] }
    , P.Package { P.name = "latex"
                , P.spec = Debianize (Hackage "latex")
                , P.flags = [] }
    , debianize (hackage "texmath")
    , P.Package { P.name = "derive"
                , P.spec = Debianize (Hackage "derive")
                , P.flags = [] }
    , P.Package { P.name = "frquotes"
                , P.spec = Debianize (Hackage "frquotes")
                , P.flags = [] }
    , P.Package { P.name = "foo2zjs"
                , P.spec = Apt "quantal" "foo2zjs"
                , P.flags = [] }
{-  -- Has no Setup.hs file, not sure how to build this.
    , P.Package { P.name = "hackage"
                , P.spec = Debianize (Darcs "http://code.haskell.org/hackage-server")
                , P.flags = [] } -}
    , P.Package { P.name = "stringsearch"
                , P.spec = Debianize (Hackage "stringsearch")
                , P.flags = [] }
    , P.Package { P.name = "rss"
                , P.spec = Debianize (Hackage "rss")
                , P.flags = [] }
    , P.Package { P.name = "async"
                , P.spec = Debianize (Hackage "async")
                , P.flags = [] }
    , P.Package { P.name = "csv"
                , P.spec = Debianize (Hackage "csv")
                , P.flags = (rel release [P.DebVersion "0.1.2-2"] [P.DebVersion "0.1.2-2build2"]) }
{-
    -- Needs a build dependency on libXrandr-dev and the cabal package x11.
    , P.Package { P.name = "xmobar"
                , P.spec = Debianize (Hackage "xmobar")
                , P.flags = [] }
-}
    -- Needs update for current http-conduit
    -- , debianize $ (hackage "dropbox-sdk") `patch` $(embedFile "patches/dropbox-sdk.diff")
    , debianize (darcs "haskell-hs3" (repo ++ "/hS3"))
                `flag` P.DebVersion "0.5.6-2"
                `flag` P.ModifyAtoms (doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
    , debianize (hackage "urlencoded")
    , debianize (hackage "resourcet")
    , debianize (hackage "hxt")
    , debianize (hackage "hxt-charproperties") `flag` P.DebVersion "9.1.1-2build1"
    , debianize (hackage "hxt-regex-xmlschema")
    , debianize (hackage "hxt-unicode") `flag` P.DebVersion "9.0.2-2"
    ]

compiler release =
  P.Packages (singleton "ghc") $
  ghc release [{-ghc74,-} devscripts] [{-ghc76,-} devscripts]
    where
      ghc76 = P.Package { P.name = "ghc"
                        , P.spec = Apt "experimental" "ghc"
                        , P.flags = relax }
      ghc74 = P.Package { P.name = "ghc"
                        , P.spec = Apt "sid" "ghc"
                        , P.flags = relax }
      relax = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt","python-minimal","libgmp-dev"]
      devscripts =
        P.Package { P.name = "haskell-devscripts"
                  , P.spec = ghc release (Patch (Apt "precise" "haskell-devscripts") $(embedFile "patches/haskell-devscripts-0.8.12.diff"))
                                             (Patch (Apt "experimental" "haskell-devscripts") $(embedFile "patches/haskell-devscripts-0.8.13.diff"))
                  , P.flags = ghc release [P.RelaxDep "python-minimal", P.DebVersion "0.8.12ubuntu1"] [P.RelaxDep "python-minimal"] }
      -- haskell-devscripts-0.8.13 is for ghc-7.6 only

platform release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "platform") $
    [ -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Hackage "happy") (Darcs "http://src.seereason.com/happy-debian"),
                  P.flags = [P.RelaxDep "happy", P.CabalDebian ["--executable", "happy"],
                             P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize (hackage "stm")
    , ghc release (apt "sid" "haskell-zlib") (debianize (hackage "zlib" `flag` P.ExtraDevDep "zlib1g-dev"))
    , debianize (hackage "mtl")
    , debianize (hackage "transformers" `qflag` P.DebVersion "0.3.0.0-1build3")
    , debianize (hackage "parallel")
    , debianize (hackage "syb")
    , debianize (hackage "fgl" `pflag` P.DebVersion "5.4.2.4-2" `qflag` P.DebVersion "5.4.2.4-2build2")
    , debianize (hackage "text")
    , P.Package { P.name = "alex"
                , P.spec = Debianize (Hackage "alex")
                  -- alex shouldn't rebuild just because alex seems newer, but alex does require
                  -- an installed alex binary to build
                , P.flags = [P.RelaxDep "alex",
                             P.ExtraDep "alex",
                             P.CabalDebian ["--executable", "alex"],
                             P.ModifyAtoms (\ atoms -> setL compat (Just 9) $
                                                       foldr (\ name atoms -> modL installData (insertWith union (BinPkgName "alex") (singleton (name, name))) atoms)
                                                           atoms
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
    , debianize (hackage "haskell-src"
                             `flag` P.ExtraDep "happy"
                             `pflag` P.DebVersion "1.0.1.5-1"
                             `qflag` P.DebVersion "1.0.1.5-1build2")
    -- Versions 2.4.1.1 and 2.4.1.2 change unEscapeString in a way
    -- that breaks our apps: https://github.com/haskell/network/issues/86
    , debianize (hackage "network" `flag` P.CabalPin "2.4.1.0")
    , debianize (hackage "publicsuffixlist")
    , debianize (hackage "HTTP" `pflag` P.DebVersion "1:4000.2.3-1~hackage1" `qflag` P.DebVersion "1:4000.2.3-1build2")
    , P.Package { P.name = "haskell-cgi"
                , P.spec = Debianize (Hackage "cgi")
                , P.flags = [P.DebVersion "3001.1.8.3-1~hackage1"] }
    -- This is bundled with the compiler
    -- , debianize (hackage "process")
    , debianize (hackage "random" `pflag` P.DebVersion "1.0.1.1-1" `qflag` P.DebVersion "1.0.1.1-1build2")
    , debianize (hackage "HUnit")
    , debianize (hackage "QuickCheck" `flag` P.ExtraDep "libghc-random-prof")
    , ghc release (apt "sid" "haskell-parsec") (debianize (hackage "parsec"))
    , apt (rel release "sid" "quantal") "haskell-html"
    , apt (rel release "sid" "quantal") "haskell-regex-compat"
    , apt (rel release "sid" "quantal") "haskell-regex-base"
    , ghc release (apt "sid" "haskell-regex-posix") (debianize (hackage "regex-posix"))
    , debianize (hackage "xhtml" `qflag` P.DebVersion "3000.2.1-1build2")
    ]

clckwrks _home release =
    let useDevRepo = True
        repo = if useDevRepo
               then "http://hub.darcs.net/stepcut/clckwrks-dev"
               else "http://hub.darcs.net/stepcut/clckwrks"
               -- localRepo _home ++ "/clckwrks"
    in
    P.Packages (singleton "clckwrks") $
        [ happstack release
        , authenticate _home release
        , happstackdotcom _home
        , plugins
        , P.Package { P.name = "haskell-clckwrks"
                    , P.spec = Debianize (Patch
                                          (DataFiles
                                           (DataFiles
                                            (Cd "clckwrks" (Darcs repo))
                                            (Uri "http://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                 "e211065e573ea0239d6449882c9d860d")
                                            "jstree")
                                           (Uri "https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
                                                "2ae07a68fc44f0ef8d92cce25620bd5f")
                                           "json2")
                                          $(embedFile "patches/clckwrks.diff"))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-cli"
                    , P.spec = Debianize (Cd "clckwrks-cli" (Darcs repo))
                    , P.flags = [] }
        , P.Package { P.name = "haskell-clckwrks-plugin-bugs"
                    , P.spec = Debianize (Cd "clckwrks-plugin-bugs" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-plugin-media"
                    , P.spec = Debianize (Cd "clckwrks-plugin-media" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-plugin-ircbot"
                    , P.spec = Debianize (Cd "clckwrks-plugin-ircbot" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-theme-bootstrap"
                    , P.spec = Debianize (Cd "clckwrks-theme-bootstrap" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "clckwrks-dot-com"
                    , P.spec = Debianize (Patch (Cd "clckwrks-dot-com" (Darcs repo)) $(embedFile "patches/clckwrks-dot-com.diff"))
                    , P.flags = [] }
        , P.Package { P.name = "clckwrks-theme-clckwrks"
                    , P.spec = Debianize (Cd "clckwrks-theme-clckwrks" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , debianize (hackage "jmacro")
        , debianize (hackage "hsx-jmacro")
        , debianize (hackage "monadlist")
        ] ++
    if useDevRepo
    then [ P.Package { P.name = "haskell-clckwrks-plugin-page"
                     , P.spec = Debianize (Cd "clckwrks-plugin-page" (Darcs repo))
                     , P.flags = [P.ExtraDep "haskell-hsx-utils"] } ]
    else []


happstack release =
    let privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String in
    P.Packages (singleton "happstack")
    [ plugins
    , P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization"
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-packaging"
                , P.spec = Debianize (Darcs "http://src.seereason.com/debian-packaging")
                , P.flags = [] }
    , P.Package { P.name = "haskell-seereason-base"
                , P.spec = Darcs (repo ++ "/seereason-base")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack"
                , P.spec = Debianize (Hackage "happstack")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-fay"
                , P.spec = Debianize (Hackage "happstack-fay")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-fay-ajax"
                , P.spec = Debianize (Hackage "happstack-fay-ajax")
                , P.flags = [] }
    , P.Package { P.name = "haskell-fay-hsx"
                , P.spec = Debianize (Hackage "fay-hsx")
                , P.flags = [] }
    , debianize (hackage "fay")
    , debianize (hackage "fay-base")
    , P.Package { P.name = "haskell-fay-jquery"
                , P.spec = Debianize (Git "https://github.com/faylang/fay-jquery")
                , P.flags = [] }
    , P.Package { P.name = "mastermind"
                , P.spec = Debianize (Patch (Darcs "http://hub.darcs.net/stepcut/mastermind")
                                            $(embedFile "patches/mastermind.diff"))
                , P.flags = [P.CabalDebian ["--build-dep=haskell-fay-utils", "--build-dep=haskell-fay-jquery-utils", "--build-dep=haskell-happstack-fay-utils"]] }
    , ghc release (P.Package { P.name = "haskell-happstack-data"
                             , P.spec = Debianize (Patch (Hackage "happstack-data") $(embedFile "patches/happstack-data.diff"))
                             , P.flags = [P.DebVersion "6.0.1-1build1"] }) P.NoPackage
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-hsp"
                , P.spec = Debianize (Hackage "happstack-hsp")
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    , ghc release (P.Package { P.name = "haskell-happstack-ixset"
                             , P.spec = DebDir (Cd "happstack-ixset" (Darcs happstackRepo)) (Darcs (repo ++ "/happstack-ixset-debian"))
                             , P.flags = [] }) P.NoPackage

    , P.Package { P.name = "haskell-happstack-jmacro"
                , P.spec = Debianize (Hackage "happstack-jmacro")
                , P.flags = [] }
    , debianize (hackage "jmacro-rpc-happstack")
    , debianize (hackage "jmacro-rpc")
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-server"
                , P.spec = Debianize (Hackage "happstack-server")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-server-tls"
                , P.spec = Debianize (Hackage "happstack-server-tls")
                , P.flags = [] }
    , debianize (hackage "time-compat")
    , debianize (hackage "base64-bytestring")
    , debianize (hackage "threads")
    , P.Package { P.name = "haskell-list-tries"
                , P.spec = Debianize (Hackage "list-tries")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-static-routing"
                , P.spec = Debianize (Hackage "happstack-static-routing")
                , P.flags = [P.DebVersion "0.3.1-1~hackage1"] }
    , P.Package { P.name = "haskell-happstack-util"
                , P.spec = Debianize (Patch (Hackage "happstack-util") (ghc release $(embedFile "patches/happstack-util.diff")
                                                                                        $(embedFile "patches/happstack-util-ghc76.diff")))
                , P.flags = [P.DebVersion "6.0.3-1"] }
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , P.Package { P.name = "haskell-hsp"
                , P.spec = Debianize (Patch (Hackage "hsp") $(embedFile "patches/hsp.diff"))
                , P.flags = rel release [P.DebVersion "0.7.1-1~hackage1", CabalPin "0.7.1", P.ExtraDep "haskell-hsx-utils"] [CabalPin "0.7.1", P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "haskell-hsx"
                , P.spec = Debianize (Hackage "hsx")
                , P.flags = [ P.DebVersion "0.10.4-1~hackage1"
                            -- Putting trhsx into a package called trhsx is problematic,
                            -- because the official debian package puts it into libghc-hsx-utils.
                            -- , P.CabalDebian ["--executable", "trhsx"]
                            ] }
    , P.Package { P.name = "haskell-pandoc"
                , P.spec = Debianize (Patch (Hackage "pandoc") $(embedFile "patches/pandoc.diff"))
                , P.flags = [P.RelaxDep "libghc-pandoc-doc"]
                }
    , P.Package { P.name = "markdown"
                , P.spec = Debianize (Hackage "markdown")
                , P.flags = [] }
    , P.Package { P.name = "haskell-highlighting-kate"
                , P.spec = Debianize (Hackage "highlighting-kate")
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Debianize (Hackage "web-routes")
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-boomerang"
                , P.spec = Debianize (Hackage "web-routes-boomerang")
                , P.flags = [P.DebVersion "0.27.0-1~hackage1"] }
    , P.Package { P.name = "haskell-web-routes-happstack"
                , P.spec = Debianize (Hackage "web-routes-happstack")
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-hsp"
                , P.spec = Debianize (Hackage "web-routes-hsp")
                , P.flags = [P.DebVersion "0.23.0-1~hackage1"] }
    , P.Package { P.name = "haskell-web-routes-mtl"
                , P.spec = Debianize (Hackage "web-routes-mtl")
                , P.flags = [P.DebVersion "0.20.1-1~hackage1"] }
    , P.Package { P.name = "haskell-web-routes-th"
                , P.spec = Debianize (Hackage "web-routes-th")
                , P.flags = [P.DebVersion "0.22.1-1~hackage1"] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-scaffolding"
                , P.spec = Darcs (repo ++ "/happstack-scaffolding")
                           -- Don't use Debianize here, it restores the doc package which crashes the build
                , P.flags = [] }
    , debianize (hackage "HJScript")
    , P.Package { P.name = "reform"
                , P.spec = Debianize (Cd "reform" (Darcs (darcsHub ++ "/reform")))
                , P.flags = [P.DebVersion "0.1.2-1~hackage1"] }
    , P.Package { P.name = "reform-blaze"
                , P.spec = Debianize (Cd "reform-blaze" (Darcs (darcsHub ++ "/reform")))
                , P.flags = [] }
    , P.Package { P.name = "reform-happstack"
                , P.spec = Debianize (Cd "reform-happstack" (Darcs (darcsHub ++ "/reform")))
                , P.flags = [] }
{-  , P.Package { P.name = "reform-heist"
                , P.spec = Debianize (Cd "reform-heist" (Darcs darcsHub ++ "/reform"))
                , P.flags = [] } -}
    , P.Package { P.name = "reform-hsp"
                , P.spec = Debianize (Cd "reform-hsp" (Darcs (darcsHub ++ "/reform")))
                , P.flags = [] }
    , debianize (hackage "blaze-markup")
    , apt (rel release "sid" "quantal") "haskell-blaze-builder"
    , P.Package { P.name = "haskell-blaze-builder-enumerator"
                , P.spec = Debianize (Hackage "blaze-builder-enumerator")
                , P.flags = [P.DebVersion "0.2.0.5-1~hackage1"] }
    , debianize (hackage "blaze-from-html")
    , debianize (hackage "blaze-html")
    , debianize (hackage "blaze-textual")
    , P.Package { P.name = "haskell-blaze-textual-native"
                , P.spec = Debianize (Patch
                                      (Hackage "blaze-textual-native") $(embedFile "patches/blaze-textual-native.diff"))
                , P.flags = [P.Revision ""] }
    , P.Package { P.name = "clckwrks-theme-happstack"
                , P.spec = Debianize (Patch (Cd "clckwrks-theme-happstack" (Darcs (repo ++ "/happstack-clckwrks"))) $(embedFile "patches/clckwrks-theme-happstack.diff"))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "happstack-dot-com"
                , P.spec = Debianize (Patch (Cd "happstack-dot-com" (Darcs (repo ++ "/happstack-clckwrks"))) $(embedFile "patches/happstack-dot-com.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-acid-state"
                , P.spec = Debianize (Hackage "acid-state")
                , P.flags = [] }
    ]

-- | We need new releases of all the conduit packages before we can move
-- from conduit 0.4.2 to 0.5.
conduit =
  P.Packages (singleton "conduit")
    [ P.Package { P.name = "haskell-conduit"
                , P.spec = Debianize (Hackage "conduit")
                , P.flags = [] }
    , debianize (hackage "attoparsec-conduit")
    , debianize (hackage "blaze-builder-conduit")
    , P.Package { P.name = "haskell-http-conduit"
                , P.spec = Debianize (Hackage "http-conduit")
                -- Version 1.9.0 is too new for fb 0.13.4.2.
                , P.flags = [P.CabalPin "1.8.9"] }
    , debianize (hackage "zlib-conduit")
    , P.Package { P.name = "haskell-xml-conduit"
                , P.spec = Debianize (Hackage "xml-conduit")
                , P.flags = [] }
    , debianize (hackage "mime-types")
    ]

-- | Packages pinned pending update of happstack-authenticate (in one possible build order.)
authenticate _home release =
  P.Packages (singleton "authenticate") $
    [ conduit
    , ghc release (apt "sid" "haskell-puremd5") (debianize (hackage "pureMD5"))
    , debianize (hackage "monadcryptorandom")
    , debianize (hackage "RSA")
    , P.Package { P.name = "haskell-resourcet"
                , P.spec = Debianize (Hackage "resourcet")
                , P.flags = [] }
    , debianize (hackage "void")
    -- Version 1.3.1 may be too new for tls 0.9.11
    , debianize (hackage "certificate")
    , debianize (hackage "pem")
    , debianize (hackage "zlib-bindings")
    , debianize (hackage "tls")
    , debianize (hackage "tls-extra")
    , debianize (hackage "cipher-rc4")
    , debianize (hackage "crypto-pubkey")
    , debianize (hackage "crypto-numbers")
    , P.Package { P.name = "haskell-authenticate"
                , P.spec = Debianize (Hackage "authenticate")
                , P.flags = [] }
    , P.Package { P.name = "haskell-zlib-enum"
                , P.spec = Debianize (Hackage "zlib-enum")
                , P.flags = [P.DebVersion "0.2.3-1~hackage1"] }
    , P.Package { P.name = "haskell-happstack-authenticate"
                , P.spec = Debianize (Darcs (darcsHub ++ "/happstack-authenticate"))
                , P.flags = [] }
    , digestiveFunctors
    , P.Package { P.name = "haskell-fb"
                , P.spec = Debianize (Patch (Hackage "fb") $(embedFile "patches/fb.diff"))
                , P.flags = [] }
    ]

-- ircbot needs a dependency on containers
happstackdotcom _home =
    P.Packages (singleton "happstackdotcom") $
    [ P.Package { P.name = "haskell-ircbot"
                , P.spec = Debianize (Hackage "ircbot")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc"
                , P.flags = [] } ]

-- Broken targets:
--
-- Text/JSONb/Decode.hs:48:3:
--     Not in scope: data constructor `Done'
--     Perhaps you meant `Attoparsec.Done' (imported from Data.Attoparsec.Char8)
--
-- Text/JSONb/Decode.hs:49:3:
--     Not in scope: data constructor `Fail'
--     Perhaps you meant `Attoparsec.Fail' (imported from Data.Attoparsec.Char8)
--
-- Text/JSONb/Decode.hs:50:3:
--     Not in scope: data constructor `Partial'
--     Perhaps you meant `Attoparsec.Partial' (imported from Data.Attoparsec.Char8)
jsonb = P.Packages (singleton "jsonb") $
    [ debianize (hackage "JSONb" `flag` P.DebVersion "1.0.7-1~hackage1")
    , debianize (hackage "data-object-json") ]

-- May work with these added dependencies (statevar thru openglraw)
opengl release = P.Packages (singleton "opengl") $
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    [ debianize (hackage "OpenGL")
{-  , P.Package { P.name = "haskell-vacuum-opengl"
                , P.spec =  Debianize (Patch (Hackage "vacuum-opengl"))
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
    , P.Package { P.name = "haskell-bitmap-opengl"
                , P.spec = Debianize (Hackage "bitmap-opengl")
                , P.flags = [P.ExtraDep "libglu1-mesa-dev"] }
    , debianize (hackage "GLUT")
    , debianize (hackage "StateVar" `pflag` P.DebVersion "1.0.0.0-2build1" `qflag` P.DebVersion "1.0.0.0-2build3")
    , debianize (hackage "Tensor")
    , debianize (hackage "GLURaw")
    , debianize (hackage "ObjectName")
    , debianize (hackage "OpenGLRaw" `flag` P.ExtraDep "libgl1-mesa-dev")
    ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib _release = P.Packages (singleton "glib") $
    [ debianize (hackage "glib")
                    `flag` P.ExtraDep "haskell-gtk2hs-buildtools-utils"
                    `flag` P.ExtraDep "libglib2.0-dev"
    , apt "sid" "haskell-criterion"
    , apt "sid" "haskell-ltk"
    , apt "sid" "haskell-chart"
    , apt "sid" "haskell-gio"
    , apt "sid" "haskell-gtk"
    , apt "sid" "haskell-gtksourceview2"
    , apt "sid" "haskell-pango" ]

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
plugins = P.Packages (singleton "plugins") $
    [ debianize (hackage "plugins")
    , P.Package { P.name = "haskell-plugins-auto"
                , P.spec = Debianize (Patch (Hackage "plugins-auto") $(embedFile "patches/plugins-auto.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-plugins"
                , P.spec = Debianize (Patch (Hackage "happstack-plugins") $(embedFile "patches/happstack-plugins.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-plugins"
                , P.spec = Debianize (Cd "web-plugins" (Darcs "http://hub.darcs.net/stepcut/web-plugins"))
                , P.flags = [] }
    ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = P.Packages (singleton "higgsset") $
    [ debianize (hackage "unpack-funcs")
    , debianize (hackage "HiggsSet")
    , debianize (hackage "TrieMap" `flag` P.DebVersion "4.0.1-1~hackage1") ]

frisby = P.Packages (singleton "frisby")
    [ P.Package { P.name = "haskell-frisby"
                , P.spec = DebDir (Cd "frisby" (Darcs "http://src.seereason.com/frisby")) (Darcs "http://src.seereason.com/frisby-debian")
                , P.flags = [] }
    , P.Package { P.name = "haskell-decimal"
                , P.spec = Darcs "http://src.seereason.com/decimal"
                , P.flags = [] } ]

haddock _release =
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base ==
    -- 4.3.* so we can't use "debianize "haddock" []".  I don't think
    -- we really need this, or the hackage version.  Version 2.10.0 is
    -- included with ghc 7.4.0.
    [ apt "sid" "haskell-haddock" ]

-- These have been failing for some time, and I don't think we've missed them.
failing _release =
    [ debianize (hackage "funsat")
    , apt "sid" "haskell-statistics" ]

diagrams = P.Packages (singleton "diagrams")
    [ debianize (hackage "diagrams")
    , debianize (hackage "diagrams-lib")
    , debianize (hackage "diagrams-builder")
    , debianize (hackage "diagrams-core")
    , debianize (hackage "diagrams-contrib")
    , debianize (hackage "diagrams-gtk")
    , debianize (hackage "diagrams-cairo")
    , debianize (hackage "diagrams-svg")
    , debianize (hackage "dual-tree")
    , debianize (hackage "monoid-extras")
    , debianize (hackage "newtype")
    , debianize (hackage "active")
    , debianize (hackage "Boolean")
    , debianize (hackage "MemoTrie")
    , debianize (hackage "blaze-svg")
    , debianize (hackage "force-layout")
    , debianize (hackage "cairo")
    , debianize (hackage "hint")
    , debianize (hackage "vector-space")
    , debianize (hackage "vector-space-points")
    , debianize (hackage "MonadCatchIO-mtl")
    ]

algebra release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "algebra")
    [ debianize (hackage "data-lens")
    , debianize (hackage "adjunctions")
    , debianize (hackage "algebra")
    , debianize (hackage "bifunctors")
    , debianize (hackage "categories")
    , debianize (hackage "comonad")
    , debianize (hackage "comonads-fd")
    , debianize (hackage "comonad-transformers")
    , debianize (hackage "control-monad-free")
    , debianize (hackage "transformers-free")
    , debianize (hackage "contravariant"
                             `patch` $(embedFile "patches/contravariant.diff")
                             `qflag` P.DebVersion "0.2.0.2-1build2")
    , debianize (hackage "distributive"
                             `patch` $(embedFile "patches/distributive.diff")
                             `qflag` P.DebVersion "0.2.2-1build2")
    -- This package fails to build in several different ways because it has no modules.
    -- I am just going to patch the packages that use it to require transformers >= 0.3.
    -- Specifically, distributive and lens.
    -- , debianize (hackage "transformers-compat" {-`flag` P.NoDoc-})
    , debianize (hackage "profunctor-extras")
    , debianize (hackage "semigroupoid-extras")
    , debianize (hackage "groupoids")
    , debianize (hackage "profunctors")
    , debianize (hackage "reflection")
    , debianize (hackage "free")
    , debianize (hackage "keys")
    , debianize (hackage "lens" `patch` $(embedFile "patches/lens.diff"))
    , debianize (hackage "lens-family-core")
    , debianize (hackage "lens-family")
    , debianize (hackage "lens-family-th" `patch` $(embedFile "patches/lens-family-th.diff"))
    , debianize (hackage "linear")
    , debianize (hackage "representable-functors")
    , debianize (hackage "representable-tries")
    , debianize (hackage "semigroupoids")
    , debianize (hackage "spine") ]

-- Debian package has versioned dependencies on binary, but the
-- virtual binary package provided with ghc 7.4 (0.5.1.0) is
-- newer than the version of binary in hackage (0.5.0.2.)  This
-- means we try to pull in bogus debs for libghc-binary-* and
-- dependency problems ensue.
agda _release =
    [ apt "sid" "agda"
    , apt "sid" "agda-bin"
    , apt "sid" "agda-stdlib" ]

other _release =
    [ apt "sid" "darcs"
    , debianize (hackage "aeson-native" `patch` $(embedFile "patches/aeson-native.diff"))
    , apt "sid" "haskell-binary-shared" -- for leksah
    , debianize (hackage "cairo" `flag` P.ExtraDep "haskell-gtk2hs-buildtools-utils") -- for leksah
    , debianize (hackage "cabal-dev") -- build-env for cabal
    , debianize (hackage "gnuplot" `flag` P.DebVersion "0.4.2-1~hackage1")
    , apt "sid" "bash-completion"
    ]

{-
-- |Here is a program to generate a list of all the packages in sid that have ghc for a build dependency.

#!/usr/bin/env runghc

import Data.Maybe (catMaybes)
import Debian.Control (Control'(unControl), parseControlFromFile, fieldValue)
import Debian.Relation (Relation(Rel), parseRelations)

main =
    parseControlFromFile "/home/dsf/.autobuilder/dists/sid/aptEnv/var/lib/apt/lists/mirrors.usc.edu_pub_linux_distributions_debian_dists_sid_main_source_Sources" >>=
    either (error "parse") (mapM_ putStrLn . catMaybes . map checkPackage . unControl)
    where
      checkPackage p =
          if any (\ (Rel name _ _) -> name == "ghc") rels then fieldValue "Package" p else Nothing
          where
            rels = either (const []) concat $
                       maybe (Left undefined) parseRelations $
                           fieldValue "Build-Depends" p
-}
