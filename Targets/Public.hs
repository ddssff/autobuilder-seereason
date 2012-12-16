{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Targets.Public ( targets ) where

import qualified Data.ByteString as B
import Data.Char (toLower, chr)
import Data.FileEmbed (embedFile)
import Data.Set (empty, singleton)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Debian.Relation (BinPkgName(..))
import System.FilePath((</>))
import Targets.Common (repo, localRepo, happstackRepo)

patchTag = "http://patch-tag.com/r/stepcut"
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
--    , diagrams

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

quantal :: String -> a -> a -> a
quantal release p q =
    case release of
      "quantal-seereason" -> q
      _ -> p

fixme =
    P.Packages (singleton "fixme") $
    [ debianize "test-framework-smallcheck" []
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
                , P.spec = Cd "process-progress" (Darcs (repo </> "debian-tools"))
                , P.flags = [] }
    ]

digestiveFunctors =
    P.Packages (singleton "digestive-functors")
    [ debianize "digestive-functors" [P.CabalPin "0.2.1.0"]  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
    -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
    , P.Package { P.name = "haskell-digestive-functors-happstack"
                , P.spec = Debianize (Hackage "digestive-functors-happstack")
                , P.flags = [P.CabalPin "0.1.1.5", P.DebVersion "0.1.1.5-1~hackage1"] }
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Debianize (Darcs (repo ++ "/digestive-functors-hsp"))
                , P.flags = [P.DebVersion "0.5.0-1~hackage1"] } ]

main _home release =
    P.Packages (singleton "main") $
    [ ghc release
    , platform release
    , debianize "hashtables" []
    , P.Package { P.name = "bugzilla"
                , P.spec = Apt "squeeze" "bugzilla"
                , P.flags = [] }
    , P.Package { P.name = "haskell-listlike"
                , P.spec = Debianize (Hackage "ListLike")
                , P.flags = [] }
    , P.Package { P.name = "haskell-listlike-instances"
                , P.spec = Debianize (Hackage "listlike-instances")
                , P.flags = [] }
    , apt (quantal release "sid" "quantal") "cpphs" []
    , P.Package { P.name = "debootstrap"
                , P.spec = Apt "sid" "debootstrap"
                , P.flags = [P.UDeb "debootstrap-udeb"] }
-- Build fails due to some debianization issue
--    , apt "sid" "geneweb" []
    , P.Package { P.name = "gtk2hs-buildtools"
                , P.spec = Debianize (Hackage "gtk2hs-buildtools")
                , P.flags =
                    [ P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"
                    , P.ExtraDep "alex"
                    , P.ExtraDep "happy"
                    , P.Revision "" ] }
    -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize "aeson" []
    , P.Package { P.name = "haskell-agi"
                , P.spec = Darcs "http://src.seereason.com/haskell-agi"
                , P.flags = [] }
    , debianize "ansi-terminal" []
    , debianize "ansi-wl-pprint" (quantal release [P.DebVersion "0.6.4-1"] [P.DebVersion "0.6.4-1build2"])
    , debianize "wl-pprint-text" []
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = Debianize (Hackage "applicative-extras"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.DebVersion "0.1.8-1"] }
    , debianize "asn1-data" []
    , debianize "attempt" (quantal release [P.DebVersion "0.4.0-1"] [P.DebVersion "0.4.0-1build2"])
    , debianize "errors" []
    , debianize "failure" (quantal release [] [P.DebVersion "0.2.0.1-1build2"])
    , debianize "attoparsec" []
    , debianize "attoparsec-enumerator" []
    , P.Package { P.name = "haskell-attoparsec-text"
                , P.spec = Debianize (Patch (Hackage "attoparsec-text") $(embedFile "patches/attoparsec-text.diff"))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , debianize "attoparsec-text-enumerator" []
    , debianize "base-unicode-symbols" []
    , debianize "bimap" [P.DebVersion "0.2.4-1~hackage1"]
    , debianize "data-default" []
    , debianize "template-default" []
    , debianize "bitmap" []
    , debianize "bitset" [P.DebVersion "1.1-1~hackage1"]
    , apt (quantal release "sid" "quantal") "haskell-bytestring-nums" []
    , debianize "bytestring-trie" []
    , quantal release (P.Package { P.name = "haskell-bzlib"
                                 , P.spec = Quilt (Apt "sid" "haskell-bzlib") (Darcs "http://src.seereason.com/haskell-bzlib-quilt")
                                 , P.flags = [] })
                      (debianize "bzlib" [])
    -- , debianize "cairo-pdf" []
    , debianize "case-insensitive" []
    , debianize "CC-delcont" [P.DebVersion "0.2-1~hackage1"]
    , apt (quantal release "sid" "quantal") "haskell-cereal" []
    , debianize "citeproc-hs" []
    , P.Package {P.name = "haskell-hexpat",
                 P.spec = Debianize (Hackage "hexpat"),
                 P.flags = []}
    , debianize "List" []
    , debianize "uuid" []
    , debianize "maccatcher" [P.DebVersion "2.1.5-3"]
    , debianize "colour" (quantal release [P.DebVersion "2.3.3-1build1"] [P.DebVersion "2.3.3-1build3"])
    -- , apt "sid" "haskell-configfile" []
    , debianize "ConfigFile" []
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer"
                , P.flags = [] }
    , debianize "cipher-aes" []
    , debianize "cprng-aes" []
    , debianize "crypto-random-api" []
    , P.Package { P.name = "haskell-crypto"
                , P.spec = Debianize (Hackage "Crypto")
                , P.flags = [] }
{-  , patch (debianize "Crypto" [ P.DebVersion "4.2.4-1"])
                    (unlines
                      [ "--- old/Data/Digest/SHA2.hs\t2012-01-03 23:14:43.000000000 -0800"
                      , "+++ new/Data/Digest/SHA2.hs\t2012-01-03 23:23:31.786481686 -0800"
                      , "@@ -106,7 +106,7 @@"
                      , " data Hash384 = Hash384 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 deriving (Eq, Ord)"
                      , " data Hash224 = Hash224 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 deriving (Eq, Ord)"
                      , " "
                      , "-instance (Integral a) => Show (Hash8 a) where"
                      , "+instance (Integral a, Show a) => Show (Hash8 a) where"
                      , "  showsPrec _ (Hash8 a b c d e f g h) ="
                      , "   (showHex a) . (' ':) ."
                      , "   (showHex b) . (' ':) ."
                      , "@@ -146,7 +146,7 @@"
                      , "      where"
                      , "       bs = bitSize (head r)"
                      , " "
                      , "-instance (Integral h, Bits h) => Hash (Hash8 h) where"
                      , "+instance (Integral h, Bits h, Show h) => Hash (Hash8 h) where"
                      , "   toOctets (Hash8 x0 x1 x2 x3 x4 x5 x6 x7) = bitsToOctets =<< [x0, x1, x2, x3, x4, x5, x6, x7]"
                      , " "
                      , " instance Hash Hash384 where" ]) -}
    , debianize "crypto-api" (quantal release [] [P.DebVersion "0.10.2-1build3"])
    , debianize "crypto-pubkey-types" []
    , debianize "cryptocipher" []
    , debianize "cryptohash" []
    , debianize "cpu" (quantal release [] [P.DebVersion "0.1.1-1build1"])
    , debianize "css" [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" (quantal release [P.DebVersion "0.1.1-3"] [P.DebVersion "0.1.1-3build1"])
    , apt (quantal release "sid" "quantal") "haskell-curl" []
    , debianize "data-accessor" []
    , debianize "data-accessor-template" []
    , debianize "data-default" []
    , patch (debianize "data-object" []) $(embedFile "patches/data-object.diff")
    , debianize "dataenc" []
    , debianize "Diff" []
    , apt (quantal release "sid" "quantal") "haskell-digest" []
    , apt (quantal release "sid" "quantal") "haskell-dlist" []
    -- Natty only(?)
    , debianize "double-conversion" []
    , debianize "groom" []
    , apt "sid" "haskell-dummy" []
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , quantal release (P.Package { P.name = "haskell-edison-api"
                                 , P.spec = Apt "sid" "haskell-edison-api"
                                 , P.flags = [] })
                      (debianize "EdisonAPI" [P.DebVersion "1.2.1-18build2"])
    , quantal release (apt "sid" "haskell-edison-core" []) (debianize "EdisonCore" [P.DebVersion "1.2.1.3-9build2"])
    , apt (quantal release "sid" "quantal") "haskell-entropy" []
    , debianize "enumerator" (quantal release [] [P.DebVersion "0.4.19-1build2"])
    , P.Package { P.name = "haskell-hdaemonize"
                , P.spec = Debianize (Patch (Hackage "hdaemonize") $(embedFile "patches/hdaemonize.diff"))
                , P.flags = [P.DebVersion "0.4.4.1-1~hackage1"] }
    , debianize "hsyslog" []
    , debianize "erf" [P.DebVersion "2.0.0.0-3"]
    , quantal release (apt "sid" "haskell-feed" []) (debianize "feed" [P.DebVersion "0.3.8-3build2"])
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
    , debianize "file-embed" []
    , P.Package { P.name = "haskell-formlets"
                , P.spec = Debianize (Patch (Hackage "formlets") $(embedFile "patches/formlets.diff"))
                , P.flags = [P.DebVersion "0.8-1~hackage1"] }
    , patch (debianize "gd"  [ P.ExtraDevDep "libgd-dev", P.ExtraDevDep "libm-dev", P.ExtraDevDep "libfreetype-dev" ])
                    $(embedFile "patches/gd.diff")
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , debianize "cabal-macosx" []
    , apt (quantal release "sid" "quantal") "haskell-ghc-paths" [] -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , apt (quantal release "sid" "quantal") "haskell-harp" []
    , debianize "hashable" [CabalPin "1.1.2.5"]
    , debianize "hashed-storage" []
    , P.Package { P.name = "haskell-haskeline"
                , P.spec = Debianize (Hackage "haskeline")
                , P.flags = [P.DebVersion "0.7.0.3-1~hackage1"] }
    , P.Package { P.name = "haskell-th-orphans"
                , P.spec = Debianize (Patch (Hackage "th-orphans") $(embedFile "patches/th-orphans.diff"))
                , P.flags = [P.DebVersion "0.6-1~hackage1"] }
    , debianize "haskell-src-meta" []
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize "HaXml" [P.DebVersion "1:1.22.5-2"]
    , debianize "heap" [P.DebVersion "1.0.0-1~hackage1"]
    , P.Package { P.name = "haskell-heist"
                , P.spec = Debianize (Hackage "heist")
                , P.flags = [] }
    , debianize "xmlhtml" []
    , debianize "directory-tree" []
    , debianize "MonadCatchIO-transformers" (quantal release [] [P.DebVersion "0.3.0.0-2build2"])
    , debianize "MonadCatchIO-mtl" []
    , debianize "haskell-lexer" []
    , debianize "hinotify" []
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    -- Not used, and not building.
    -- , debianize "hoauth" []
    , debianize "hostname" [P.DebVersion (quantal release "1.0-4build1" "1.0-4build3")]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" []
    , debianize "hs-bibutils" []
    , apt "sid" "haskell-hsemail" (quantal release [] [P.DebVersion "1.7.1-2build2"])
    , patch (debianize "HsOpenSSL" [ P.ExtraDevDep "libssl-dev", P.ExtraDevDep "libcrypto++-dev" ])
                    $(embedFile "patches/HsOpenSSL.diff")
    , debianize "HsSyck" (quantal release [P.DebVersion "0.50-2"] [P.DebVersion "0.50-2build2"])
    , debianize "HStringTemplate" []
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities"
                , P.flags = [] }
    , debianize "http-types" []
    , debianize "i18n" [P.DebVersion "0.3-1~hackage1"]
    , debianize "iconv" []
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian")
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "generic-deriving" []
    , debianize "irc" []
    , debianize "ixset" []
    , P.Package { P.name = "haskell-json"
                , P.spec = Darcs (repo ++ "/haskell-json")
                , P.flags = [] }
    , debianize "language-css" [P.DebVersion "0.0.4.1-1~hackage1"]
    , debianize "largeword" []
{-  , apt "sid" "haskell-leksah" []
    , apt "sid" "haskell-leksah-server" [] -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , P.Package { P.name = "haskell-pointed"
                , P.spec = Debianize (Hackage "pointed")
                , P.flags = [] }
    , patch (debianize "logic-TPTP" [ P.ExtraDep "alex", P.ExtraDep "happy" ])
                           $(embedFile "patches/logic-TPTP.diff")
    , apt "sid" "haskell-maybet" []
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime"
                , P.flags = [] }
    , quantal release (apt "sid" "haskell-mmap" []) (debianize "mmap" [])
    , debianize "monad-control" []
    , P.Package { P.name = "haskell-monad-par-extras"
                , P.spec = Debianize (Hackage "monad-par-extras")
                , P.flags = [P.DebVersion "0.3.2-1~hackage1"] }
    , debianize "abstract-deque" []
    , debianize "abstract-par" []
    , debianize "monad-par" []
    , debianize "IORefCAS" []
    , debianize "bits-atomic" []
    , debianize "monadLib" []
    -- Putting this in our repo can cause problems, because when it is
    -- installed some packages can't compile unless you add package
    -- qualifiers to their imports.  For this reason, when we run the
    -- autobuilder with the --lax flag we usually get a failure from
    -- some package that builds after monads-tf got installed.  On the
    -- other hand, without monads-tf we lose this dependency chain:
    -- monads-tf -> options -> fay.
    -- , debianize "monads-tf" []
    , apt (quantal release "sid" "quantal") "haskell-monoid-transformer" []
    , debianize "murmur-hash" []
    , quantal release (apt "sid" "haskell-mwc-random" []) (debianize "mwc-random" [])
    , patch (debianize "nano-hmac" (quantal release [P.DebVersion "0.2.0ubuntu1"] []))
                            $(embedFile "patches/nano-hmac.diff")
    , patch (debianize "openid" []) $(embedFile "patches/openid.diff")
    , P.Package { P.name = "haskell-operational"
                , P.spec = Debianize (Hackage "operational")
                , P.flags = [P.OmitLTDeps] }
--    , debianize "options" []
    , debianize "optparse-applicative" []
    , debianize "ordered" []
    , debianize "multiset" [P.CabalPin "0.2.1"] -- 0.2.2 requires containers >= 0.5, which comes with ghc 7.6.
    , debianize "temporary" []
    , debianize "pandoc-types" (quantal release [P.DebVersion "1.9.1-1"] [P.DebVersion "1.9.1-1build2"])
    , debianize "parse-dimacs" []
    , debianize "parseargs" []
    , apt (quantal release "sid" "quantal") "haskell-parsec2" []
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2") (Darcs "http://src.seereason.com/pbkdf2-debian"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt (quantal release "sid" "quantal") "haskell-pcre-light" []
    , debianize "permutation" [P.DebVersion "0.4.1-1~hackage1"]
    , debianize "pipes" []
    , debianize "polyparse" []
    , debianize "primitive" []
    , debianize "PropLogic" []
{-  , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs (repo ++ "/haskell-proplogic-debian"))
                , P.flags = [] } -}
    {- , P.Package { P.name = "haskell-propositional-classes"
                , P.spec = Darcs (repo ++ "/propositional-classes")
                , P.flags = [] } -}
    , debianize "PSQueue" (quantal release [P.DebVersion "1.1-2"] [P.DebVersion "1.1-2build2"])
    , P.Package { P.name = "haskell-pwstore-purehaskell"
                , P.spec = Debianize (Patch (Hackage "pwstore-purehaskell") $(embedFile "patches/pwstore-purehaskell.diff"))
                , P.flags = [P.DebVersion "2.1-1~hackage1"] }
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1.
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    , apt (quantal release "sid" "quantal") "haskell-quickcheck1" []
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- , apt "sid" "haskell-regex-tdfa" []
    , debianize "regex-tdfa" (quantal release [P.DebVersion "1.1.8-1"] [P.DebVersion "1.1.8-2build2"])
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision"
                , P.flags = [] }
    , debianize "RJson" []
    , debianize "safe" [P.DebVersion "0.3.3-2"]
    , debianize "safecopy" []
    , patch (debianize "sat" [ P.DebVersion "1.1.1-1~hackage1" ]) $(embedFile "patches/sat.diff")
    , debianize "semigroups" []
    , debianize "sendfile" []
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra"
                , P.flags = [] }
    , apt (quantal release "sid" "quantal") "haskell-sha" []
    , debianize "shake" []
    , debianize "shakespeare" []
    , debianize "shakespeare-css" []
    , P.Package { P.name = "haskell-simple-css",
                  P.spec = Patch (Debianize (Hackage "simple-css")) $(embedFile "patches/simple-css.diff")
                , P.flags = [P.DebVersion "0.0.4-1~hackage1"] }
    , debianize "SMTPClient" [P.DebVersion "1.0.4-3"]
    , debianize "socks" []
    , debianize "split" []
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , debianize "haskell-src-exts" [P.ExtraDep "happy"]
    , debianize "stb-image" []
    , apt (quantal release "sid" "quantal") "haskell-strict" [] -- for leksah
    , debianize "strict-concurrency" [P.DebVersion "0.2.4.1-3"]
    , debianize "strict-io" [] -- for GenI
    , debianize "smallcheck" []
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb-with-class" []
    , apt (quantal release "sid" "quantal") "haskell-syb-with-class-instances-text" []
    , debianize "tagged" []
    , debianize "tagsoup" []
    , debianize "tar" []
    , debianize "terminfo" [P.ExtraDep "libncurses5-dev", P.ExtraDevDep "libncurses5-dev"]
    , debianize "test-framework" []
    , debianize "test-framework-hunit" []
    , debianize "test-framework-quickcheck" []
    , debianize "test-framework-quickcheck2" []
    , debianize "test-framework-th" []
    , P.Package { P.name = "haskell-testpack"
                , P.spec = Debianize (Patch (Hackage "testpack") $(embedFile "patches/testpack.diff"))
                , P.flags = [] }
    , debianize "th-expand-syns" []
    , debianize "th-lift" []
    , debianize "transformers-base" (quantal release [P.DebVersion "0.4.1-2"] [P.DebVersion "0.4.1-2build2"])
    , debianize "unicode-names" [P.DebVersion "3.2.0.0-1~hackage1"]
    , patch (debianize "unicode-properties" [ P.DebVersion "3.2.0.0-1~hackage1" ]) $(embedFile "patches/unicode-properties.diff")
    , debianize "uniplate" []
    -- , apt "sid" "haskell-unix-compat" []
    , debianize "cmdargs" []
    , P.Package { P.name = "haskell-language-javascript",
                  P.spec = Patch (Debianize (Hackage "language-javascript")) $(embedFile "patches/language-javascript.diff"),
                   P.flags = []
                 }
    , debianize "utf8-light" (quantal release [P.DebVersion "0.4.0.1-2build1"] [P.DebVersion "0.4.0.1-2build3"])
    , debianize "language-haskell-extract" []
    , P.Package { P.name = "haskell-pretty-show", P.spec = (Debianize (Hackage "pretty-show")), P.flags = [] }
    , P.Package { P.name = "haskell-language-ecmascript"
                , P.spec = Debianize (Hackage "language-ecmascript")
                , P.flags = [] }
    , P.Package { P.name = "haskell-elm"
                , P.spec = Debianize (Hackage "Elm")
                , P.flags = [] }
    , P.Package { P.name = "elm-server"
                , P.spec = Debianize (Patch (Hackage "elm-server") $(embedFile "patches/elm-server.diff"))
                , P.flags = [] }
    , debianize "hjsmin" []
    , debianize "unix-compat" []
    , debianize "Unixutils-shadow" []
    , debianize "unordered-containers" []
    , debianize "utf8-prelude" [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt (quantal release "sid" "quantal") "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize "unification-fd" []
    , P.Package { P.name = "haskell-logict"
                , P.spec = Debianize (Hackage "logict")
                , P.flags = [P.DebVersion "0.5.0.2-1~hackage1"] }
    , quantal release (apt "sid" "haskell-utility-ht" []) (debianize "utility-ht" [])
    , debianize "vacuum" []
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt "sid" "haskell-vector" []
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" []
    , debianize "vector-algorithms" []
    , patch (debianize "virthualenv" []) $(embedFile "patches/virthualenv.diff")
    , debianize "vault" []
    , P.Package { P.name = "haskell-wai"
                , P.spec = Debianize (Hackage "wai")
                , P.flags = [] }
    , patch (debianize "web-encodings" []) $(embedFile "patches/web-encodings.diff")
    , debianize "boomerang" []
    , apt (quantal release "sid" "quantal") "haskell-xml" []
    , debianize "cookie" []
    , debianize "lifted-base" []
    , debianize "system-filepath" []
    , patch (debianize "xml-enumerator" []) $(embedFile "patches/xml-enumerator.diff")
    , debianize "xml-types" []
    , debianize "xss-sanitize" (quantal release [] [P.DebVersion "0.3.2-1build1"])
    , debianize "yaml-light" (quantal release [P.DebVersion "0.1.4-2"] [P.DebVersion "0.1.4-2build1"])
    , quantal release (apt "sid" "haskell-zip-archive" []) (debianize "zip-archive" [])
    , debianize "regex-pcre-builtin" [P.MapDep "pcre" (BinPkgName "libpcre3-dev") {-, P.ExtraDevDep "libpcre3-dev"-}]
    , apt (quantal release "sid" "quantal") "hscolour" [P.RelaxDep "hscolour"]
    , quantal release
        (P.Package { P.name = "haskell-hslogger"
                   , P.spec = Patch (Apt "sid" "hslogger") $(embedFile "patches/hslogger.diff")
                   , P.flags = [] })
        (debianize "hslogger" [])
    , quantal release P.NoPackage (debianize "extensible-exceptions" []) -- required for ghc-7.6, not just quantal
    , case release of
      "quantal-seereason" -> P.NoPackage -- This build hangs when performing tests
      _ -> apt "sid" "html-xml-utils" []
-- No longer in sid
--  , P.Package { P.name = "node-uglify"
--              , P.spec = Apt "sid" "node-uglify"
--              , P.flags = [] }
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery")
                , P.flags = [] }
    , P.Package { P.name = "jquery-goodies"
                , P.spec = Proc (Apt "sid" "jquery-goodies")
                , P.flags = [] }
    , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "sid" "jqueryui")
                , P.flags = [] }
    , P.Package { P.name = "jcrop"
                , P.spec = DebDir (Uri "http://src.seereason.com/jcrop/Jcrop.tar.gz" "028feeb9b6415af3b7fd7d9471c92469") (Darcs (repo ++ "/jcrop-debian"))
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "sid" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-missingh"
                , P.spec = Debianize (Hackage "MissingH")
                , P.flags = [P.Maintainer "SeeReason Autobuilder <logic@seereason.com>", P.Revision ""] }
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring"
                , P.flags = [P.UDeb "seereason-keyring-udeb"] }
    , apt "sid" "tinymce" []
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
    , debianize "texmath" []
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
                , P.flags = (quantal release [P.DebVersion "0.1.2-2"] [P.DebVersion "0.1.2-2build2"]) }
{-
    -- Fails in lucid due to build deps
    , P.Package { P.name = "jbigkit"
                , P.spec = Apt "sid" "jbigkit"
                , P.flags = [] }
-}
    ]

ghc release =
  P.Packages (singleton "ghc") $
  quantal release [devscripts] [ghc76, devscripts]
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
                  , P.spec = quantal release (Patch (Apt "precise" "haskell-devscripts") $(embedFile "patches/haskell-devscripts-0.8.12.diff")) -- Apt "sid" "haskell-devscripts"
                                             (Apt "experimental" "haskell-devscripts")
                  , P.flags = quantal release [P.RelaxDep "python-minimal", P.DebVersion "0.8.12ubuntu1"] [P.RelaxDep "python-minimal"] }
      -- haskell-devscripts-0.8.13 is for ghc-7.6 only

platform release =
    P.Packages (singleton "platform") $
    [ -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Hackage "happy") (Darcs "http://src.seereason.com/happy-debian"),
                  P.flags = [P.RelaxDep "happy",
                             P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    {- , debianize "happy" [] -}
    -- There are newer versions in hackage, but it will trigger a huge
    -- build.  Also, ghc-7.4.1-2 conflicts with libghc-directory-dev, so we actually
    -- can't build this unless we modify the ghc package first.
    -- debianize "directory" []
    , debianize "stm" []
    , quantal release (apt "sid" "haskell-zlib" [])
                      (debianize "zlib" [])
    , debianize "mtl" []
    , debianize "transformers" (quantal release [] [P.DebVersion "0.3.0.0-1build3" {- version in ubuntu repo -}])
    , debianize "parallel" []
    , debianize "syb" []
    , debianize "fgl" (quantal release [P.DebVersion "5.4.2.4-2"] [P.DebVersion "5.4.2.4-2build2"])
    , debianize "text" []
    , P.Package { P.name = "alex"
                , P.spec = Apt "sid" "alex"
                , P.flags = [P.RelaxDep "alex"] }
    , opengl release
    -- , haddock release
    , debianize "haskell-src" (quantal release [ P.ExtraDep "happy", P.DebVersion "1.0.1.5-1" ] [ P.ExtraDep "happy", P.DebVersion "1.0.1.5-1build2" ])
    , debianize "network" []
    , debianize "HTTP" (quantal release [P.DebVersion "1:4000.2.3-1~hackage1"] [P.DebVersion "1:4000.2.3-1build2"])
    , P.Package { P.name = "haskell-cgi"
                , P.spec = Debianize (Hackage "cgi")
                -- , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian")
                , P.flags = [P.DebVersion "3001.1.8.3-1~hackage1"] }
    -- This is bundled with the compiler
    -- , debianize "process" []
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" (quantal release [P.DebVersion "1.0.1.1-1"] [P.DebVersion "1.0.1.1-1build2"])
    , debianize "HUnit" []
    , debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]
    , quantal release (apt "sid" "haskell-parsec" []) (debianize "parsec" [])
    , apt (quantal release "sid" "quantal") "haskell-html" []
    , apt (quantal release "sid" "quantal") "haskell-regex-compat" []
    , apt (quantal release "sid" "quantal") "haskell-regex-base" []
    , quantal release (apt "sid" "haskell-regex-posix" []) (debianize "regex-posix" [])
    , debianize "xhtml" (quantal release [] [P.DebVersion "3000.2.1-1build2"])
    ]

clckwrks _home release =
    let repo = "http://hub.darcs.net/stepcut/clckwrks" {- localRepo _home ++ "/clckwrks" -} in
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
                    , P.spec = Debianize (Patch
                                          (Cd "clckwrks-dot-com" (Darcs repo)) $(embedFile "patches/clckwrks-dot-com.diff"))
                    , P.flags = [] }
        , P.Package { P.name = "clckwrks-theme-clckwrks"
                    , P.spec = Debianize (Cd "clckwrks-theme-clckwrks" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , debianize "jmacro" []
        , debianize "hsx-jmacro" []
        , debianize "monadlist" []
        , debianize "fay" []
        ]

happstack release =
    let privateRepo = "ssh://upload@src.seereason.com/srv/darcs" in
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
                , P.spec = Debianize (Patch (Hackage "happstack-fay") $(embedFile "patches/happstack-fay.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-fay-jquery"
                , P.spec = Debianize (Git "https://github.com/faylang/fay-jquery.git")
                , P.flags = [] }
    , P.Package { P.name = "mastermind"
                , P.spec = Debianize (Patch
                                      (Darcs "http://hub.darcs.net/stepcut/mastermind") $(embedFile "patches/mastermind.diff"))
                , P.flags = [P.CabalDebian ["--build-dep=haskell-fay-utils", "--build-dep=haskell-fay-jquery-utils", "--build-dep=haskell-happstack-fay-utils"]] }
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = Debianize (Patch (Hackage "happstack-data") $(embedFile "patches/happstack-data.diff"))
                , P.flags = [P.DebVersion "6.0.1-1build1"] }
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra")
                , P.flags = [] }
{- retired
    , P.Package { P.name = "haskell-happstack-facebook"
                , P.spec = Darcs (repo ++ "/happstack-facebook")
                , P.flags = [] }
-}
    , P.Package { P.name = "haskell-happstack-hsp"
                , P.spec = Debianize (Hackage "happstack-hsp")
                -- trhsx
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    -- Version 6.1.0, which is just a wrapper around the non-happstack
    -- ixset package, has not yet been uploaded to hackage.
    -- , debianize "happstack-ixset" []
    , P.Package { P.name = "haskell-happstack-ixset"
                , P.spec = DebDir (Cd "happstack-ixset" (Darcs happstackRepo)) (Darcs (repo ++ "/happstack-ixset-debian"))
                , P.flags = [] }

    , P.Package { P.name = "haskell-happstack-jmacro"
                , P.spec = Debianize (Hackage "happstack-jmacro")
                , P.flags = [] }
    , debianize "jmacro-rpc-happstack" []
    , debianize "jmacro-rpc" []
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search")
                , P.flags = [] }
    -- Current happstack-server requires directory >= 1.2, which comes with ghc-7.6.
    , P.Package { P.name = "haskell-happstack-server"
                , P.spec = Debianize (Hackage "happstack-server")
                , P.flags = [P.CabalPin "7.0.7", P.DebVersion "7.0.7-1"] }
    , debianize "base64-bytestring" []
    , debianize "threads" []
    , P.Package { P.name = "haskell-list-tries"
                , P.spec = Debianize (Hackage "list-tries")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-static-routing"
                , P.spec = Debianize (Hackage "happstack-static-routing")
                , P.flags = [P.DebVersion "0.3.1-1~hackage1"] }
    , P.Package { P.name = "haskell-happstack-util"
                , P.spec = Debianize (Patch (Hackage "happstack-util") $(embedFile "patches/happstack-util.diff"))
                , P.flags = [P.DebVersion "6.0.3-1"] }
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , P.Package { P.name = "haskell-hsp"
                , P.spec = Debianize (Hackage "hsp")
                , P.flags = quantal release [P.DebVersion "0.7.1-1~hackage1", CabalPin "0.7.1", P.ExtraDep "haskell-hsx-utils"] [CabalPin "0.7.1", P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "haskell-hsx"
                , P.spec = Debianize (Hackage "hsx")
                , P.flags = [P.DebVersion "0.10.4-1~hackage1"] }
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
    , debianize "HJScript" []
    , P.Package { P.name = "reform"
                , P.spec = Debianize (Cd "reform" (Darcs (patchTag ++ "/reform")))
                , P.flags = [P.DebVersion "0.1.2-1~hackage1"] }
    , P.Package { P.name = "reform-blaze"
                , P.spec = Debianize (Cd "reform-blaze" (Darcs (patchTag ++ "/reform")))
                , P.flags = [P.DebVersion "0.1-1~hackage1"] }
    , P.Package { P.name = "reform-happstack"
                , P.spec = Debianize (Cd "reform-happstack" (Darcs (patchTag ++ "/reform")))
                , P.flags = [P.DebVersion "0.1.1-1~hackage1"] }
{-  , P.Package { P.name = "reform-heist"
                , P.spec = Debianize (Cd "reform-heist" (Darcs patchTag ++ "/reform"))
                , P.flags = [] } -}
    , P.Package { P.name = "reform-hsp"
                , P.spec = Debianize (Cd "reform-hsp" (Darcs (patchTag ++ "/reform")))
                , P.flags = [P.DebVersion "0.1.1-1~hackage1"] }
    , debianize "blaze-markup" []
    , apt (quantal release "sid" "quantal") "haskell-blaze-builder" []
    , P.Package { P.name = "haskell-blaze-builder-enumerator"
                , P.spec = Debianize (Hackage "blaze-builder-enumerator")
                , P.flags = [P.DebVersion "0.2.0.5-1~hackage1"] }
    , debianize "blaze-from-html" []
    , debianize "blaze-html" []
    , debianize "blaze-textual" []
    , P.Package { P.name = "haskell-blaze-textual-native"
                , P.spec = Debianize (Patch
                                      (Hackage "blaze-textual-native") $(embedFile "patches/blaze-textual-native.diff"))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , P.Package { P.name = "clckwrks-theme-happstack"
                , P.spec = Debianize (Cd "clckwrks-theme-happstack" (Darcs (repo ++ "/happstack-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "happstack-dot-com"
                , P.spec = Debianize (Patch
                                      (Cd "happstack-dot-com" (Darcs (repo ++ "/happstack-clckwrks"))) $(embedFile "patches/happstack-dot-com.diff"))
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
    , debianize "attoparsec-conduit" []
    , debianize "blaze-builder-conduit" []
    , P.Package { P.name = "haskell-http-conduit"
                , P.spec = Debianize (Hackage "http-conduit")
                , P.flags = [] }
    , debianize "zlib-conduit" []
    , P.Package { P.name = "haskell-xml-conduit"
                , P.spec = Debianize (Hackage "xml-conduit")
                , P.flags = [] }
    ]

-- | Packages pinned pending update of happstack-authenticate (in one possible build order.)
authenticate _home release =
  P.Packages (singleton "authenticate") $
    [ conduit
    , quantal release (apt "sid" "haskell-puremd5" []) (debianize "pureMD5" [])
    , debianize "monadcryptorandom" []
    , debianize "RSA" []
    , P.Package { P.name = "haskell-resourcet"
                , P.spec = Debianize (Hackage "resourcet")
                , P.flags = [] }
    , debianize "void" []
    -- Version 1.3.1 may be too new for tls 0.9.11
    , debianize "certificate" []
    , debianize "pem" []
    , debianize "zlib-bindings" []
    , debianize "tls" []
    , debianize "tls-extra" []
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
                , P.spec = Debianize (Hackage "fb")
                , P.flags = [] }
    ]

-- ircbot needs a dependency on containers
happstackdotcom _home =
    P.Packages (singleton "happstackdotcom") $
    [ P.Package { P.name = "haskell-ircbot"
                , P.spec = Debianize (Hackage "ircbot")
                , P.flags = [] }
{-  , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs ("http://src.seereason.com/happstackDotCom")
                , P.flags = [] } -}
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
    [ debianize "JSONb" [P.DebVersion "1.0.7-1~hackage1"]
    , debianize "data-object-json" [] ]

-- May work with these added dependencies (statevar thru openglraw)
opengl release = P.Packages (singleton "opengl") $
    [ debianize "OpenGL" []
{-  , P.Package { P.name = "haskell-vacuum-opengl"
                , P.spec = Patch (Debianize (Hackage "vacuum-opengl"))
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
    , debianize "GLUT" []
    , debianize "StateVar" (quantal release [P.DebVersion "1.0.0.0-2build1"] [P.DebVersion "1.0.0.0-2build3"])
    , debianize "Tensor" []
    , debianize "GLURaw" []
    , debianize "ObjectName" []
    , debianize "OpenGLRaw" [ P.ExtraDep "libgl1-mesa-dev" ]
    ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib _release = P.Packages (singleton "glib") $
    [ debianize "glib" [ P.ExtraDep "haskell-gtk2hs-buildtools-utils"
                       , P.ExtraDep "libglib2.0-dev"]
    , apt "sid" "haskell-criterion" []
    , apt "sid" "haskell-ltk" []
    , apt "sid" "haskell-chart" []
    , apt "sid" "haskell-gio" []
    , apt "sid" "haskell-gtk" []
    , apt "sid" "haskell-gtksourceview2" []
    , apt "sid" "haskell-pango" [] ]

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
    [ debianize "plugins" []
    , debianize "happstack-plugins" []
    , debianize "plugins-auto" []
    , P.Package { P.name = "haskell-web-plugins"
                , P.spec = Debianize (Cd "web-plugins" (Darcs "http://hub.darcs.net/stepcut/web-plugins"))
                , P.flags = [] }
    ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = P.Packages (singleton "higgsset") $
    [ debianize "unpack-funcs" []
    , debianize "HiggsSet" []
    , debianize "TrieMap" [P.DebVersion "4.0.1-1~hackage1"] ]

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
    [ apt "sid" "haskell-haddock" [] ]

-- These have been failing for some time, and I don't think we've missed them.
failing _release =
    [ debianize "funsat" []
    , apt "sid" "haskell-statistics" [] ]

diagrams = P.Packages (singleton "diagrams")
    [ debianize "diagrams" []
    , debianize "diagrams-lib" []
    , debianize "diagrams-builder" []
    , debianize "diagrams-core" []
    , debianize "diagrams-contrib" []
    , debianize "diagrams-gtk" []
    , debianize "diagrams-cairo" []
    , debianize "diagrams-svg" []
    , debianize "dual-tree" []
    , debianize "monoid-extras" []
    , debianize "newtype" []
    , debianize "active" []
    , debianize "Boolean" []
    , debianize "MemoTrie" []
    , debianize "blaze-svg" []
    , debianize "force-layout" []
    , debianize "cairo" []
    , debianize "hint" []
    , debianize "vector-space" []
    , debianize "vector-space-points" []
    , debianize "MonadCatchIO-mtl" []
    ]

algebra release = P.Packages (singleton "algebra")
    [ debianize "data-lens" []
    , debianize "adjunctions" []
    , debianize "algebra" []
    , debianize "bifunctors" []
    , debianize "categories" []
    , debianize "comonad" []
    , debianize "comonads-fd" []
    , debianize "comonad-transformers" []
    , debianize "control-monad-free" []
    , debianize "transformers-free" []
    , debianize "contravariant" (quantal release [] [P.DebVersion "0.2.0.2-1build2"])
    , debianize "distributive" (quantal release [] [P.DebVersion "0.2.2-1build2"])
    , P.Package { P.name = "free"
                , P.spec = Debianize (Hackage "free")
                , P.flags = [P.DebVersion "3.2-1~hackage1"] }
    , debianize "keys" []
    , debianize "lens" []
    , debianize "lens-family-core" []
    , debianize "lens-family" []
    , P.Package { P.name = "haskell-lens-family-th"
                , P.spec = Debianize (Patch (Hackage "lens-family-th") $(embedFile "patches/lens-family-th.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-linear"
                , P.spec = Debianize (Hackage "linear")
                , P.flags = [] }
    , debianize "representable-functors" []
    , debianize "representable-tries" []
    , debianize "semigroupoids" []
    , debianize "spine" [] ]

-- Debian package has versioned dependencies on binary, but the
-- virtual binary package provided with ghc 7.4 (0.5.1.0) is
-- newer than the version of binary in hackage (0.5.0.2.)  This
-- means we try to pull in bogus debs for libghc-binary-* and
-- dependency problems ensue.
agda _release =
    [ apt "sid" "agda" []
    , apt "sid" "agda-bin" []
    , apt "sid" "agda-stdlib" [] ]

other _release =
    [ apt "sid" "darcs" []
    , patch (debianize "aeson-native" []) $(embedFile "patches/aeson-native.diff")
    , apt "sid" "haskell-binary-shared" [] -- for leksah
    , debianize "cairo" [P.ExtraDep "haskell-gtk2hs-buildtools-utils"] -- for leksah
    , debianize "cabal-dev" [] -- build-env for cabal
    , debianize "gnuplot" [P.DebVersion "0.4.2-1~hackage1"]
    , apt "sid" "bash-completion" []
    ]

apt :: String -> String -> [P.PackageFlag] -> P.Packages
apt dist name flags =
          P.Package
               { P.name = name
               , P.spec = Apt dist name
               , P.flags = flags }

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

-- | Build a target that pulls the source from hackage and then
-- generates a debianization using cabal-debian.  Note that this will
-- affect the debian source package names for a given cabal package,
-- but it does not affect the dependency names generated by cabal
-- debian when it debianizes a package.
debianize :: String -> [P.PackageFlag] -> P.Packages
debianize s flags =
    P.Package { P.name = debianName s
              , P.spec = Debianize (Hackage s)
              , P.flags = flags}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "haskell-quickcheck2"
      debianName "parsec" = "haskell-parsec3"
      debianName "gtk2hs-buildtools" = "gtk2hs-buildtools"
      -- The correct name would be haskell-haskell-src-exts, but the package
      -- in sid has the name "haskell-src-exts".  (Update: we no longer use
      -- the haskell-src-exts package from sid.  Now packages from sid that
      -- depend on this package will fail, so we will have to remove those too.
      -- debianName "haskell-src-exts" = "haskell-src-exts"
      debianName "MissingH" = "haskell-missingh"
      debianName _ = "haskell-" ++ map toLower s

-- FIXME: make generic
patch :: P.Packages -> B.ByteString -> P.Packages
patch package@(P.Package {}) s = package {P.spec = Patch (P.spec package) s}
patch p@(P.Packages {}) s = p {P.packages = map (`patch` s) (P.packages p)}
patch P.NoPackage _ = P.NoPackage

asciiToString :: B.ByteString -> String
asciiToString = map (chr . fromIntegral) . B.unpack
