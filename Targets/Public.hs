{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Targets.Public ( targets ) where

import Data.Char (toLower)
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

    , algebra

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
    , P.Package { P.name = "cpphs"
                , P.spec = Apt "sid" "cpphs"
                , P.flags = [] }
    , P.Package { P.name = "debootstrap"
                , P.spec = Apt "sid" "debootstrap"
                , P.flags = [P.UDeb "debootstrap-udeb"] }
-- Build fails due to some debianization issue
--    , apt release "geneweb"
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
    , debianize "ansi-wl-pprint" [P.DebVersion "0.6.4-1"]
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = Debianize (Hackage "applicative-extras"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.DebVersion "0.1.8-1"] }
    , debianize "asn1-data" []
    , debianize "attempt" [P.DebVersion "0.4.0-1"]
    , debianize "failure" []
    , debianize "attoparsec" []
    , debianize "attoparsec-enumerator" []
    , P.Package { P.name = "haskell-attoparsec-text"
                , P.spec = Debianize (Patch
                                      (Hackage "attoparsec-text")
                                      (unlines
                                       [ "--- x/attoparsec-text.cabal.orig\t2012-01-01 12:43:48.746481982 -0800"
                                       , "+++ x/attoparsec-text.cabal\t2012-01-01 12:20:22.226482130 -0800"
                                       , "@@ -59,10 +59,10 @@"
                                       , " "
                                       , " library"
                                       , "   build-depends: base       >= 3       && < 5,"
                                       , "-                 attoparsec >= 0.7     && < 0.10,"
                                       , "+                 attoparsec >= 0.7,"
                                       , "                  text       >= 0.10    && < 0.12,"
                                       , "                  containers >= 0.1.0.1 && < 0.5,"
                                       , "-                 array      >= 0.1     && < 0.4"
                                       , "+                 array      >= 0.1     && < 0.5"
                                       , "   extensions:      CPP"
                                       , "   exposed-modules: Data.Attoparsec.Text"
                                       , "                    Data.Attoparsec.Text.FastSet" ]))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , debianize "attoparsec-text-enumerator" []
    , debianize "base-unicode-symbols" []
    , debianize "bimap" [P.DebVersion "0.2.4-1~hackage1"]
    , debianize "data-default" []
    , debianize "template-default" []
    , debianize "bitmap" []
    , debianize "bitset" [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-bytestring-nums"
    , debianize "bytestring-trie" []
    , P.Package { P.name = "haskell-bzlib"
                , P.spec = Quilt (Apt "sid" "haskell-bzlib") (Darcs "http://src.seereason.com/haskell-bzlib-quilt")
                , P.flags = [] }
    -- , debianize "cairo-pdf" []
    , debianize "case-insensitive" []
    , debianize "CC-delcont" [P.DebVersion "0.2-1~hackage1"]
    , apt release "haskell-cereal"
    , debianize "citeproc-hs" []
    , P.Package {P.name = "haskell-hexpat",
                 P.spec = Debianize (Hackage "hexpat"),
                 P.flags = []}
    , debianize "List" []
    , debianize "uuid" []
    , debianize "maccatcher" [P.DebVersion "2.1.5-3"]
    , case release of
        "natty-seereason" -> debianize "colour" []
        "precise-seereason" -> debianize "colour" [P.DebVersion "2.3.3-1build1"]
        _ -> apt release "haskell-colour"
    -- , apt release "haskell-configfile"
    , debianize "ConfigFile" []
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer"
                , P.flags = [] }
    , debianize "cipher-aes" []
    , debianize "cprng-aes" []
    , debianize "crypto-random-api" []
    , P.Package { P.name = "haskell-crypto"
                , P.spec = Debianize (Hackage "Crypto")
                , P.flags = [P.DebVersion "4.2.5-1~hackage1"] }
{-  , patched "Crypto"
                    [ P.DebVersion "4.2.4-1"]
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
    , debianize "crypto-api" []
    , debianize "crypto-pubkey-types" []
    , debianize "cryptocipher" []
    , debianize "cryptohash" []
    , debianize "cpu" []
    , debianize "css" [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" [P.DebVersion "0.1.1-3"]
    , apt release "haskell-curl"
    , debianize "data-accessor" []
    , debianize "data-accessor-template" []
    , debianize "data-default" []
    , patched "data-object" []
                    (unlines
                      [ "--- old/data-object.cabal\t2012-01-20 06:42:12.000000000 -0800"
                      , "+++ new/data-object.cabal\t2012-01-20 10:13:25.147160370 -0800"
                      , "@@ -20,6 +20,6 @@"
                      , "                      bytestring >= 0.9.1.4,"
                      , "                      text >= 0.5,"
                      , "                      time >= 1.1.4,"
                      , "-                     failure >= 0.1.0 && < 0.2"
                      , "+                     failure >= 0.1.0"
                      , "     exposed-modules: Data.Object"
                      , "     ghc-options:     -Wall" ])
    , debianize "dataenc" []
    , debianize "Diff" [P.DebVersion "0.1.3-2"]
    , apt release "haskell-digest"
    , apt release "haskell-dlist"
    -- Natty only(?)
    , debianize "double-conversion" []
    , debianize "groom" []
    , apt release "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Apt "sid" "haskell-edison-api"
                , P.flags = [] }
    , apt release "haskell-edison-core"
    , apt release "haskell-entropy"
    , debianize "enumerator" []
    , P.Package { P.name = "haskell-hdaemonize"
                , P.spec = Debianize (Patch
                                      (Hackage "hdaemonize")
                                      (unlines
                                       [ "--- old/System/Posix/Daemonize.hs\t2012-02-04 17:36:24.000000000 -0800"
                                       , "+++ new/System/Posix/Daemonize.hs\t2012-05-18 10:59:27.809909158 -0700"
                                       , "@@ -156,23 +156,33 @@"
                                       , "                  "
                                       , "       process daemon [\"stop\"]  = "
                                       , "           do pid <- pidRead daemon"
                                       , "-             let ifdo x f = x >>= \\x -> if x then f else pass"
                                       , "              case pid of"
                                       , "                Nothing  -> pass"
                                       , "-               Just pid -> "
                                       , "-                   (do signalProcess sigTERM pid"
                                       , "-                       usleep (10^6)"
                                       , "-                       ifdo (pidLive pid) $ "
                                       , "-                            do usleep (3*10^6)"
                                       , "-                               ifdo (pidLive pid) (signalProcess sigKILL pid))"
                                       , "-                   `finally`"
                                       , "-                   removeLink (pidFile daemon)"
                                       , "+               Just pid -> (stop pid >> wait (waitSecs daemon) pid) `finally` removeLink (pidFile daemon)"
                                       , " "
                                       , "       process daemon [\"restart\"] = do process daemon [\"stop\"]"
                                       , "                                       process daemon [\"start\"]"
                                       , "       process _      _ = "
                                       , "         getProgName >>= \\pname -> putStrLn $ \"usage: \" ++ pname ++ \" {start|stop|restart}\""
                                       , " "
                                       , "+      -- If the process still exists, begin the shutdown process."
                                       , "+      stop :: CPid -> IO ()"
                                       , "+      stop pid ="
                                       , "+          pidLive pid >>= \\ alive ->"
                                       , "+          if alive"
                                       , "+          then signalProcess sigTERM pid >> usleep (10^3)"
                                       , "+          else pass"
                                       , "+"
                                       , "+      -- If still alive wait the designated period."
                                       , "+      wait :: Maybe Int -> CPid -> IO ()"
                                       , "+      wait remain pid ="
                                       , "+          pidLive pid >>= \\ alive -> "
                                       , "+          if alive"
                                       , "+          then if maybe True (> 0) remain"
                                       , "+               then (usleep (10^6) >> wait (fmap (\\x->x-1) remain) pid)"
                                       , "+               else signalProcess sigKILL pid"
                                       , "+          else pass"
                                       , "+"
                                       , " -- | The details of any given daemon are fixed by the 'CreateDaemon'"
                                       , " -- record passed to 'serviced'.  You can also take a predefined form"
                                       , " -- of 'CreateDaemon', such as 'simpleDaemon' below, and set what"
                                       , "@@ -214,6 +224,7 @@"
                                       , "                                      -- have a good reason to do"
                                       , "                                      -- otherwise, leave this as"
                                       , "                                      -- 'Nothing'."
                                       , "+  , waitSecs :: Maybe Int   -- ^ How many seconds to wait before sending the sigKILL.  If Nothing wait forever.  Default 4."
                                       , " }"
                                       , " "
                                       , " -- | The simplest possible instance of 'CreateDaemon' is "
                                       , "@@ -240,7 +251,8 @@"
                                       , "   syslogOptions = [],"
                                       , "   pidfileDirectory = Nothing,"
                                       , "   program = const $ M.forever $ return (),"
                                       , "-  privilegedAction = return ()"
                                       , "+  privilegedAction = return (),"
                                       , "+  waitSecs = Just 4"
                                       , " }"
                                       , "   "
                                       , " " ]))
                , P.flags = [P.DebVersion "0.4.4.1-1~hackage1"] }
    , debianize "hsyslog" []
    , debianize "erf" [P.DebVersion "2.0.0.0-3"]
    , apt release "haskell-feed"
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
                , P.spec = Debianize (Patch
                                      (Hackage "formlets")
                                      (unlines
                                       [ "diff -ru formlets-0.8.orig/formlets.cabal formlets-0.8/formlets.cabal"
                                       , "--- old/formlets.cabal\t2010-12-21 19:08:34.000000000 -0800"
                                       , "+++ new/formlets.cabal\t2012-05-21 17:32:06.863265531 -0700"
                                       , "@@ -21,12 +21,12 @@"
                                       , "     Description: Choose the even newer, even smaller, split-up base package."
                                       , " "
                                       , " Library"
                                       , "-  Build-Depends:   haskell98, "
                                       , "-                   xhtml, "
                                       , "+  Build-Depends:   xhtml, "
                                       , "                    applicative-extras >= 0.1.7, "
                                       , "                    bytestring,"
                                       , "-                   blaze-html >= 0.2,"
                                       , "-                   transformers == 0.2.2.0"
                                       , "+                   blaze-html >= 0.5,"
                                       , "+                   blaze-markup,"
                                       , "+                   transformers >= 0.2.2.0"
                                       , "   if flag(base4)"
                                       , "     Build-Depends: base >= 4 && < 5, syb"
                                       , "   else"
                                       , "diff -ru formlets-0.8.orig/Text/Blaze/Html5/Formlets.hs formlets-0.8/Text/Blaze/Html5/Formlets.hs"
                                       , "--- old/Text/Blaze/Html5/Formlets.hs\t2010-12-21 19:08:34.000000000 -0800"
                                       , "+++ new/Text/Blaze/Html5/Formlets.hs\t2012-05-21 17:30:10.513955542 -0700"
                                       , "@@ -35,34 +35,34 @@"
                                       , " --"
                                       , " input :: Monad m => Html5Formlet m String"
                                       , " input = input' $ \\n v -> H.input ! A.type_ \"text\""
                                       , "-                                 ! A.name (H.stringValue n)"
                                       , "-                                 ! A.id (H.stringValue n)"
                                       , "-                                 ! A.value (H.stringValue v)"
                                       , "+                                 ! A.name (H.toValue n)"
                                       , "+                                 ! A.id (H.toValue n)"
                                       , "+                                 ! A.value (H.toValue v)"
                                       , " "
                                       , " -- | A textarea with optional rows and columns, and an optional value"
                                       , " --"
                                       , " textarea :: Monad m => Maybe Int -> Maybe Int -> Html5Formlet m String"
                                       , "-textarea r c = input' $ \\n v -> (applyAttrs n H.textarea) (H.string v)"
                                       , "+textarea r c = input' $ \\n v -> (applyAttrs n H.textarea) (H.toHtml v)"
                                       , "   where"
                                       , "-    applyAttrs n = (! A.name (H.stringValue n)) . rows r . cols c"
                                       , "-    rows = maybe id $ \\x -> (! A.rows (H.stringValue $ show x))"
                                       , "-    cols = maybe id $ \\x -> (! A.cols (H.stringValue $ show x))"
                                       , "+    applyAttrs n = (! A.name (H.toValue n)) . rows r . cols c"
                                       , "+    rows = maybe id $ \\x -> (! A.rows (H.toValue $ show x))"
                                       , "+    cols = maybe id $ \\x -> (! A.cols (H.toValue $ show x))"
                                       , " "
                                       , " -- | A password field with an optional value"
                                       , " --"
                                       , " password :: Monad m => Html5Formlet m String"
                                       , " password = input' $ \\n v -> H.input ! A.type_ \"password\""
                                       , "-                                    ! A.name (H.stringValue n)"
                                       , "-                                    ! A.id (H.stringValue n)"
                                       , "-                                    ! A.value (H.stringValue v)"
                                       , "+                                    ! A.name (H.toValue n)"
                                       , "+                                    ! A.id (H.toValue n)"
                                       , "+                                    ! A.value (H.toValue v)"
                                       , " "
                                       , " -- | A hidden input field"
                                       , " --"
                                       , " hidden :: Monad m => Html5Formlet m String"
                                       , " hidden = input' $ \\n v -> H.input ! A.type_ \"hidden\""
                                       , "-                                  ! A.name (H.stringValue n)"
                                       , "-                                  ! A.id (H.stringValue n)"
                                       , "-                                  ! A.value (H.stringValue v)"
                                       , "+                                  ! A.name (H.toValue n)"
                                       , "+                                  ! A.id (H.toValue n)"
                                       , "+                                  ! A.value (H.toValue v)"
                                       , " "
                                       , " -- | A validated integer component"
                                       , " --"
                                       , "@@ -73,8 +73,8 @@"
                                       , " --"
                                       , " file :: Monad m => Html5Form m File"
                                       , " file = inputFile $ \\n -> H.input ! A.type_ \"file\""
                                       , "-                                 ! A.name (H.stringValue n)"
                                       , "-                                 ! A.id (H.stringValue n)"
                                       , "+                                 ! A.name (H.toValue n)"
                                       , "+                                 ! A.id (H.toValue n)"
                                       , " "
                                       , " -- | A checkbox with an optional default value"
                                       , " --"
                                       , "@@ -84,13 +84,13 @@"
                                       , "     asBool (Just _) = Success True"
                                       , "     asBool Nothing = Success False"
                                       , "     html (Just True) n = H.input ! A.type_ \"checkbox\" "
                                       , "-                                 ! A.name (H.stringValue n)"
                                       , "-                                 ! A.id (H.stringValue n)"
                                       , "+                                 ! A.name (H.toValue n)"
                                       , "+                                 ! A.id (H.toValue n)"
                                       , "                                  ! A.value \"on\""
                                       , "                                  ! A.checked \"checked\""
                                       , "     html _ n = H.input ! A.type_ \"checkbox\""
                                       , "-                       ! A.name (H.stringValue n)"
                                       , "-                       ! A.id (H.stringValue n)"
                                       , "+                       ! A.name (H.toValue n)"
                                       , "+                       ! A.id (H.toValue n)"
                                       , "                        ! A.value \"on\""
                                       , " "
                                       , " -- | A radio choice"
                                       , "@@ -102,19 +102,19 @@"
                                       , "   where"
                                       , "     makeRadio name selected ((value, label'), idx) = do"
                                       , "         applyAttrs (radio' name value id')"
                                       , "-        H.label ! A.for (H.stringValue id')"
                                       , "+        H.label ! A.for (H.toValue id')"
                                       , "                 ! A.class_ \"radio\""
                                       , "-                $ H.string label'"
                                       , "+                $ H.toHtml label'"
                                       , "       where"
                                       , "         applyAttrs | selected == value = (! A.checked \"checked\")"
                                       , "                    | otherwise = id"
                                       , "         id' = name ++ \"_\" ++ show idx"
                                       , " "
                                       , "     radio' n v i = H.input ! A.type_ \"radio\""
                                       , "-                           ! A.name (H.stringValue n)"
                                       , "-                           ! A.id (H.stringValue i)"
                                       , "+                           ! A.name (H.toValue n)"
                                       , "+                           ! A.id (H.toValue i)"
                                       , "                            ! A.class_ \"radio\""
                                       , "-                           ! A.value (H.stringValue v)"
                                       , "+                           ! A.value (H.toValue v)"
                                       , " "
                                       , " -- | An radio choice for Enums"
                                       , " --"
                                       , "@@ -129,7 +129,7 @@"
                                       , " -- | A label"
                                       , " --"
                                       , " label :: Monad m => String -> Form H.Html m ()"
                                       , "-label = xml . H.label . H.string"
                                       , "+label = xml . H.label . H.toHtml"
                                       , " "
                                       , " -- | This is a helper function to generate select boxes"
                                       , " --"
                                       , "@@ -138,11 +138,11 @@"
                                       , "            -> String              -- ^ The value that is selected"
                                       , "            -> H.Html"
                                       , " selectHtml choices name selected ="
                                       , "-    H.select ! A.name (H.stringValue name)"
                                       , "+    H.select ! A.name (H.toValue name)"
                                       , "              $ mconcat $ map makeChoice choices"
                                       , "   where"
                                       , "     makeChoice (value, label') = applyAttrs $"
                                       , "-        H.option ! A.value (H.stringValue value) $ label'"
                                       , "+        H.option ! A.value (H.toValue value) $ label'"
                                       , "       where"
                                       , "         applyAttrs | selected == value = (! A.selected \"selected\")"
                                       , "                    | otherwise = id" ]))
                , P.flags = [P.DebVersion "0.8-1~hackage1"] }
    , patched "gd"  [ P.ExtraDevDep "libgd-dev" ]
                    (unlines
                      [ "--- gd/gd.cabal.orig\t2011-06-25 12:27:26.000000000 -0700"
                      , "+++ gd/gd.cabal\t2011-09-10 14:29:48.514415016 -0700"
                      , "@@ -21,7 +21,7 @@"
                      , "   Extensions: ForeignFunctionInterface"
                      , "   Exposed-Modules: Graphics.GD, Graphics.GD.ByteString, Graphics.GD.ByteString.Lazy"
                      , "   Ghc-options: -Wall"
                      , "-  Extra-libraries: gd, png, z, jpeg, m, fontconfig, freetype, expat"
                      , "+  Extra-libraries: gd, png, z, jpeg, fontconfig, expat"
                      , "   Includes: gd.h"
                      , "   Include-dirs:        cbits"
                      , "   Install-includes: gd-extras.h" ])
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , debianize "cabal-macosx" []
    , apt release "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , apt release "haskell-harp"
    , debianize "hashable" []
    , debianize "hashed-storage" []
    , P.Package { P.name = "haskell-haskeline"
                , P.spec = Debianize (Hackage "haskeline")
                , P.flags = [P.DebVersion "0.7.0.3-1~hackage1"] }
    , P.Package { P.name = "haskell-th-orphans"
                , P.spec = Debianize (Patch
                                      (Hackage "th-orphans")
                                      (unlines
                                       [ "--- old/src/Language/Haskell/TH/Instances.hs\t2012-09-17 21:52:45.000000000 -0700"
                                       , "+++ new/src/Language/Haskell/TH/Instances.hs\t2012-09-17 22:17:14.057539784 -0700"
                                       , "@@ -9,10 +9,11 @@"
                                       , "   Maintainer  :  Michael Sloan <mgsloan at gmail>"
                                       , "   Stability   :  experimental"
                                       , "   Portability :  portable (template-haskell)"
                                       , "+"
                                       , "+  Provides Ord and Lift instances for the TH datatypes.  Also provides"
                                       , "+  Show / Eq for Loc, and Ppr for Loc / Lit."
                                       , " -}"
                                       , " "
                                       , "--- | Provides Ord and Lift instances for the TH datatypes.  Also provides"
                                       , "---   Show / Eq for Loc, and Ppr for Loc / Lit."
                                       , " module Language.Haskell.TH.Instances () where"
                                       , " "
                                       , " import Language.Haskell.TH.Syntax" ]))
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
    , debianize "MonadCatchIO-transformers" []
    , debianize "hinotify" []
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    -- Not used, and not building.
    -- , debianize "hoauth" []
    , debianize "hostname" [P.DebVersion "1.0-4build1"]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" []
    , debianize "hs-bibutils" []
    , apt release "haskell-hsemail"
    , patched "HsOpenSSL"
                    [ P.ExtraDevDep "libssl-dev"
                    , P.ExtraDevDep "libcrypto++-dev" ]
                    (unlines
                      [ "--- old/HsOpenSSL.cabal\t2011-09-10 15:02:20.000000000 -0700"
                      , "+++ new/HsOpenSSL.cabal\t2011-09-10 15:24:16.735325250 -0700"
                      , "@@ -50,7 +50,7 @@"
                      , "       CC-Options:         -D MINGW32"
                      , "       CPP-Options:        -DCALLCONV=stdcall"
                      , "   else"
                      , "-      Extra-Libraries: crypto ssl"
                      , "+      Extra-Libraries: crypto++ ssl"
                      , "       C-Sources:          cbits/mutex-pthread.c"
                      , "       CC-Options:         -D PTHREAD"
                      , "       CPP-Options:        -DCALLCONV=ccall"
                      , "@@ -108,6 +108,6 @@"
                      , "   C-Sources:"
                      , "           cbits/HsOpenSSL.c"
                      , "   Include-Dirs:"
                      , "-          cbits"
                      , "+          cbits, dist/build/autogen, dist-ghc/build/autogen"
                      , "   Install-Includes:"
                      , "           HsOpenSSL.h"
                      , "--- old/cbits/HsOpenSSL.h\t2012-04-06 18:54:18.000000000 -0700"
                      , "+++ new/cbits/HsOpenSSL.h\t2012-04-06 20:37:47.827713118 -0700"
                      , "@@ -27,7 +27,7 @@"
                      , "  * hsc2hs so we can reach the cabal_macros.h from cbits."
                      , "  */"
                      , " #if !defined(MIN_VERSION_base)"
                      , "-#  include \"../dist/build/autogen/cabal_macros.h\""
                      , "+#  include <cabal_macros.h>"
                      , " #endif"
                      , " "
                      , " /* OpenSSL ********************************************************************/" ])
    , debianize "HsSyck" [P.DebVersion "0.50-2"]
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
{-  , apt release "haskell-leksah"
    , apt release "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , P.Package { P.name = "haskell-pointed"
                , P.spec = Debianize (Hackage "pointed")
                , P.flags = [] }
    , patched "logic-TPTP" [ P.ExtraDep "alex"
                           , P.ExtraDep "happy" ]
                           (unlines
                               [ "--- old/logic-TPTP.cabal\t2011-09-15 16:31:03.000000000 -0700"
                               , "+++ new/logic-TPTP.cabal\t2011-09-16 13:40:26.458725487 -0700"
                               , "@@ -51,7 +51,7 @@"
                               , "  "
                               , " "
                               , " Library"
                               , "- ghc-options: -Wall -O2"
                               , "+ ghc-options: -Wall -O2 -XBangPatterns"
                               , "  "
                               , "  build-depends:      base >=4 && < 5"
                               , "                    , array"
                               , "--- old/Parser.y\t2012-01-20 11:54:31.000000000 -0800"
                               , "+++ new/Parser.y\t2012-01-20 13:07:07.427160933 -0800"
                               , "@@ -9,7 +9,7 @@"
                               , " import Control.Monad"
                               , " import Data.List as L"
                               , " import Lexer"
                               , "-import Data.Set as S"
                               , "+import qualified Data.Set as S"
                               , " import Codec.TPTP.Base"
                               , " import System.IO"
                               , " import System.IO.Unsafe"
                               , "--- old/ParserC.y\t2012-01-20 13:28:19.000000000 -0800"
                               , "+++ new/ParserC.y\t2012-01-20 13:35:49.917165510 -0800"
                               , "@@ -7,7 +7,7 @@"
                               , " import Control.Monad"
                               , " import Data.List as L"
                               , " import Lexer"
                               , "-import Data.Set as S"
                               , "+import qualified Data.Set as S"
                               , " import Codec.TPTP.Base"
                               , " import System.IO"
                               , " import System.IO.Unsafe" ])
    , apt release "haskell-maybet"
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime"
                , P.flags = [] }
    , apt release "haskell-mmap"
    , debianize "monad-control" []
    , P.Package { P.name = "haskell-monad-par-extras"
                , P.spec = Debianize (Hackage "monad-par-extras")
                , P.flags = [P.DebVersion "0.3.2-1~hackage1"] }
    , debianize "abstract-deque" []
    , debianize "abstract-par" []
    , debianize "monad-par" []
    , debianize "IORefCAS" []
    , debianize "bits-atomic" []
    , apt release "haskell-monadcatchio-mtl"
    , debianize "monadLib" []
    -- Putting this in our repo can cause problems, because when it is
    -- installed some packages can't compile unless you add package
    -- qualifiers to their imports.  For this reason, when we run the
    -- autobuilder with the --lax flag we usually get a failure from
    -- some package that builds after monads-tf got installed.  On the
    -- other hand, without monads-tf we lose this dependency chain:
    -- monads-tf -> options -> fay.
    -- , debianize "monads-tf" []
    , apt release "haskell-monoid-transformer"
    , debianize "murmur-hash" []
    , apt release "haskell-mwc-random"
    , patched "nano-hmac" [ P.DebVersion "0.2.0ubuntu1" ]
                            (unlines
                             [ "--- nano-hmac/nano-hmac.cabal.orig\t2011-08-14 09:25:43.000000000 -0700"
                             , "+++ nano-hmac/nano-hmac.cabal\t2011-09-10 14:24:25.234226579 -0700"
                             , "@@ -20,8 +20,8 @@"
                             , "   else"
                             , "     build-depends:     base < 3"
                             , "   exposed-modules:     Data.Digest.OpenSSL.HMAC"
                             , "-  ghc-options:         -Wall -Werror -O2 -fvia-C"
                             , "+  ghc-options:         -Wall -Werror -O2"
                             , "   extensions:          ForeignFunctionInterface, BangPatterns, CPP"
                             , "   includes:            openssl/hmac.h"
                             , "-  extra-libraries:     crypto ssl"
                             , "+  extra-libraries:     crypto++ ssl"
                             , " "
                             , "--- old/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 16:39:39.603631778 -0700"
                             , "+++ new/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 13:57:55.000000000 -0700"
                             , "@@ -35,8 +35,11 @@"
                             , " "
                             , " import qualified Data.ByteString as B"
                             , " import qualified Data.ByteString.Unsafe as BU"
                             , "-import Foreign"
                             , "+import System.IO.Unsafe"
                             , " import Foreign.C.Types"
                             , "+import Foreign.Ptr"
                             , "+import Foreign.Storable"
                             , "+import Data.Word"
                             , " import Numeric (showHex)"
                             , " "
                             , " #include \"openssl/hmac.h\""
                             , "--- old/Data/Digest/OpenSSL/HMAC.hsc\t2012-01-20 06:44:45.000000000 -0800"
                             , "+++ new/Data/Digest/OpenSSL/HMAC.hsc\t2012-01-20 10:35:41.337161457 -0800"
                             , "@@ -103,13 +103,13 @@"
                             , "        what else was I going to do?"
                             , "     -}"
                             , "     where"
                             , "-      go :: (Storable a, Integral a) => Ptr a -> Int -> [String] -> IO String"
                             , "+      go :: (Storable a, Integral a, Show a) => Ptr a -> Int -> [String] -> IO String"
                             , "       go !q !n acc"
                             , "           | n >= len  = return $ concat (reverse acc)"
                             , "           | otherwise = do w <- peekElemOff q n"
                             , "                            go q (n+1) (draw w : acc)"
                             , " "
                             , "-      draw :: (Integral a) => a -> String"
                             , "+      draw :: (Integral a, Show a) => a -> String"
                             , "       draw w = case showHex w [] of"
                             , "                  [x] -> ['0', x]"
                             , "                  x   -> x" ])
    , patched "openid" []
                    (unlines
                      [ "--- old/openid.cabal\t2012-09-17 16:18:01.000000000 -0700"
                      , "+++ new/openid.cabal\t2012-09-17 21:23:19.021463349 -0700"
                      , "@@ -28,11 +28,11 @@"
                      , " library"
                      , "   build-depends:   base       >= 4.0.0.0  && < 5.0.0.0,"
                      , "                    bytestring >= 0.9.1.0  && < 0.10.0.0,"
                      , "-                   containers >= 0.2.0.0  && < 0.4.1.0,"
                      , "-                   HTTP       >= 4000.0.9 && < 4000.2,"
                      , "-                   monadLib   >= 3.6.0.0  && < 3.7.0.0,"
                      , "-                   network    >= 2.2.0.0  && < 2.4.0.0,"
                      , "-                   time       >= 1.1.0.0  && < 1.3.0.0,"
                      , "+                   containers >= 0.2.0.0,"
                      , "+                   HTTP       >= 4000.0.9,"
                      , "+                   monadLib   >= 3.6.0.0,"
                      , "+                   network    >= 2.2.0.0  && < 2.5.0.0,"
                      , "+                   time       >= 1.1.0.0,"
                      , "                    xml        >= 1.3.0.0  && < 1.4.0.0,"
                      , "                    HsOpenSSL  >= 0.9.0.0  && < 0.11.0.0"
                      , "   hs-source-dirs:  src"
                      , "--- old/src/Data/Digest/OpenSSL/AlternativeHMAC.hsc\t2012-01-23 15:26:46.027160840 -0800"
                      , "+++ new/src/Data/Digest/OpenSSL/AlternativeHMAC.hsc\t2012-01-20 11:40:25.566842934 -0800"
                      , "@@ -59,7 +59,7 @@"
                      , " showHMAC bs ="
                      , "     concatMap draw $ BS.unpack bs"
                      , "     where"
                      , "-      draw :: (Integral a) => a -> String"
                      , "+      draw :: (Integral a, Show a) => a -> String"
                      , "       draw w = case showHex w [] of"
                      , "                  [x] -> ['0', x]"
                      , "                  x   -> x" ])
    , P.Package { P.name = "haskell-operational"
                , P.spec = Debianize (Hackage "operational")
                , P.flags = [P.OmitLTDeps, P.DebVersion "0.2.1.3-1~hackage1"] }
--    , debianize "options" []
    , debianize "optparse-applicative" []
    , debianize "ordered" []
    , debianize "multiset" [P.CabalPin "0.2.1"] -- 0.2.2 requires containers >= 0.5, which comes with ghc 7.6.
    , debianize "temporary" []
    , debianize "pandoc-types" [P.DebVersion "1.9.1-1"]
    , debianize "parse-dimacs" []
    , debianize "parseargs" []
    , apt release "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2") (Darcs "http://src.seereason.com/pbkdf2-debian"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt release "haskell-pcre-light"
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
    , debianize "PSQueue" [P.DebVersion "1.1-2"]
    , debianize "pwstore-purehaskell" [P.DebVersion "2.1-1~hackage1"]
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1.
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    , apt release "haskell-quickcheck1"
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- , apt release "haskell-regex-tdfa"
    , debianize "regex-tdfa" [P.DebVersion "1.1.8-1"]
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision"
                , P.flags = [] }
    , debianize "RJson" []
    , debianize "safe" [P.DebVersion "0.3.3-2"]
    , debianize "safecopy" []
    , patched "sat"
                    [ P.DebVersion "1.1.1-1~hackage1" ]
                    (unlines
                      [ "--- sat/sat.cabal.orig\t2011-09-10 10:16:05.000000000 -0700"
                      , "+++ sat/sat.cabal\t2011-09-10 14:14:46.784184607 -0700"
                      , "@@ -13,7 +13,7 @@"
                      , " description: CNF(Clausal Normal Form) SATisfiability Solver and Generator"
                      , " category: algorithms"
                      , " -- tested-with: ghc-6.4.2"
                      , "-build-depends: base"
                      , "+build-depends: base, random"
                      , " "
                      , " executable: SATSolve"
                      , " main-is: \"SATSolver.hs\"" ])
    , debianize "semigroups" []
    , debianize "sendfile" []
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra"
                , P.flags = [] }
    , apt release "haskell-sha"
    , debianize "shake" []
    , debianize "shakespeare" []
    , debianize "shakespeare-css" []
    , P.Package { P.name = "haskell-simple-css",
                  P.spec = Patch (Debianize (Hackage "simple-css"))
                                 (unlines
                                  [ "--- old/simple-css.cabal\t2012-07-10 07:12:19.000000000 -0700"
                                  , "+++ new/simple-css.cabal\t2012-07-10 09:49:49.394977221 -0700"
                                  , "@@ -26,7 +26,7 @@"
                                  , "   Build-Depends:"
                                  , "         base >= 4, base < 5, "
                                  , "         unordered-containers >= 0.1.1, hashable >= 1.1.1.0,"
                                  , "-        language-css >= 0.0.2, blaze-html >= 0.4  "
                                  , "+        language-css >= 0.0.2, blaze-html >= 0.5, blaze-markup >= 0.5.1"
                                  , "   Hs-Source-Dirs:      src/"
                                  , "   Exposed-Modules:"
                                  , "         SimpleCss"
                                  , "--- old/src/SimpleCss.hs\t2012-03-25 07:53:41.000000000 -0700"
                                  , "+++ new/src/SimpleCss.hs\t2012-03-25 14:43:54.789176546 -0700"
                                  , "@@ -20,7 +20,7 @@"
                                  , " "
                                  , " --import qualified Data.Map as M"
                                  , " import Data.Hashable"
                                  , "-import qualified Data.HashMap.Lazy as H"
                                  , "+import qualified Data.HashMap.Lazy as HM"
                                  , " "
                                  , " import qualified Text.Blaze as H"
                                  , " import qualified Text.Blaze.Html5 as H"
                                  , "@@ -135,11 +135,11 @@"
                                  , " blazeSpec = HtmlSpec"
                                  , "     (blazeTag H.div)"
                                  , "     (blazeTag H.span)"
                                  , "-    (\\href -> blazeTag (H.a H.! HA.href (H.stringValue href)))"
                                  , "-    (\\s x -> x H.! HA.class_ (H.stringValue s))"
                                  , "+    (\\href -> blazeTag (H.a H.! HA.href (H.toValue href)))"
                                  , "+    (\\s x -> x H.! HA.class_ (H.toValue s))"
                                  , " "
                                  , " blazeTag tag xs"
                                  , "-    | null xs   = tag $ H.string \"\""
                                  , "+    | null xs   = tag $ H.toHtml \"\""
                                  , "     | otherwise = tag $ foldl1 (>>) xs"
                                  , " "
                                  , " -------------------------------------------------------------------------"
                                  , "@@ -149,7 +149,7 @@"
                                  , "     deriving (Show, Eq, Ord)"
                                  , " "
                                  , " type ClassId    = String"
                                  , "-type ClassTable = H.HashMap Rule ClassId"
                                  , "+type ClassTable = HM.HashMap Rule ClassId"
                                  , " "
                                  , " data CssTag a = DivTag | SpanTag | ATag Href | Prim a"
                                  , " "
                                  , "@@ -171,7 +171,7 @@"
                                  , " "
                                  , " -- class names table"
                                  , " classTable :: [Rule] -> ClassTable"
                                  , "-classTable = H.fromList . flip zip ids . nub"
                                  , "+classTable = HM.fromList . flip zip ids . nub"
                                  , "     where ids = map (('c' : ). show) [0 ..]  "
                                  , "           phi id (a, b) = (b, (id, a))"
                                  , "  "
                                  , "@@ -245,7 +245,7 @@"
                                  , " -- print ruleSets"
                                  , " ppRuleSets :: ClassTable -> [RuleSet]"
                                  , " ppRuleSets = ((uncurry $ flip toRuleSet) =<< ) "
                                  , "-    . sortOn snd . H.toList"
                                  , "+    . sortOn snd . HM.toList"
                                  , " "
                                  , " "
                                  , " toRuleSet :: String -> Rule -> [RuleSet]"
                                  , "@@ -260,7 +260,7 @@"
                                  , " ppHtml :: HtmlSpec a -> ClassTable -> TagTree a -> a"
                                  , " ppHtml spec table (TagTree (CssNode tag rules) xs) = "
                                  , "     setAttrs spec attrs next"
                                  , "-    where attrs = map (maybe [] id . flip H.lookup table) rules"
                                  , "+    where attrs = map (maybe [] id . flip HM.lookup table) rules"
                                  , "           next  = case tag of"
                                  , "                     Prim a    -> a"
                                  , "                     DivTag    -> divTag  spec next'"
                                  , "--- old/src/SimpleCss/Tricks/Shortcuts/Html.hs\t2012-10-12 10:38:26.000000000 -0700"
                                  , "+++ new/src/SimpleCss/Tricks/Shortcuts/Html.hs\t2012-10-12 11:02:38.474180790 -0700"
                                  , "@@ -19,7 +19,7 @@"
                                  , " "
                                  , " -- html-elements"
                                  , " "
                                  , "-textTag tag = prim . tag . H.string"
                                  , "+textTag tag = prim . tag . H.toHtml"
                                  , " "
                                  , " -- | @p@ tag "
                                  , " p :: String -> Css H.Html"
                                  , "@@ -37,7 +37,7 @@"
                                  , " --"
                                  , " -- * text"
                                  , " a :: String -> String -> Css H.Html"
                                  , "-a href text = prim $ H.a H.! HA.href (H.stringValue href) $ H.string text"
                                  , "+a href text = prim $ H.a H.! HA.href (H.toValue href) $ H.toHtml text"
                                  , " "
                                  , " -- headers"
                                  , " "
                                  , "@@ -73,7 +73,7 @@"
                                  , " --"
                                  , " -- * @src@ atribute value"
                                  , " img :: String -> String -> Css H.Html"
                                  , "-img alt src = prim $ H.img H.! HA.src (H.stringValue src) H.! HA.alt (H.stringValue alt)"
                                  , "+img alt src = prim $ H.img H.! HA.src (H.toValue src) H.! HA.alt (H.toValue alt)"
                                  , " "
                                  , " "
                                  , " -- | @ul@ tag"
                                  , "@@ -85,7 +85,7 @@"
                                  , " ol = ls H.ol"
                                  , " "
                                  , " -- lists"
                                  , "-ls constr = prim . constr . foldl1 (>>) . map (H.li . H.string)"
                                  , "+ls constr = prim . constr . foldl1 (>>) . map (H.li . H.toHtml)"
                                  , " "
                                  , " "
                                  , " -- | @ul@ tag with links"
                                  , "@@ -102,7 +102,7 @@"
                                  , " "
                                  , " "
                                  , " als constr = prim . constr . foldl1 (>>) . map (H.li . setA)"
                                  , "-    where setA (href, name) = H.a H.! HA.href (H.stringValue href) $ H.string name"
                                  , "+    where setA (href, name) = H.a H.! HA.href (H.toValue href) $ H.toHtml name"
                                  , " "
                                  , " "
                                  , " "
                                  , "@@ -121,15 +121,15 @@"
                                  , "     case h of"
                                  , "         Just x  -> tr H.th x >> trs"
                                  , "         Nothing -> trs"
                                  , "-    where tr f x  = H.tr $ foldl (>>) (return ()) $ map (f . H.string) x"
                                  , "+    where tr f x  = H.tr $ foldl (>>) (return ()) $ map (f . H.toHtml) x"
                                  , "           trs     = foldl1 (>>) $ map (tr H.td) rs   "
                                  , " "
                                  , " "
                                  , " "
                                  , " encoding :: String -> H.Html"
                                  , "-encoding str = H.meta H.! HA.httpEquiv (H.stringValue \"Content-Type\")"
                                  , "-                      H.! HA.content (H.stringValue \"text/html\")"
                                  , "-                      H.! HA.charset (H.stringValue str)"
                                  , "+encoding str = H.meta H.! HA.httpEquiv (H.toValue \"Content-Type\")"
                                  , "+                      H.! HA.content (H.toValue \"text/html\")"
                                  , "+                      H.! HA.charset (H.toValue str)"
                                  , " "
                                  , " "
                                  , " -- | writes css and htmls to files"
                                  , "@@ -162,16 +162,16 @@"
                                  , " "
                                  , " linkCss :: String -> H.Html"
                                  , " linkCss cssFile = "
                                  , "-    H.link H.! HA.rel   (H.stringValue \"stylesheet\")"
                                  , "-           H.! HA.type_ (H.stringValue \"text/css\") "
                                  , "-           H.! HA.href  (H.stringValue cssFile)"
                                  , "+    H.link H.! HA.rel   (H.toValue \"stylesheet\")"
                                  , "+           H.! HA.type_ (H.toValue \"text/css\") "
                                  , "+           H.! HA.href  (H.toValue cssFile)"
                                  , " "
                                  , " -- | genereates html filenames and head's sublelements from list of titles"
                                  , " initHtmls :: [String] -> [(String, H.Html)]"
                                  , " initHtmls names = zip (map (++ \".html\") names) $ map fromTitle names"
                                  , " "
                                  , " fromTitle :: String -> H.Html"
                                  , "-fromTitle title = encoding \"UTF-8\" >> (H.title $ H.string title)"
                                  , "+fromTitle title = encoding \"UTF-8\" >> (H.title $ H.toHtml title)"
                                  , " "
                                  , " "
                                  , " " ])
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
    , apt release "haskell-strict" -- for leksah
    , debianize "strict-concurrency" [P.DebVersion "0.2.4.1-3"]
    , debianize "strict-io" [] -- for GenI
    , debianize "smallcheck" []
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb-with-class" []
    , apt release "haskell-syb-with-class-instances-text"
    , debianize "tagged" []
    , debianize "tagsoup" []
    , debianize "tar" []
    , debianize "terminfo" [P.ExtraDep "libncurses5-dev", P.ExtraDevDep "libncurses5-dev"]
    , debianize "test-framework"
                    [ {- P.Patch . B.pack. unlines $
                      [ "--- old/Test/Framework/Runners/Console/Run.hs\t2012-01-20 11:09:22.000000000 -0800"
                      , "+++ new/Test/Framework/Runners/Console/Run.hs\t2012-01-20 11:34:42.187163011 -0800"
                      , "@@ -18,7 +18,7 @@"
                      , " "
                      , " import Text.PrettyPrint.ANSI.Leijen"
                      , " "
                      , "-import Data.Monoid"
                      , "+import Data.Monoid (Monoid(..))"
                      , " "
                      , " import Control.Arrow (second, (&&&))"
                      , " import Control.Monad (unless)" ]
                    , P.ExtraDep "libghc-random-prof" -} ]
    , debianize "test-framework-hunit" []
    , debianize "test-framework-quickcheck" []
    , debianize "test-framework-quickcheck2" []
    , debianize "test-framework-th" []
    , P.Package { P.name = "haskell-testpack"
                , P.spec = Debianize (Patch
                                      (Hackage "testpack")
                                      (unlines
                                       [ "--- old/src/Test/HUnit/Tools.hs\t2012-10-05 04:45:37.000000000 -0700"
                                       , "+++ new/src/Test/HUnit/Tools.hs\t2012-10-05 05:02:50.779290231 -0700"
                                       , "@@ -99,7 +99,7 @@"
                                       , " tests :: Args -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()"
                                       , " tests config gen rnd0 ntest nfail stamps"
                                       , "   | ntest == maxSuccess config = return ()"
                                       , "-  | nfail == maxDiscard config = assertFailure $ \"Arguments exhausted after \" ++ show ntest ++ \" tests.\""
                                       , "+  | nfail == maxDiscardRatio config = assertFailure $ \"Arguments exhausted after \" ++ show ntest ++ \" tests.\""
                                       , "   | otherwise               ="
                                       , "       do putStr (configEvery config ntest (arguments result))"
                                       , "          case ok result of"
                                       , "@@ -128,7 +128,7 @@"
                                       , " >        q \"Integer -> Int (safe bounds)\" prop_integer_to_int_pass]"
                                       , " -}"
                                       , " qc2hu :: QC.Testable a => Int -> String -> a -> HU.Test"
                                       , "-qc2hu maxTest = qccheck (stdArgs {maxSuccess = maxTest, maxDiscard = 20000})"
                                       , "+qc2hu maxTest = qccheck (stdArgs {maxSuccess = maxTest, maxDiscardRatio = 20000})"
                                       , " "
                                       , " {- | Run verbose tests.  Example:"
                                       , " "
                                       , "@@ -174,7 +174,7 @@"
                                       , "               Just (rnd,_) -> return rnd"
                                       , "      test MkState{ terminal          = tm"
                                       , "                  , maxSuccessTests   = maxSuccess args"
                                       , "-                 , maxDiscardedTests = maxDiscard args"
                                       , "+                 , maxDiscardedTests = maxDiscardRatio args"
                                       , "                  , computeSize       = case replay args of"
                                       , "                                          Nothing    -> \n d -> (n * maxSize args)"
                                       , "                                                          `div` maxSuccess args" ]))
                , P.flags = [] }
    , debianize "th-expand-syns" []
    , debianize "th-lift" []
    , debianize "transformers-base" [P.DebVersion "0.4.1-2"]
    , debianize "unicode-names" [P.DebVersion "3.2.0.0-1~hackage1"]
    , patched "unicode-properties"
                    [ P.DebVersion "3.2.0.0-1~hackage1" ]
                    (unlines
                          [ "--- haskell-unicode-properties-3.2.0.0/Data/Char/Properties/MiscData.hs~\t2011-12-04 10:25:17.000000000 -0800"
                          , "+++ haskell-unicode-properties-3.2.0.0/Data/Char/Properties/MiscData.hs\t2011-12-04 11:25:53.000000000 -0800"
                          , "@@ -1,4 +1,3 @@"
                          , "-{-# OPTIONS -fvia-C #-}"
                          , " module Data.Char.Properties.MiscData where"
                          , " {"
                          , " \timport Data.Char.Properties.PrivateData;"
                          , "--- haskell-unicode-properties-3.2.0.0/Data/Char/Properties/CaseData.hs~\t2011-12-04 10:25:17.000000000 -0800"
                          , "+++ haskell-unicode-properties-3.2.0.0/Data/Char/Properties/CaseData.hs\t2011-12-04 11:24:00.000000000 -0800"
                          , "@@ -1,4 +1,3 @@"
                          , "-{-# OPTIONS -fvia-C #-}"
                          , " module Data.Char.Properties.CaseData where"
                          , " {"
                          , " \timport Data.Map;" ])
    , debianize "uniplate" []
    -- , apt release "haskell-unix-compat"
    , debianize "cmdargs" []
    , P.Package { P.name = "haskell-language-javascript",
                  P.spec = Patch (Debianize (Hackage "language-javascript"))
                                 (unlines [ "--- language-javascript-0.5.4/debian/control~\t2012-07-29 17:42:20.055247337 -0700"
                                          , "+++ language-javascript-0.5.4/debian/control\t2012-07-29 18:20:09.983238576 -0700"
                                          , "@@ -7,6 +7,7 @@"
                                          , "                , cdbs"
                                          , "                , ghc"
                                          , "                , ghc-prof"
                                          , "+\t       , happy"
                                          , "                , libghc-array-dev (>= 0.3) | ghc"
                                          , "                , libghc-array-dev (<< 0.5) | ghc"
                                          , "                , libghc-array-prof (>= 0.3) | ghc-prof"
                                          ]),
                   P.flags = []
                 }
    , debianize "utf8-light" [P.DebVersion "0.4.0.1-2build1"]
    , debianize "language-haskell-extract" []
    , P.Package { P.name = "haskell-pretty-show", P.spec = (Debianize (Hackage "pretty-show")), P.flags = [] }
    , P.Package { P.name = "haskell-language-ecmascript"
                , P.spec = Debianize (Hackage "language-ecmascript")
                , P.flags = [] }
    , P.Package { P.name = "haskell-elm"
                , P.spec = Debianize (Patch
                                      (Hackage "Elm")
                                      (unlines
                                       [ "--- old/Elm.cabal\t2012-11-06 04:08:41.000000000 -0800"
                                       , "+++ new/Elm.cabal\t2012-11-06 04:20:54.966287740 -0800"
                                       , "@@ -69,7 +69,7 @@"
                                       , "                        text,"
                                       , "                        template-haskell,"
                                       , "                        shakespeare >= 1,"
                                       , "-                       pandoc <= 1.9.4.2,"
                                       , "+                       pandoc,"
                                       , "                        bytestring,"
                                       , "                        hjsmin"
                                       , " "
                                       , "@@ -110,6 +110,6 @@"
                                       , "                        blaze-markup == 0.5.1.*,"
                                       , "                        deepseq,"
                                       , "                        cmdargs,"
                                       , "-                       pandoc <= 1.9.4.2,"
                                       , "+                       pandoc,"
                                       , "                        bytestring,"
                                       , "                        hjsmin" ]))
                , P.flags = [] }
    , P.Package { P.name = "elm-server"
                , P.spec = Debianize (Patch
                                      (Hackage "elm-server")
                                      (unlines
                                       [ "--- old/elm-server.cabal\t2012-10-26 05:14:32.000000000 -0700"
                                       , "+++ new/elm-server.cabal\t2012-10-26 05:15:15.811759457 -0700"
                                       , "@@ -32,7 +32,7 @@"
                                       , "                        parsec >= 3.1.1,"
                                       , "                        blaze-html >= 0.5.1,"
                                       , "                        HTTP >= 4000,"
                                       , "-                       happstack-server == 7.0.2,"
                                       , "+                       happstack-server >= 7.0.2,"
                                       , "                        deepseq,"
                                       , "                        directory,"
                                       , "                        Elm >= 0.5.0" ]))
                , P.flags = [] }
    , debianize "hjsmin" []
    , debianize "unix-compat" []
    , debianize "Unixutils-shadow" []
    , debianize "unordered-containers" []
    , debianize "utf8-prelude" [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt "sid" "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize "unification-fd" []
    , P.Package { P.name = "haskell-logict"
                , P.spec = Debianize (Hackage "logict")
                , P.flags = [P.DebVersion "0.5.0.2-1~hackage1"] }
    , apt release "haskell-utility-ht"
    , debianize "vacuum" []
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt release "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" []
    , debianize "vector-algorithms" []
    , patched "virthualenv" []
                    (unlines
                     [ "--- old/virthualenv.cabal\t2012-09-05 22:50:47.809090845 -0700"
                     , "+++ new/virthualenv.cabal\t2012-09-05 22:49:55.521089609 -0700"
                     , "@@ -103,10 +103,10 @@"
                     , "                , filepath >= 1.1.0.3 && < 1.4"
                     , "                , directory >= 1.0.1.0 && < 1.2"
                     , "                , Cabal >= 1.8.0.6 && < 1.15"
                     , "-               , mtl >= 1.1.0.2 && < 2.1"
                     , "+               , mtl >= 1.1.0.2"
                     , "                , bytestring >= 0.9.1.7 && < 0.10"
                     , "                , file-embed >= 0.0.4.1 && < 0.1"
                     , "-               , split >= 0.1.4 && < 0.2"
                     , "+               , split >= 0.1.4"
                     , "                , safe >= 0.3 && < 0.4"
                     , " "
                     , "   Other-modules: Util.Cabal" ])
    , debianize "vault" []
    , P.Package { P.name = "haskell-wai"
                , P.spec = Debianize (Hackage "wai")
                , P.flags = [] }
    , patched "web-encodings" []
                    (unlines
                      [ "--- old/web-encodings.cabal\t2012-01-20 06:47:07.000000000 -0800"
                      , "+++ new/web-encodings.cabal\t2012-01-20 08:55:12.256222810 -0800"
                      , "@@ -22,7 +22,7 @@"
                      , "                      old-locale >= 1.0.0.1 && < 1.1,"
                      , "                      bytestring >= 0.9.1.4 && < 0.10,"
                      , "                      text >= 0.11 && < 0.12,"
                      , "-                     failure >= 0.0.0 && < 0.2,"
                      , "+                     failure >= 0.0.0 && < 0.3,"
                      , "                      directory >= 1 && < 1.2"
                      , "     exposed-modules: Web.Encodings"
                      , "                      Web.Encodings.MimeHeader," ])
    , debianize "boomerang" []
    , apt release "haskell-xml"
    , debianize "cookie" []
    , debianize "lifted-base" []
    , debianize "system-filepath" []
    , patched "xml-enumerator" []
                 (unlines
                   [ "--- old/xml-enumerator.cabal\t2012-01-20 06:47:15.000000000 -0800"
                   , "+++ new/xml-enumerator.cabal\t2012-01-20 09:54:58.246541244 -0800"
                   , "@@ -42,9 +42,9 @@"
                   , "                    , attoparsec                >= 0.10"
                   , "                    , blaze-builder             >= 0.2      && < 0.4"
                   , "                    , blaze-builder-enumerator  >= 0.2      && < 0.3"
                   , "-                   , transformers              >= 0.2      && < 0.3"
                   , "-                   , failure                   >= 0.1      && < 0.2"
                   , "-                   , data-default              >= 0.2      && < 0.4"
                   , "+                   , transformers              >= 0.2"
                   , "+                   , failure                   >= 0.1"
                   , "+                   , data-default              >= 0.2"
                   , "     exposed-modules: Text.XML.Stream.Parse"
                   , "                      Text.XML.Stream.Render"
                   , "                      Text.XML.Unresolved"
                   , "@@ -61,7 +61,7 @@"
                   , "     build-depends:          base                         >= 4               && < 5"
                   , "                           , containers                   >= 0.2             && < 0.5"
                   , "                           , text                                               < 1"
                   , "-                          , transformers                 >= 0.2             && < 0.3"
                   , "+                          , transformers                 >= 0.2"
                   , "                           , enumerator                   >= 0.4.14          && < 0.5"
                   , "                           , bytestring                   >= 0.9             && < 0.10"
                   , "                           , xml-enumerator               >= 0.4             && < 0.5" ])
    , debianize "xml-types" []
    , debianize "xss-sanitize" []
    , debianize "yaml-light" [P.DebVersion "0.1.4-2"]
    , apt release "haskell-zip-archive"
    , debianize "regex-pcre-builtin" [P.MapDep "pcre" (BinPkgName "libpcre3-dev") {-, P.ExtraDevDep "libpcre3-dev"-}]
    , P.Package { P.name = "hscolour"
                , P.spec = Apt "sid" "hscolour"
                , P.flags = [P.RelaxDep "hscolour"] }
    , P.Package { P.name = "hslogger"
                , P.spec = Patch (Apt "sid" "hslogger")
                                 (unlines [ "--- old/hslogger.cabal\t2011-03-07 07:22:05.000000000 -0800"
                                          , "+++ new/hslogger.cabal\t2012-09-17 20:54:27.381422435 -0700"
                                          , "@@ -42,7 +42,7 @@"
                                          , "         System.Log.Handler.Growl, System.Log.Handler.Log4jXML,"
                                          , "         System.Log.Logger"
                                          , "     Extensions: CPP, ExistentialQuantification"
                                          , "-    Build-Depends: network < 2.4, mtl"
                                          , "+    Build-Depends: network < 2.5, mtl"
                                          , "     if !os(windows)"
                                          , "         Build-Depends: unix"
                                          , "     if flag(small_base)" ])
                , P.flags = [] }
    , case release of
      "quantal-seereason" -> P.NoPackage -- This build hangs when performing tests
      _ -> apt release "html-xml-utils"
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
    , apt release "tinymce"
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
                , P.flags = [P.DebVersion "0.1.2-2"] }
{-
    -- Fails in lucid due to build deps
    , P.Package { P.name = "jbigkit"
                , P.spec = Apt "sid" "jbigkit"
                , P.flags = [] }
-}
    ]

ghc release =
  P.Packages (singleton "ghc") $
  case release of
    "quantal-seereason" -> [ghc76, devscripts]
    "precise-seereason" -> [devscripts]
    _ ->  [devscripts]
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
                  , P.spec = case release of
                               "quantal-seereason" -> Apt "experimental" "haskell-devscripts"
                               "precise-seereason" -> Patch (Apt "precise" "haskell-devscripts") devscripts0812Patch
                               _ -> Apt "sid" "haskell-devscripts"
                  , P.flags = [P.RelaxDep "python-minimal"] }
      -- haskell-devscripts-0.8.13 is for ghc-7.6 only
      devscripts0812Patch =
                (unlines [ "--- old/dh_haskell_depends\t2012-03-10 09:07:37.000000000 -0800"
                         , "+++ new/dh_haskell_depends\t2012-06-28 11:30:04.266489732 -0700"
                         , "@@ -102,6 +102,12 @@"
                         , "                     dev=`echo $pkg | sed -e 's/-prof$/-dev/'`"
                         , "                     version='(=${binary:Version})'"
                         , "                     depends=\"$dev ${version}, `depends_for_ghc_prof $cfiles`\""
                         , "+                    # If there is a doc package, add it to the"
                         , "+                    # Haskell:depends list of the prof package."
                         , "+                    doc=`echo $pkg | sed -e 's/-prof$/-doc/'`"
                         , "+                    for p in `dh_listpackages $args`; do"
                         , "+                        if [ \"$p\" = \"$doc\" ]; then depends=\"$doc ${version}, $depends\"; fi"
                         , "+                    done"
                         , " \t\t    echo \"haskell:Depends=$depends\" >> $sfile.tmp"
                         , "                     echo \"haskell:Recommends=\" >> $sfile.tmp"
                         , "                     echo \"haskell:Suggests=\" >> $sfile.tmp"
                         , "--- old/hlibrary.mk\t2012-01-04 11:06:45.000000000 -0800"
                         , "+++ new/hlibrary.mk\t2012-07-18 08:50:59.485188897 -0700"
                         , "@@ -135,15 +135,15 @@"
                         , " \t$(DEB_SETUP_BIN_NAME) copy --builddir=dist-ghc --destdir=debian/tmp-inst-ghc"
                         , " "
                         , " debian/extra-depends: debian/tmp-inst-ghc"
                         , "-\tpkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | sed -r 's,.*: ,,'` ; \\"
                         , "+\tpkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | tr -d ' \\n' | sed -r 's,^.*:,,'`; \\"
                         , " \t\tdh_haskell_extra_depends $$pkg_config ; \\"
                         , " \t\trm $$pkg_config"
                         , " "
                         , " install/libghc-$(CABAL_PACKAGE)-dev:: debian/tmp-inst-ghc debian/extra-depends"
                         , " \tcd debian/tmp-inst-ghc ; find usr/lib/haskell-packages/ghc/lib/ \\"
                         , "-\t\t\\( ! -name \"*_p.a\" ! -name \"*.p_hi\" \\) \\"
                         , "+\t\t! -type d \\( ! -name \"*_p.a\" ! -name \"*.p_hi\" \\) \\"
                         , " \t\t-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'"
                         , "-\tpkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | sed -r 's,.*: ,,'`; \\"
                         , "+\tpkg_config=`$(DEB_SETUP_BIN_NAME) register --builddir=dist-ghc --gen-pkg-config | tr -d ' \\n' | sed -r 's,^.*:,,'`; \\"
                         , " \t\t$(if $(HASKELL_HIDE_PACKAGES),sed -i 's/^exposed: True$$/exposed: False/' $$pkg_config;) \\"
                         , " \t\tinstall -Dm 644 $$pkg_config debian/$(notdir $@)/var/lib/ghc/package.conf.d/$$pkg_config; \\"
                         , " \t\trm -f $$pkg_config"
                         , "@@ -156,7 +156,7 @@"
                         , " "
                         , " install/libghc-$(CABAL_PACKAGE)-prof:: debian/tmp-inst-ghc install/libghc-$(CABAL_PACKAGE)-dev debian/extra-depends"
                         , " \tcd debian/tmp-inst-ghc ; find usr/lib/haskell-packages/ghc/lib/ \\"
                         , "-\t\t! \\( ! -name \"*_p.a\" ! -name \"*.p_hi\" \\) \\"
                         , "+\t\t! -type d ! \\( ! -name \"*_p.a\" ! -name \"*.p_hi\" \\) \\"
                         , " \t\t-exec install -Dm 644 '{}' ../$(notdir $@)/'{}' ';'"
                         , " \tdh_haskell_provides -p$(notdir $@)"
                         , " \tdh_haskell_depends -p$(notdir $@)"
                         ])
      -- This was required to build ghc-7.4.1 on ubuntu, but we don't use
      -- it because we don't build ghc-7.4.1, we just stick with the one
      -- we have.
      ghc741Patch = (unlines  [ "--- old/ghc-7.4.1/debian/control\t2012-03-10 10:38:09.000000000 -0800"
                              , "+++ new/ghc-7.4.1/debian/control\t2012-03-22 17:18:51.548720165 -0700"
                              , "@@ -6,7 +6,7 @@"
                              , " Standards-Version: 3.9.2"
                              , " Build-Depends:"
                              , "   debhelper (>= 7),"
                              , "-  libgmp-dev,"
                              , "+  libgmp-dev | libgmp3-dev,"
                              , "   devscripts,"
                              , "   ghc,"
                              , "   grep-dctrl,"
                              , "@@ -31,7 +31,7 @@"
                              , " "
                              , " Package: ghc"
                              , " Architecture: any"
                              , "-Depends: gcc (>= 4:4.2), llvm-3.0 [armel armhf], libgmp-dev, libffi-dev, libbsd-dev, libc6-dev, ${shlibs:Depends}, ${misc:Depends}"
                              , "+Depends: gcc (>= 4:4.2), llvm-3.0 [armel armhf], libgmp-dev | libgmp3-dev, libffi-dev, libbsd-dev, libc6-dev, ${shlibs:Depends}, ${misc:Depends}"
                              , " Provides: haskell-compiler, ${provided-devs}, ${haskell:Provides}, ${ghci}"
                              , " Replaces: ghc6 (<< 7)"
                              , " Conflicts: ghc6 (<< 7), ${provided-devs}" ])

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
    , apt release "haskell-zlib"
    -- , apt "haskell-deepseq"
    , case release of
        "natty-seereason" -> P.NoPackage -- a version newer than the latest in hackage is bundled with ghc
        _ -> P.NoPackage -- debianize "deepseq" []

    , case release of
        "precise-seereason" -> debianize "mtl" []
        _ -> P.Package { P.name = "haskell-mtl"
                       , P.spec = Apt "sid" "haskell-mtl"
                       , P.flags = [{-P.AptPin "2.0.1.0-3"-}] }
    , case release of
        "precise-seereason" -> debianize "transformers" []
        _ -> P.Package { P.name = "haskell-transformers"
                       , P.spec = Apt "sid" "haskell-transformers"
                       , P.flags = [{-P.AptPin "0.2.2.0-3"-}] }
    , debianize "parallel" []
    , debianize "syb" []
    , debianize "fgl" [P.DebVersion "5.4.2.4-2"]
    , debianize "text" []
    , P.Package { P.name = "alex"
                , P.spec = Apt "sid" "alex"
                , P.flags = [P.RelaxDep "alex"] }
    , opengl release
    -- , haddock release
    , debianize "haskell-src" [ P.ExtraDep "happy", P.DebVersion "1.0.1.5-1" ]
    , debianize "network" []
    , debianize "HTTP" [P.DebVersion "1:4000.2.3-1~hackage1"]
    , P.Package { P.name = "haskell-cgi"
                , P.spec = Debianize (Hackage "cgi")
                -- , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian")
                , P.flags = [P.DebVersion "3001.1.8.3-1~hackage1"] }
    -- This is bundled with the compiler
    -- , debianize "process" []
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" [P.DebVersion "1.0.1.1-1"]
    , debianize "HUnit" []
    , debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]
    , apt release "haskell-parsec"
    , apt release "haskell-html"
    , apt release "haskell-regex-compat"
    , apt release "haskell-regex-base"
    , apt release "haskell-regex-posix"
    , debianize "xhtml" []
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
                                          (unlines
                                           [ "--- old/clckwrks.cabal\t2012-08-22 10:37:00.000000000 -0700"
                                           , "+++ new/clckwrks.cabal\t2012-08-22 10:37:50.000000000 -0700"
                                           , "@@ -15,6 +15,79 @@"
                                           , " Cabal-version:       >=1.6"
                                           , " Data-Files:"
                                           , "     static/admin.css"
                                           , "+    jstree/_lib/jquery.hotkeys.js"
                                           , "+    jstree/_lib/jquery.js"
                                           , "+    jstree/_lib/jquery.cookie.js"
                                           , "+    jstree/jquery.jstree.js"
                                           , "+    jstree/_demo/_inc/class._database.php"
                                           , "+    jstree/_demo/_inc/class.tree.php"
                                           , "+    jstree/_demo/_inc/class._database_i.php"
                                           , "+    jstree/_demo/_inc/__mysql_errors.log"
                                           , "+    jstree/_demo/_install.txt"
                                           , "+    jstree/_demo/file.png"
                                           , "+    jstree/_demo/index.html"
                                           , "+    jstree/_demo/_dump.sql"
                                           , "+    jstree/_demo/config.php"
                                           , "+    jstree/_demo/root.png"
                                           , "+    jstree/_demo/server.php"
                                           , "+    jstree/_demo/folder.png"
                                           , "+    jstree/_docs/cookies.html"
                                           , "+    jstree/_docs/unique.html"
                                           , "+    jstree/_docs/_json_data.json"
                                           , "+    jstree/_docs/json_data.html"
                                           , "+    jstree/_docs/_html_data.html"
                                           , "+    jstree/_docs/sort.html"
                                           , "+    jstree/_docs/html_data.html"
                                           , "+    jstree/_docs/themeroller.html"
                                           , "+    jstree/_docs/_drive.png"
                                           , "+    jstree/_docs/xml_data.html"
                                           , "+    jstree/_docs/index.html"
                                           , "+    jstree/_docs/hotkeys.html"
                                           , "+    jstree/_docs/languages.html"
                                           , "+    jstree/_docs/ui.html"
                                           , "+    jstree/_docs/!style.css"
                                           , "+    jstree/_docs/checkbox.html"
                                           , "+    jstree/_docs/_search_data.json"
                                           , "+    jstree/_docs/syntax/page_white_code.png"
                                           , "+    jstree/_docs/syntax/wrapping.png"
                                           , "+    jstree/_docs/syntax/page_white_copy.png"
                                           , "+    jstree/_docs/syntax/!script.js"
                                           , "+    jstree/_docs/syntax/printer.png"
                                           , "+    jstree/_docs/syntax/!style.css"
                                           , "+    jstree/_docs/syntax/magnifier.png"
                                           , "+    jstree/_docs/syntax/clipboard.swf"
                                           , "+    jstree/_docs/syntax/help.png"
                                           , "+    jstree/_docs/_search_result.json"
                                           , "+    jstree/_docs/crrm.html"
                                           , "+    jstree/_docs/types.html"
                                           , "+    jstree/_docs/core.html"
                                           , "+    jstree/_docs/dnd.html"
                                           , "+    jstree/_docs/logo.png"
                                           , "+    jstree/_docs/search.html"
                                           , "+    jstree/_docs/contextmenu.html"
                                           , "+    jstree/_docs/themes.html"
                                           , "+    jstree/_docs/_xml_nest.xml"
                                           , "+    jstree/_docs/_xml_flat.xml"
                                           , "+    jstree/themes/default/d.gif"
                                           , "+    jstree/themes/default/d.png"
                                           , "+    jstree/themes/default/style.css"
                                           , "+    jstree/themes/default/throbber.gif"
                                           , "+    jstree/themes/apple/d.png"
                                           , "+    jstree/themes/apple/style.css"
                                           , "+    jstree/themes/apple/dot_for_ie.gif"
                                           , "+    jstree/themes/apple/bg.jpg"
                                           , "+    jstree/themes/apple/throbber.gif"
                                           , "+    jstree/themes/default-rtl/d.gif"
                                           , "+    jstree/themes/default-rtl/d.png"
                                           , "+    jstree/themes/default-rtl/dots.gif"
                                           , "+    jstree/themes/default-rtl/style.css"
                                           , "+    jstree/themes/default-rtl/throbber.gif"
                                           , "+    jstree/themes/classic/d.gif"
                                           , "+    jstree/themes/classic/d.png"
                                           , "+    jstree/themes/classic/style.css"
                                           , "+    jstree/themes/classic/dot_for_ie.gif"
                                           , "+    jstree/themes/classic/throbber.gif"
                                           , "+    json2/json2.js"
                                           , " "
                                           , " Library"
                                           , "   Build-tools:     trhsx" ]))
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
                                          (Cd "clckwrks-dot-com" (Darcs repo))
                                          (unlines
                                           [ "--- old/clckwrks-dot-com.cabal\t2012-12-06 08:47:42.000000000 -0800"
                                           , "+++ new/clckwrks-dot-com.cabal\t2012-12-06 08:49:46.069545349 -0800"
                                           , "@@ -12,7 +12,7 @@"
                                           , " "
                                           , " Flag backups"
                                           , "      Description: enable the backups executable (currently disabled by default do to wacky dependencies not on hackage)"
                                           , "-     Default: False"
                                           , "+     Default: True"
                                           , " "
                                           , " Executable             clckwrks-dot-com-server"
                                           , "   main-is:             Main.hs" ]))
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
                , P.spec = Debianize (Patch
                                      (Hackage "happstack-fay")
                                      (unlines
                                       [ "--- old/happstack-fay.cabal\t2012-11-24 06:12:24.000000000 -0800"
                                       , "+++ new/happstack-fay.cabal\t2012-11-24 06:33:36.694192699 -0800"
                                       , "@@ -23,7 +23,7 @@"
                                       , "                        Happstack.Fay,"
                                       , "                        Paths_happstack_fay"
                                       , "   build-depends:       base             > 4 && <5,"
                                       , "-                       fay              == 0.9.*,"
                                       , "+                       fay              >= 0.9,"
                                       , "                        fay-jquery       == 0.1.*,"
                                       , "                        aeson            == 0.6.*,"
                                       , "                        happstack-server >= 7.0 && <7.2" ]))
                , P.flags = [] }
    , P.Package { P.name = "haskell-fay-jquery"
                , P.spec = Debianize (Git "https://github.com/faylang/fay-jquery.git")
                , P.flags = [] }
    , P.Package { P.name = "mastermind"
                , P.spec = Debianize (Patch
                                      (Darcs "http://hub.darcs.net/stepcut/mastermind")
                                      (unlines
                                       [ "--- old/mastermind.cabal\t2012-11-26 12:11:40.277002455 -0800"
                                       , "+++ new/mastermind.cabal\t2012-11-26 12:11:32.241002885 -0800"
                                       , "@@ -22,7 +22,7 @@"
                                       , "                      acid-state       == 0.8.*,"
                                       , "                      aeson            == 0.6.*,"
                                       , "                      containers       == 0.4.*,"
                                       , "-                     fay              == 0.9.*,"
                                       , "+                     fay              >= 0.9,"
                                       , "                      fay-jquery       == 0.1.*,"
                                       , "                      happstack-fay    == 0.1.*,"
                                       , "                      happstack-server == 7.0.*,"
                                       , "--- old/Setup.hs\t2012-11-26 11:17:28.000000000 -0800"
                                       , "+++ new/Setup.hs\t2012-11-26 19:33:40.367586489 -0800"
                                       , "@@ -8,6 +8,7 @@"
                                       , " import Distribution.PackageDescription"
                                       , " import Paths_fay_jquery    as FayJQuery"
                                       , " import Paths_happstack_fay as HappstackFay"
                                       , "+import System.FilePath ((</>))"
                                       , " "
                                       , " main = defaultMainWithHooks userHooks"
                                       , " "
                                       , "@@ -26,7 +26,7 @@"
                                       , "     do fayQueryDataDir     <- liftIO $ FayJQuery.getDataDir"
                                       , "        happstackFayDataDir <- liftIO $ HappstackFay.getDataDir"
                                       , "        runDbProgram normal fayProgram (withPrograms localBuildInfo)"
                                       , "-          [ \"--include\", intercalate \",\" [fayQueryDataDir, happstackFayDataDir]"
                                       , "+          [ \"--include\", intercalate \",\" [fayQueryDataDir </> \"src\", happstackFayDataDir]"
                                       , "           , \"MasterMind/Client/Main.hs\""
                                       , "           ]"
                                       , "        return ()"
                                       , "--- old/MasterMind/Client/Main.hs\t2012-11-26 11:17:28.000000000 -0800"
                                       , "+++ new/MasterMind/Client/Main.hs\t2012-11-26 19:59:51.999502576 -0800"
                                       , "@@ -12,7 +12,7 @@"
                                       , " main ="
                                       , "     ready $ do"
                                       , "       submitButton <- select \"#guess\""
                                       , "-      click submitGuess submitButton"
                                       , "+      click_ submitGuess submitButton"
                                       , "       pegs <- select \".peg\""
                                       , "       (flip each) pegs (\\index pegElem ->"
                                       , "           do peg     <- selectElement pegElem"
                                       , "@@ -21,7 +21,7 @@"
                                       , "              (flip each) buttons $ (\\buttonIndex button ->"
                                       , "                do b <- selectElement button"
                                       , "                   color <- getAttr \"data-color\" b"
                                       , "-                  click (setPegColor peg color) b"
                                       , "+                  click_ (setPegColor peg color) b"
                                       , "                   return True)"
                                       , "              return True)"
                                       , "       call \"/ajax\" FetchBoard $ \\mboard ->"
                                       , "@@ -90,14 +90,14 @@"
                                       , " "
                                       , " updateBoard :: Row -> Fay ()"
                                       , " updateBoard (Row (Guess peg1 peg2 peg3 peg4) (Score score1 score2 score3 score4)) ="
                                       , "-    do html <- tr [] $ [ td [return (\"class\", \"peg\")] [span [return (\"class\", pegColor peg1)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", pegColor peg2)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", pegColor peg3)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", pegColor peg4)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", scoreColor score1)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", scoreColor score2)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", scoreColor score3)] []]"
                                       , "-                       , td [return (\"class\", \"peg\")] [span [return (\"class\", scoreColor score4)] []]"
                                       , "+    do html <- tr [] $ [ td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", pegColor peg1)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", pegColor peg2)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", pegColor peg3)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", pegColor peg4)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", scoreColor score1)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", scoreColor score2)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", scoreColor score3)] []]"
                                       , "+                       , td [return (\"class\", \"peg\")] [genElement \"span\" [return (\"class\", scoreColor score4)] []]"
                                       , "                        ]"
                                       , "        jq <- renderHTML html"
                                       , "        row0 <- select \"#row0\""
                                       , "@@ -125,7 +125,7 @@"
                                       , " "
                                       , " mkClick :: JQuery -> Fay JQuery"
                                       , " mkClick jq ="
                                       , "-    click (alert \"you clicked it\") jq"
                                       , "+    click_ (alert \"you clicked it\") jq"
                                       , " "
                                       , " "
                                       , " modal :: JQuery -> Fay ()"
                                       , "@@ -134,13 +134,13 @@"
                                       , " ignore :: () -> Fay ()"
                                       , " ignore _ = return ()"
                                       , " "
                                       , "-data Placement = Top | Bottom | Left | Right"
                                       , "+data Placement = Top | Bottom | Left_ | Right_"
                                       , " "
                                       , " placementStr :: Placement -> String"
                                       , " placementStr Top    = \"top\""
                                       , " placementStr Bottom = \"bottom\""
                                       , "-placementStr Left   = \"left\""
                                       , "-placementStr Right  = \"right\""
                                       , "+placementStr Left_  = \"left\""
                                       , "+placementStr Right_ = \"right\""
                                       , " "
                                       , " data Trigger = Click | Hover | Focus | Manual"
                                       , " "
                                       , "@@ -166,7 +166,7 @@"
                                       , " popoverOptions = PopoverOptions"
                                       , "     { popoverAnimation = True"
                                       , "     , popoverHtml      = True"
                                       , "-    , popoverPlacement = Right"
                                       , "+    , popoverPlacement = Right_"
                                       , "     , popoverSelector  = False"
                                       , "     , popoverTrigger   = Click"
                                       , "     , popoverTitle     = \"\""
                                       , "@@ -181,8 +181,8 @@"
                                       , " popover_ :: JQuery -> Bool -> Bool -> String -> Bool -> String -> String -> (() -> Fay String) -> Int -> Fay JQuery"
                                       , " popover_ = ffi \"%1.popover({animation: %2, html : %3, placement : %4, selector : %5, trigger : %6, title : %7, content : %8, delay : %9})\""
                                       , " "
                                       , "-click :: Fay () -> JQuery -> Fay JQuery"
                                       , "-click = ffi \"%2.click(%1)\""
                                       , "+click_ :: Fay () -> JQuery -> Fay JQuery"
                                       , "+click_ = ffi \"%2.click(%1)\""
                                       , " "
                                       , " alert :: String -> Fay ()"
                                       , " alert = ffi \"window.alert(%1)\""
                                       ]))
                , P.flags = [P.CabalDebian ["--build-dep=haskell-fay-utils", "--build-dep=haskell-fay-jquery-utils", "--build-dep=haskell-happstack-fay-utils"]] }
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = Debianize (Patch
                                      (Hackage "happstack-data")
                                      (unlines
                                       [ "--- old/happstack-data.cabal\t2012-04-16 09:55:33.000000000 -0700"
                                       , "+++ new/happstack-data.cabal\t2012-04-16 10:38:09.015673204 -0700"
                                       , "@@ -68,7 +68,7 @@"
                                       , "   Build-Depends:      binary,"
                                       , "                       bytestring,"
                                       , "                       containers,"
                                       , "-                      mtl >= 1.1 && < 2.1,"
                                       , "+                      mtl >= 1.1,"
                                       , "                       pretty,"
                                       , "                       syb-with-class-instances-text,"
                                       , "                       text >= 0.10 && < 0.12," ]))
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
                , P.spec = Debianize (Patch
                                      (Hackage "happstack-util")
                                      (unlines
                                       [ "--- old/happstack-util.cabal\t2012-04-17 05:38:25.000000000 -0700"
                                       , "+++ new/happstack-util.cabal\t2012-04-17 05:53:09.430600945 -0700"
                                       , "@@ -29,7 +29,7 @@"
                                       , "                        directory,"
                                       , "                        extensible-exceptions, "
                                       , "                        hslogger >= 1.0.2,"
                                       , "-                       mtl >= 1.1 && < 2.1,"
                                       , "+                       mtl >= 1.1,"
                                       , "                        old-locale,"
                                       , "                        old-time,"
                                       , "                        parsec < 4," ]))
                , P.flags = [P.DebVersion "6.0.3-1"] }
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , P.Package { P.name = "haskell-hsp"
                , P.spec = Debianize (Hackage "hsp")
                , P.flags = [ P.ExtraDep "haskell-hsx-utils", P.DebVersion "0.7.1-1~hackage1" ] }
    , P.Package { P.name = "haskell-hsx"
                , P.spec = Debianize (Hackage "hsx")
                , P.flags = [P.DebVersion "0.10.4-1~hackage1"] }
    -- This is pinned because version 1.9.4.4 failed to build.  I'm not sure
    -- if this is a bug that will be resolved in future pandocs or if it is
    -- something we need to fix.
    , P.Package { P.name = "haskell-pandoc"
                , P.spec = Debianize (Patch
                                      (Hackage "pandoc")
                                      (unlines
                                       [ "--- old/Setup.hs\t2012-11-22 06:10:25.000000000 -0800"
                                       , "+++ new/Setup.hs\t2012-11-23 06:02:06.600907904 -0800"
                                       , "@@ -33,10 +33,10 @@"
                                       , " "
                                       , " -- | Build man pages from markdown sources in man/"
                                       , " makeManPages :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()"
                                       , "-makeManPages _ flags _ _ = do"
                                       , "+makeManPages _ flags _ lbi = do"
                                       , "   let verbosity = fromFlag $ buildVerbosity flags"
                                       , "   let args = [\"--verbose\" | verbosity /= silent]"
                                       , "-  rawSystem (\"dist\" </> \"build\" </> \"make-pandoc-man-pages\" </> \"make-pandoc-man-pages\")"
                                       , "+  rawSystem (buildDir lbi </> \"make-pandoc-man-pages\" </> \"make-pandoc-man-pages\")"
                                       , "       args >>= exitWith"
                                       , " "
                                       , " manpages :: [FilePath]"
                                       , "--- old/pandoc.cabal\t2012-11-22 05:21:28.000000000 -0800"
                                       , "+++ new/pandoc.cabal\t2012-11-22 05:26:16.569050073 -0800"
                                       , "@@ -189,7 +189,7 @@"
                                       , "   Default:       True"
                                       , " Flag blaze_html_0_5"
                                       , "   Description:   Use blaze-html 0.5 and blaze-markup 0.5"
                                       , "-  Default:       False"
                                       , "+  Default:       True"
                                       , " "
                                       , " Library"
                                       , "   -- Note: the following is duplicated in all stanzas."
                                       , "--- old/src/Text/Pandoc/Readers/Markdown.hs\t2012-12-05 20:20:58.459944135 -0800"
                                       , "+++ new/src/Text/Pandoc/Readers/Markdown.hs\t2012-12-06 12:44:30.744793336 -0800"
                                       , "@@ -183,7 +183,7 @@"
                                       , "   st <- getState"
                                       , "   let firstPassParser = referenceKey"
                                       , "                      <|> (if stateStrict st then pzero else noteBlock)"
                                       , "-                     <|> liftM snd (withRaw codeBlockDelimited)"
                                       , "+                     -- <|> liftM snd (withRaw codeBlockDelimited)"
                                       , "                      <|> lineClump"
                                       , "   docMinusKeys <- liftM concat $ manyTill firstPassParser eof"
                                       , "   setInput docMinusKeys"
                                       , "@@ -299,13 +299,11 @@"
                                       , "                    , blockQuote"
                                       , "                    , hrule"
                                       , "                    , bulletList"
                                       , "-                   , orderedList"
                                       , "                    , htmlBlock"
                                       , "                    , para"
                                       , "                    , plain"
                                       , "                    , nullBlock ]"
                                       , "-              else [ codeBlockDelimited"
                                       , "-                   , macro"
                                       , "+              else [ macro"
                                       , "                    , header "
                                       , "                    , table"
                                       , "                    , codeBlockIndented"
                                       , "@@ -313,7 +311,6 @@"
                                       , "                    , blockQuote"
                                       , "                    , hrule"
                                       , "                    , bulletList"
                                       , "-                   , orderedList"
                                       , "                    , definitionList"
                                       , "                    , rawTeXBlock"
                                       , "                    , para"
                                       , "@@ -922,7 +919,7 @@"
                                       , " inlineParsers = [ whitespace"
                                       , "                 , str"
                                       , "                 , endline"
                                       , "-                , code"
                                       , "+                -- , code"
                                       , "                 , fours"
                                       , "                 , strong"
                                       , "                 , emph" ]))
                , P.flags = []
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
    , apt release "haskell-blaze-builder"
    , P.Package { P.name = "haskell-blaze-builder-enumerator"
                , P.spec = Debianize (Hackage "blaze-builder-enumerator")
                , P.flags = [P.DebVersion "0.2.0.5-1~hackage1"] }
    , debianize "blaze-from-html" []
    , debianize "blaze-html" []
    , debianize "blaze-textual" []
    , P.Package { P.name = "haskell-blaze-textual-native"
                , P.spec = Debianize (Patch
                                      (Hackage "blaze-textual-native")
                                      (unlines
                                       [ "--- x/blaze-textual-native.cabal.orig\t2012-01-01 12:22:11.676481147 -0800"
                                       , "+++ x/blaze-textual-native.cabal\t2012-01-01 12:22:21.716482151 -0800"
                                       , "@@ -66,7 +66,7 @@"
                                       , " "
                                       , "   if impl(ghc >= 6.11)"
                                       , "     cpp-options: -DINTEGER_GMP"
                                       , "-    build-depends: integer-gmp >= 0.2 && < 0.4"
                                       , "+    build-depends: integer-gmp >= 0.2"
                                       , " "
                                       , "   if impl(ghc >= 6.9) && impl(ghc < 6.11)"
                                       , "     cpp-options: -DINTEGER_GMP"
                                       , "--- x/Blaze/Text/Int.hs.orig\t2012-01-01 12:45:05.136482154 -0800"
                                       , "+++ x/Blaze/Text/Int.hs\t2012-01-01 12:45:26.016482025 -0800"
                                       , "@@ -40,7 +40,7 @@"
                                       , " # define PAIR(a,b) (a,b)"
                                       , " #endif"
                                       , " "
                                       , "-integral :: Integral a => a -> Builder"
                                       , "+integral :: (Integral a, Show a) => a -> Builder"
                                       , " {-# RULES \"integral/Int\" integral = bounded :: Int -> Builder #-}"
                                       , " {-# RULES \"integral/Int8\" integral = bounded :: Int8 -> Builder #-}"
                                       , " {-# RULES \"integral/Int16\" integral = bounded :: Int16 -> Builder #-}" ]))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , P.Package { P.name = "clckwrks-theme-happstack"
                , P.spec = Debianize (Cd "clckwrks-theme-happstack" (Darcs (repo ++ "/happstack-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "happstack-dot-com"
                , P.spec = Debianize (Patch
                                      (Cd "happstack-dot-com" (Darcs (repo ++ "/happstack-clckwrks")))
                                      (unlines
                                       [ "--- old/happstack-dot-com.cabal\t2012-12-06 08:32:56.000000000 -0800"
                                       , "+++ new/happstack-dot-com.cabal\t2012-12-06 08:40:39.773574517 -0800"
                                       , "@@ -18,7 +18,7 @@"
                                       , " "
                                       , " Flag backups"
                                       , "      Description: enable the backups executable (currently disabled by default do to wacky dependencies not on hackage)"
                                       , "-     Default: False"
                                       , "+     Default: True"
                                       , " "
                                       , " Executable happstack-dot-com-server"
                                       , "   Main-is: Main.hs" ]))
                , P.flags = [] }
    , P.Package { P.name = "haskell-acid-state"
                , P.spec = Debianize (Hackage "acid-state")
                , P.flags = [P.DebVersion "0.8.1-1~hackage1"] }
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
    , apt release "haskell-puremd5"
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
opengl _release = P.Packages (singleton "opengl") $
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
                , P.spec = Patch (Debianize (Hackage "bitmap-opengl"))
                                 (unlines
                                  [ "--- old/Data/Bitmap/OpenGL.hs\t2012-03-25 07:48:39.000000000 -0700"
                                  , "+++ new/Data/Bitmap/OpenGL.hs\t2012-03-25 09:07:54.849175710 -0700"
                                  , "@@ -11,6 +11,7 @@"
                                  , " --------------------------------------------------------------------------------"
                                  , " "
                                  , " import Data.Bitmap.IO"
                                  , "+import Data.Bitmap.Simple (withBitmap)"
                                  , " "
                                  , " import Graphics.Rendering.OpenGL"
                                  , " " ])
                , P.flags = [P.ExtraDep "libglu1-mesa-dev", P.DebVersion "0.0.0-1~hackage1"] }
    , debianize "GLUT" []
    , debianize "StateVar" [P.DebVersion "1.0.0.0-2build1"]
    , debianize "Tensor" []
    , debianize "GLURaw" []
    , debianize "ObjectName" []
    , debianize "OpenGLRaw" [ P.ExtraDep "libgl1-mesa-dev" ]
    ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib release = P.Packages (singleton "glib") $
    [ debianize "glib" [ P.ExtraDep "haskell-gtk2hs-buildtools-utils"
                       , P.ExtraDep "libglib2.0-dev"]
    , apt release "haskell-criterion"
    , apt release "haskell-ltk"
    , apt release "haskell-chart"
    , apt release "haskell-gio"
    , apt release "haskell-gtk"
    , apt release "haskell-gtksourceview2"
    , apt release "haskell-pango" ]

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

haddock release =
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base ==
    -- 4.3.* so we can't use "debianize "haddock" []".  I don't think
    -- we really need this, or the hackage version.  Version 2.10.0 is
    -- included with ghc 7.4.0.
    [ apt release "haskell-haddock" ]

-- These have been failing for some time, and I don't think we've missed them.
failing release =
    [ debianize "funsat" []
    , apt release "haskell-statistics" ]

algebra = P.Packages (singleton "algebra")
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
    , debianize "contravariant" []
    , debianize "distributive" []
    , P.Package { P.name = "free"
                , P.spec = Debianize (Hackage "free")
                , P.flags = [P.DebVersion "3.2-1~hackage1"] }
    , debianize "keys" []
    , debianize "lens" []
    , debianize "lens-family-core" []
    , debianize "lens-family" []
    , debianize "lens-family-th" []
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
agda release =
    [ apt release "agda"
    , apt release "agda-bin"
    , apt release "agda-stdlib" ]

other release =
    [ apt release "darcs"
    , patched "aeson-native" []
                    (unlines
                      [ "--- old-aeson-native/aeson-native.cabal\t2011-12-03 08:17:32.000000000 -0800"
                      , "+++ new-aeson-native/aeson-native.cabal\t2012-01-02 12:33:12.776486492 -0800"
                      , "@@ -119,7 +119,7 @@"
                      , "     blaze-textual-native >= 0.2.0.2,"
                      , "     bytestring,"
                      , "     containers,"
                      , "-    deepseq < 1.2,"
                      , "+    deepseq < 1.3,"
                      , "     hashable >= 1.1.2.0,"
                      , "     mtl,"
                      , "     old-locale," ])
    , apt release "haskell-binary-shared" -- for leksah
    , debianize "cairo" [P.ExtraDep "haskell-gtk2hs-buildtools-utils"] -- for leksah
    , debianize "cabal-dev" [] -- build-env for cabal
    , debianize "gnuplot" [P.DebVersion "0.4.2-1~hackage1"]
    , apt release "bash-completion"
    ]

apt :: String -> String -> P.Packages
apt release name =
          let dist =
                  case release of
                    -- Several packages in oneiric are newer looking than the ones in sid
                    "oneiric-seereason" ->
                        case name of
                          "haskell-opengl" -> "oneiric"
                          "haskell-mtl" -> "oneiric"
                          "haskell-utility-ht" -> "oneiric"
                          "haskell-utf8-string" -> "oneiric"
                          "haskell-deepseq" -> "oneiric"
                          "haskell-dlist" -> "oneiric"
                          "haskell-sha" -> "oneiric"
                          _ -> "sid"
                    _ -> "sid"
              version =
                  case release of
                    "oneiric-seereason" ->
                        case name of
                          "haskell-deepseq" -> Just "1.1.0.2-2"
                          _ -> Nothing
                    _ ->
                        case name of
                          "haskell-deepseq" -> Just "1.1.0.2-2"
                          _ -> Nothing in
          P.Package
               { P.name = name
               , P.spec = Apt dist name
               , P.flags = (maybe [] (\ v -> [P.AptPin v]) version) }

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

patched :: String -> [P.PackageFlag] -> String -> P.Packages
patched s flags patch =
    let p = debianize s flags in
    let (Debianize (Hackage s)) = P.spec p in
    p {P.spec = Debianize (Patch (Hackage s) patch)}
