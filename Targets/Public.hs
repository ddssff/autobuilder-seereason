{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Public ( targets ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec (Spec(..))
import Targets.Common (repo, localRepo, checkUnique, happstackRepo)

-- |Sometimes we don't want to build the core packages even if newer
-- versions are available, because it takes so long.
-- ring0 "lucid-seereason" p = any (== (P.name p)) ["ghc", "haskell-devscripts", "haskell-dummy", "hscolour"]
ring0 _ _ = False

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> [P.Package]
targets _home release = checkUnique $ filter (not . ring0 release) $
    -- Alphabetized by name
    [ apt "agda"
    , apt "agda-bin"
    , apt "agda-stdlib"
    , P.Package { P.name = "autobuilder"
                , P.spec = Darcs (repo ++ "/autobuilder") Nothing
                , P.flags = [] }
    , apt "bash-completion"
    , P.Package { P.name = "haskell-cabal-debian"
                , P.spec = Darcs (repo ++ "/cabal-debian") Nothing
                , P.flags = [] }
    , P.Package { P.name = "cpphs"
                , P.spec = Quilt (Apt "sid" "cpphs" Nothing) (Darcs "http://src.seereason.com/cpphs-quilt" Nothing)
                , P.flags = [] }
    , apt "darcs"
    , apt "debootstrap"
    , apt "geneweb"
    , P.Package { P.name = "ghc"
                , P.spec = case release of
                             -- The server still hangs with experimental compiler version 7.2.2.
                             -- The patch in ghc7-quilt makes libgmp3-dev (for ubuntu) an alternative dependency to libgmp-dev
                             -- "natty-seereason" -> Quilt (Apt "experimental" "ghc" Nothing) (Darcs (repo ++ "/ghc7-quilt") Nothing)
                             _ -> Apt "sid" "ghc" Nothing
                , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] }
    , debianize "gtk2hs-buildtools" Newest
                                    [P.ExtraDep "alex",
                                     P.ExtraDep "happy",
                                     P.Patch . B.pack . unlines $
                                          [ "--- tmp/gtk2hs-buildtools.cabal.orig\t2011-09-15 16:29:46.000000000 -0700"
                                          , "+++ tmp/gtk2hs-buildtools.cabal\t2011-09-15 17:15:50.008424830 -0700"
                                          , "@@ -41,6 +41,7 @@"
                                          , "         hs-source-dirs: hierarchyGen"
                                          , "         other-modules:  Paths_gtk2hs_buildtools"
                                          , "         build-depends:  base"
                                          , "+        GHC-options:    -XBangPatterns"
                                          , " "
                                          , " Executable gtk2hsHookGenerator"
                                          , "         main-is:        HookGenerator.hs"
                                          , "@@ -49,6 +50,7 @@"
                                          , "           cpp-options:  -DUSE_GCLOSURE_SIGNALS_IMPL"
                                          , "         other-modules:  Paths_gtk2hs_buildtools"
                                          , "         build-depends:  base"
                                          , "+        GHC-options:    -XBangPatterns"
                                          , " "
                                          , " Executable gtk2hsC2hs"
                                          , "         main-is:        Main.hs"
                                          , "@@ -114,3 +116,4 @@"
                                          , "         else"
                                          , "           cpp-options:  -D_C2HS_CPP_LANG_SINGLE"
                                          , "         extensions:     ForeignFunctionInterface"
                                          , "+        GHC-options:    -XBangPatterns" ]]
    , P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization" Nothing
                , P.flags = [] }
    , -- Hackage target doesn't build with our
      -- debianization due to funky bootstrapping files in
      -- dist/build/happy/happy-tmp.
      -- (hackage release "happy" [])
      P.Package { P.name = "happy"
                , P.spec = Quilt (Apt "sid" "happy" Nothing) (Darcs (repo ++ "/happy-quilt") Nothing)
                , P.flags = [ P.RelaxDep "happy" ] }
    , debianize "acid-state" Newest []
    , debianize "AES" Newest [P.DebVersion "0.2.8-1~hackage1"]
    , debianize "aeson" Newest []
    , debianize "aeson-native" Newest [P.DebVersion "0.3.3.1-1~hackage1"]
    , P.Package { P.name = "haskell-agi"
                , P.spec=Darcs "http://src.seereason.com/haskell-agi" Nothing
                , P.flags = [] }
    , debianize "ansi-terminal" Newest [P.DebVersion "0.5.5-1"]
    , debianize "ansi-wl-pprint" Newest [P.DebVersion "0.6.3-1~hackage1"]
    -- Our debianization has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = DebDir (Hackage "applicative-extras" Nothing)
                                  (Darcs "http://src.seereason.com/applicative-extras-debian" Nothing),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive" Nothing
                , P.flags = [] }
    , debianize "asn1-data" Newest []
    , debianize "attempt" Newest [P.DebVersion "0.3.1.1-1~hackage1"]
    , debianize "attoparsec" Newest []
    , debianize "attoparsec-enumerator" Newest []
    , debianize "attoparsec-text" Newest [P.Patch . B.pack . unlines $
                                   [ "--- x/attoparsec-text.cabal.orig\t2011-11-28 20:42:48.000000000 -0800"
                                   , "+++ x/attoparsec-text.cabal\t2011-11-28 21:55:25.742078566 -0800"
                                   , "@@ -59,7 +59,7 @@"
                                   , " "
                                   , " library"
                                   , "   build-depends: base       >= 3       && < 5,"
                                   , "-                 attoparsec >= 0.7     && < 0.10,"
                                   , "+                 attoparsec >= 0.7,"
                                   , "                  text       >= 0.10    && < 0.12,"
                                   , "                  containers >= 0.1.0.1 && < 0.5,"
                                   , "                  array      >= 0.1     && < 0.4" ]]
    , debianize "attoparsec-text-enumerator" Newest [P.DebVersion "0.2.0.0-1~hackage1"]
    , debianize "authenticate" Newest []
    , debianize "base-unicode-symbols" Newest []
    , apt "haskell-base64-bytestring"
    , debianize "bimap" Newest [P.DebVersion "0.2.4-1~hackage1"]
    , P.Package { P.name = "haskell-binary"
                , P.spec = Quilt (Apt "sid" "haskell-binary" (Just "0.5.0.2-2")) (Darcs "http://src.seereason.com/haskell-binary-quilt" Nothing)
                , P.flags = [] }
    , apt "haskell-binary-shared" -- for leksah
    , debianize "bitmap" Newest [P.DebVersion "0.0.1-1~hackage1",
                          P.Patch . B.pack . unlines $
                               [ "--- tmp/Data/Bitmap/Pure.hs\t2011-09-15 06:47:30.638423438 -0700"
                               , "+++ tmp/Data/Bitmap/Pure.hs.orig\t2011-09-15 06:47:18.188439156 -0700"
                               , "@@ -47,7 +47,7 @@"
                               , " "
                               , " import Data.Word"
                               , " "
                               , "-import Foreign"
                               , "+import Foreign hiding (unsafePerformIO)"
                               , " "
                               , " import Data.ByteString (ByteString)"
                               , " import qualified Data.ByteString as B" ]
                                                                   ]
    , debianize "bitmap-opengl" Newest [P.DebVersion "0.0.0-1~hackage1"]
    , debianize "bitset" Newest [P.DebVersion "1.1-1~hackage1"]
    , apt "haskell-blaze-builder"
    , apt "haskell-blaze-builder-enumerator"
    , debianize "blaze-from-html" Newest [P.DebVersion "0.3.1.0-1~hackage1"]
    , debianize "blaze-html" Newest []
    , debianize "blaze-textual" Newest [P.DebVersion "0.2.0.5-1~hackage1"]
    , debianize "blaze-textual-native" Newest [P.DebVersion "0.2.1-1~hackage1"]
    , apt "haskell-bytestring-nums"
    , debianize "bytestring-trie" Newest []
    , P.Package { P.name = "haskell-bzlib"
                , P.spec = Quilt (Apt "sid" "haskell-bzlib" Nothing) (Darcs "http://src.seereason.com/haskell-bzlib-quilt" Nothing)
                , P.flags = [] }
    -- The cairo in sid imports haskell98, that fails under 7.2.1
    , debianize "cairo" Newest [P.ExtraDep "haskell-gtk2hs-buildtools-utils"] -- for leksah
    -- , debianize "cairo-pdf" Newest []
    , debianize "case-insensitive" Newest [P.DebVersion "0.4-1~hackage1"]
    , debianize "CC-delcont" Newest [P.DebVersion "0.2-1~hackage1"]
    , apt "haskell-cereal"
    , debianize "certificate" Newest [P.DebVersion "1.0.1-1~hackage1"]
    , P.Package { P.name = "haskell-cgi"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian" Nothing)
                , P.flags = [] }
    , apt "haskell-chart"
    , debianize "citeproc-hs" Newest [P.DebVersion "0.3.3-1~hackage1"]
    , apt "haskell-colour"
    , apt "haskell-configfile"
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer" Nothing
                , P.flags = [] }
    , debianize "convertible-text" Newest [P.DebVersion "0.3.0.10-1~hackage1"]
    , debianize "cprng-aes" Newest [P.DebVersion "0.2.3-1~hackage1"]
    , apt "haskell-criterion"
    , debianize "Crypto" Newest [P.DebVersion "4.2.4-1~hackage1"]
    , debianize "crypto-api" Newest [P.DebVersion "0.8-1~hackage1"]
    , debianize "crypto-pubkey-types" Newest [P.DebVersion "0.1.0-1~hackage1"]
    , debianize "cryptocipher" Newest [P.DebVersion "0.3.0-1~hackage1"]
    , debianize "cryptohash" Newest [P.DebVersion "0.7.4-1~hackage1"]
    , debianize "css" Newest [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" Newest []
    , apt "haskell-curl"
    , apt "haskell-data-accessor"
    , debianize "data-accessor-template" Newest [P.DebVersion "0.2.1.8-1"]
    , debianize "data-default" Newest [P.DebVersion "0.3.0-1~hackage1"]
    , debianize "data-object" Newest []
    , debianize "data-object-json" Newest []
    , debianize "dataenc" Newest [P.DebVersion "0.14.0.2-1"]
    , P.Package { P.name = "haskell-debian"
                , P.spec = Darcs (repo ++ "/haskell-debian-new") Nothing
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Darcs "http://src.seereason.com/haskell-debian-repo" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-decimal"
                , P.spec = Darcs "http://src.seereason.com/decimal" Nothing
                , P.flags = [] }
    , apt "haskell-deepseq"
{-  , P.Package { P.name = "haskell-deepseq"
                , P.spec = Apt "sid" "haskell-deepseq" (Just "1.1.0.2-2")
                , P.flags = [] } -}
    , apt "haskell-devscripts"
    , apt "haskell-diff"
    , apt "haskell-digest"
    , debianize "digestive-functors" Newest []
    , debianize "digestive-functors-blaze" Newest [P.DebVersion "0.2.1.0-1~hackage1"]
    , debianize "digestive-functors-happstack" Newest [P.DebVersion "0.1.1.1-1~hackage1"]
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Darcs (repo ++ "/digestive-functors-hsp") Nothing
                , P.flags = [] }
    , apt "haskell-dlist"
    -- Natty only(?)
    , debianize "double-conversion" Newest [P.DebVersion "0.2.0.3-1~hackage1"]
    , apt "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Quilt (Apt "sid" "haskell-edison-api" Nothing) (Darcs (repo ++ "/haskell-edison-api-quilt") Nothing)
                , P.flags = [] }
    , apt "haskell-edison-core"
    , apt "haskell-entropy"
    , apt "haskell-enumerator"
    , apt "haskell-erf"
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra" Nothing
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , apt "haskell-failure"
    , apt "haskell-feed"
    , debianize "fgl" Newest [P.DebVersion "5.4.2.4-1"]
    , debianize "file-embed" Newest [P.DebVersion "0.0.4.1-1~hackage1"]
    , P.Package { P.name = "haskell-formlets"
                , P.spec = DebDir (Hackage "formlets" Nothing) (Darcs "http://src.seereason.com/haskell-formlets-debian" Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-frisby"
                , P.spec = DebDir (Cd "frisby" (Darcs "http://src.seereason.com/frisby" Nothing)) (Darcs "http://src.seereason.com/frisby-debian" Nothing)
                , P.flags = [] }
    , debianize "funsat" Newest []
    , debianize "gd" Newest [P.ExtraDep "libfreetype6-dev",
                      P.Patch . B.pack . unlines $
                           [ "--- gd/gd.cabal.orig\t2011-06-25 12:27:26.000000000 -0700"
                           , "+++ gd/gd.cabal\t2011-09-10 14:29:48.514415016 -0700"
                           , "@@ -21,7 +21,7 @@"
                           , "   Extensions: ForeignFunctionInterface"
                           , "   Exposed-Modules: Graphics.GD, Graphics.GD.ByteString, Graphics.GD.ByteString.Lazy"
                           , "   Ghc-options: -Wall"
                           , "-  Extra-libraries: gd, png, z, jpeg, m, fontconfig, freetype, expat"
                           , "+  Extra-libraries: gd, png, z, jpeg, fontconfig, freetype6, expat"
                           , "   Includes: gd.h"
                           , "   Include-dirs:        cbits"
                           , "   Install-includes: gd-extras.h" ]]
    -- , debianize "gd" Newest [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , P.Package { P.name = "haskell-geni"
                , P.spec = DebDir (Darcs "http://code.haskell.org/GenI" Nothing) (Darcs (repo ++ "/haskell-geni-debian") Nothing)
                , P.flags = [] }
    , apt "haskell-ghc-paths" -- for leksah
    , apt "haskell-gio" -- for leksah
    , debianize "glib" Newest [P.ExtraDep "haskell-gtk2hs-buildtools-utils", P.ExtraDep "libglib"]
    , apt "haskell-glut"
    , debianize "gnuplot" Newest [P.DebVersion "0.4.2-1~hackage1"]
    , apt "haskell-gtk" -- for leksah
    , apt "haskell-gtksourceview2" -- for leksah
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base == 4.3.*
    -- so we can't use "debianize "haddock" []".
    , apt "haskell-haddock"
    , debianize "happstack" Newest [P.DebVersion "6.0.4-1~hackage1"]
    , P.Package { P.name = "haskell-happstack-authenticate"
                , P.spec = Darcs (repo ++ "/happstack-authenticate") Nothing
                , P.flags = [] }
    , debianize "happstack-data" Newest [P.DebVersion "6.0.0-1~hackage1"]
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-facebook"
                , P.spec = Darcs (repo ++ "/happstack-facebook") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-hsp"
                , P.spec = DebDir (Cd "happstack-hsp" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/happstack-hsp-debian") Nothing)
                , P.flags = [] }
    -- Version 6.1.0, which is just a wrapper around the non-happstack
    -- ixset package, has not yet been uploaded to hackage.
    -- , debianize "happstack-ixset" Newest []
    , P.Package { P.name = "haskell-happstack-ixset"
                , P.spec = DebDir (Cd "happstack-ixset" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/happstack-ixset-debian") Nothing)
                , P.flags = [] }

    , debianize "happstack-jmacro" Newest [P.DebVersion "6.0.0-1~hackage1"]
    , debianize "happstack-plugins" Newest [P.DebVersion "6.1.1-1~hackage1"]
    , P.Package { P.name = "haskell-happstack-scaffolding"
                , P.spec = Darcs (repo ++ "/happstack-scaffolding") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search") Nothing
                , P.flags = [] }
    , debianize "happstack-server" Newest [P.DebVersion "6.3.1-1~hackage1"]
    , debianize "happstack-state" Newest [P.DebVersion "6.1.2-1~hackage1"]
    , debianize "happstack-util" Newest [P.DebVersion "6.0.2-1~hackage1"]
    , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/happstackDotCom" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc" Nothing
                , P.flags = [] }
    , apt "haskell-harp"
    , debianize "hashable" Newest [P.DebVersion "1.1.2.2-1"]
    , debianize "hashed-storage" Newest [P.DebVersion "0.5.9-1"]
    , debianize "haskeline" Newest []
    , debianize "haskell-src" Newest [ P.ExtraDep "happy" ]
    , debianize "haskell-src-meta" Newest []
    , debianize "HaXml" Newest [P.Epoch 1, P.DebVersion "1:1.22.5-1~hackage1"]
    , debianize "heap" Newest [P.DebVersion "1.0.0-1~hackage1"]
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help" Nothing
                , P.flags = [] }
    , debianize "HiggsSet" Newest [P.DebVersion "0.1-1~hackage1"]
    , debianize "hinotify" Newest [P.DebVersion "0.3.2-1~hackage1"]
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript" Nothing) (Darcs (repo ++ "/hjavascript-quilt") Nothing)
                , P.flags = [] }
    , apt "haskell-hjscript"
    , debianize "hoauth" Newest []
    , debianize "hostname" Newest [P.DebVersion "1.0-1~hackage1"]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" Newest []
    , debianize "hs-bibutils" Newest [P.DebVersion "4.12-1~hackage1"]
    , apt "haskell-hsemail"
    , debianize "HsOpenSSL" Newest
                    [P.Patch . B.pack . unlines $
                          [ "--- HsOpenSSL.orig/HsOpenSSL.cabal\t2011-09-10 15:02:20.000000000 -0700"
                          , "+++ HsOpenSSL/HsOpenSSL.cabal\t2011-09-10 15:24:16.735325250 -0700"
                          , "@@ -50,7 +50,7 @@"
                          , "       CC-Options:         -D MINGW32"
                          , "       CPP-Options:        -DCALLCONV=stdcall"
                          , "   else"
                          , "-      Extra-Libraries: crypto ssl"
                          , "+      Extra-Libraries: crypto++ ssl"
                          , "       C-Sources:          cbits/mutex-pthread.c"
                          , "       CC-Options:         -D PTHREAD"
                          , "       CPP-Options:        -DCALLCONV=ccall" ]]
    , debianize "hsp" Newest [P.ExtraDep "trhsx", P.DebVersion "0.6.1-1"]
    , debianize "HsSyck" Newest []
    , apt "haskell-hstringtemplate"
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize "hsx" Newest [P.DebVersion "0.9.1-1"]
    , P.Package { P.name = "haskell-hsx-jmacro"
                , P.spec = DebDir (Cd "hsx-jmacro" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/haskell-hsx-jmacro-debian") Nothing)
                , P.flags = []
                }
    , apt "haskell-html"
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities" Nothing
                , P.flags = [] }
    , debianize "HTTP" Newest [P.Epoch 1, P.DebVersion "1:4000.1.2-1"]
    , debianize "http-enumerator" Newest []
    , debianize "http-types" Newest []
    , apt "haskell-hunit"
    , debianize "i18n" Newest [P.DebVersion "0.3-1~hackage1"]
    , debianize "iconv" Newest []
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver" Nothing) (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian" Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "instant-generics" Newest [P.DebVersion "0.3.3-1~hackage1"]
    , apt "haskell-irc"
    , P.Package { P.name = "haskell-ircbot"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/ircbot" Nothing
                , P.flags = []
                }
    , debianize "ixset" Newest [P.DebVersion "1.0.2-1~hackage1"]
    , debianize "jmacro" Newest [P.DebVersion "0.5.2-1~hackage1"]
    , P.Package { P.name = "haskell-json"
                , P.spec = Quilt (Apt "sid" "haskell-json" (Just "0.4.4-2")) (Darcs (repo ++ "/haskell-json-quilt") Nothing)
                , P.flags = [] }
    , debianize "JSONb" Newest [P.DebVersion "1.0.7-1~hackage1"]
    , debianize "language-css" Newest [P.DebVersion "0.0.4.1-1~hackage1"]
    , apt "haskell-largeword"
{-  , apt "haskell-leksah"
    , apt "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs "http://src.seereason.com/haskell-logic" Nothing
                , P.flags = [] }
    , debianize "logic-TPTP" Newest [ P.DebVersion "0.3.0.1-1~hackage1"
                             , P.ExtraDep "alex", P.ExtraDep "happy"
                             , P.Patch . B.pack . unlines $
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
                               , "                    , array" ] ]
    , apt "haskell-ltk" -- for leksah
    , apt "haskell-maybet"
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime" Nothing
                , P.flags = [] }
    , apt "haskell-mmap"
    -- "0.2.0.3" is the version to get from hackage,
    -- "0.2.0.3-1~hackage1" is the version to base our debian version
    -- on.  Both flags should be removed when we move to 0.3, but we
    -- need base 4.4 for that.
    , debianize "monad-control" (Pin "0.2.0.3") [P.DebVersion "0.2.0.3-1~hackage1"]
    , debianize "monad-par" Newest [P.DebVersion "0.1.0.1-1~hackage1"]
    , apt "haskell-monadcatchio-mtl"
    , debianize "monadLib" Newest [P.DebVersion "3.6.2-1~hackage1"]
    , debianize "monads-tf" Newest [P.DebVersion "0.1.0.0-1~hackage1"]
    , apt "haskell-monoid-transformer"
    , P.Package { P.name = "haskell-mtl"
                , P.spec = Apt "sid" "haskell-mtl" (Just "2.0.1.0-2")
                , P.flags = [] }
    , debianize "murmur-hash" Newest [P.DebVersion "0.1.0.4-1~hackage1"]
    , apt "haskell-mwc-random"
    , debianize "nano-hmac" Newest [ P.DebVersion "0.2.0ubuntu1"
                            , P.Patch . B.pack . unlines $
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
                                          , " " ]
                            , P.Patch . B.pack . unlines $
                                          [ "--- nano-hmac/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 16:39:39.603631778 -0700"
                                          , "+++ nano-hmac/Data/Digest/OpenSSL/HMAC.hsc.orig\t2011-09-16 13:57:55.000000000 -0700"
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
                                          , " #include \"openssl/hmac.h\"" ]]
    , debianize "network" Newest [P.DebVersion "2.3.0.7-1"]
    , apt "haskell-opengl"
    , debianize "openid" Newest
       [ P.DebVersion "0.2.0.0-1~hackage1"
       , P.Patch . B.pack . unlines $
         [ "--- openid.orig/openid.cabal\t2011-11-05 05:04:06.000000000 -0700"
         , "+++ openid/openid.cabal\t2011-11-05 06:02:40.368409637 -0700"
         , "@@ -29,12 +29,13 @@"
         , "   build-depends:   base       >= 4.0.0.0  && < 5.0.0.0,"
         , "                    bytestring >= 0.9.1.0  && < 0.10.0.0,"
         , "-                   containers >= 0.2.0.0  && < 0.4.1.0,"
         , "-                   HTTP       >= 4000.0.9 && < 4000.2,"
         , "+                   containers >= 0.2.0.0  && < 0.4.2.0,"
         , "+                   HTTP       >= 4000.0.9,"
         , "                    monadLib   >= 3.6.0.0  && < 3.7.0.0,"
         , "-                   network    >= 2.2.0.0  && < 2.3.0.0,"
         , "+                   network    >= 2.2.0.0,"
         , "                    time       >= 1.1.0.0  && < 1.3.0.0,"
         , "                    xml        >= 1.3.0.0  && < 1.4.0.0,"
         , "                    HsOpenSSL  >= 0.9.0.0  && < 0.11.0.0"
         , "+  extra-libraries: ssl, crypto++"
         , "   hs-source-dirs:  src"
         , "   exposed-modules: Codec.Binary.Base64,"
         , "                    Codec.Encryption.DH," ]]
    , debianize "operational" Newest [P.OmitLTDeps, P.DebVersion "0.2.0.3-1~hackage1"]
    , debianize "ordered" Newest []
    , debianize "pandoc" Newest [ P.DebVersion "1.8.2.1-1~hackage1"
                         , P.Patch . B.pack . unlines $
                                       [ "--- pandoc/pandoc.cabal.orig\t2011-09-10 14:35:25.000000000 -0700"
                                       , "+++ pandoc/pandoc.cabal\t2011-09-10 14:49:24.274009463 -0700"
                                       , "@@ -195,13 +195,13 @@"
                                       , "                  mtl >= 1.1 && < 2.1,"
                                       , "                  network >= 2 && < 2.4,"
                                       , "                  filepath >= 1.1 && < 1.3,"
                                       , "-                 process >= 1 && < 1.1,"
                                       , "+                 process >= 1,"
                                       , "                  directory >= 1 && < 1.2,"
                                       , "                  bytestring >= 0.9 && < 1.0,"
                                       , "                  zip-archive >= 0.1.1.7 && < 0.2,"
                                       , "                  utf8-string >= 0.3 && < 0.4,"
                                       , "                  old-time >= 1 && < 1.1,"
                                       , "-                 HTTP >= 4000.0.5 && < 4000.2,"
                                       , "+                 HTTP >= 4000.0.5,"
                                       , "                  texmath >= 0.5 && < 0.6,"
                                       , "                  xml >= 1.3.5 && < 1.4,"
                                       , "                  random >= 1 && < 1.1,"
                                       , "@@ -281,13 +281,13 @@"
                                       , "                  mtl >= 1.1 && < 2.1,"
                                       , "                  network >= 2 && < 2.4,"
                                       , "                  filepath >= 1.1 && < 1.3,"
                                       , "-                 process >= 1 && < 1.1,"
                                       , "+                 process >= 1,"
                                       , "                  directory >= 1 && < 1.2,"
                                       , "                  bytestring >= 0.9 && < 1.0,"
                                       , "                  zip-archive >= 0.1.1.7 && < 0.2,"
                                       , "                  utf8-string >= 0.3 && < 0.4,"
                                       , "                  old-time >= 1 && < 1.1,"
                                       , "-                 HTTP >= 4000.0.5 && < 4000.2,"
                                       , "+                 HTTP >= 4000.0.5,"
                                       , "                  texmath >= 0.5 && < 0.6,"
                                       , "                  xml >= 1.3.5 && < 1.4,"
                                       , "                  random >= 1 && < 1.1," ]]
    , apt "haskell-pandoc-types"
    , apt "haskell-pango" -- for leksah
    , apt "haskell-parallel"
    , debianize "parse-dimacs" Newest [P.DebVersion "1.2-1~hackage1"]
    , debianize "parseargs" Newest [P.DebVersion "0.1.3.2-1~hackage1"]
    , apt "haskell-parsec"
    , apt "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2" Nothing) (Darcs "http://src.seereason.com/pbkdf2-debian" Nothing),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt "haskell-pcre-light"
    , debianize "permutation" Newest [P.DebVersion "0.4.1-1~hackage1"]
    , debianize "plugins" Newest [P.DebVersion "1.5.1.4-1~hackage1"]
    , debianize "polyparse" Newest [P.DebVersion "1.7-1~hackage1"]
    , apt "haskell-primitive"
    , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs (repo ++ "/haskell-proplogic-debian") Nothing)
                , P.flags = [] }
    {- , P.Package { P.name = "haskell-propositional-classes"
                , P.spec = Darcs (repo ++ "/propositional-classes") Nothing
                , P.flags = [] } -}
    -- This is bundled with the compiler
    -- , debianize "process" Newest []
    , debianize "PSQueue" Newest [P.DebVersion "1.1-1~hackage1"]
    , apt "haskell-puremd5"
    , debianize "pwstore-purehaskell" Newest [P.DebVersion "2.1-1~hackage1"] ] ++
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1. 
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    [ P.Package { P.name = "haskell-quickcheck1"
                , P.spec = Quilt (Apt "sid" "haskell-quickcheck1" Nothing) (Darcs (repo ++ "/haskell-quickcheck-quilt") Nothing)
                , P.flags = [] } ] ++
    [ debianize "QuickCheck" Newest [P.ExtraDep "libghc-random-prof", P.DebVersion "2.4.1.1-1.1"] ] ++
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    [ debianize "random" Newest [P.DebVersion "1.0.1.0-1"]
    , apt "haskell-regex-base"
    , apt "haskell-regex-compat"
    , apt "haskell-regex-posix"
    , apt "haskell-regex-tdfa"
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision" Nothing
                , P.flags = [] }
    , debianize "RJson" Newest []
    , debianize "RSA" Newest [P.DebVersion "1.0.6.2-1~hackage1"]
    , apt "haskell-safe"
    -- Depends on pandoc
    --, P.Package {P.name = "haskell-safecopy", P.spec = DebDir (Hackage "safecopy" (Just "0.5.1")) (Darcs "http://src.seereason.com/haskell-safecopy-debian" Nothing), P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , debianize "safecopy" Newest [P.DebVersion "0.6.0-1~hackage1"]
{-  , P.Package { P.name = "haskell-safecopy05"
                , P.spec = Quilt (Hackage "safecopy" (Just "0.5.1")) (Darcs (repo ++ "/safecopy05-quilt") Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] } -}
    , debianize "sat" Newest [P.DebVersion "1.1.1-1~hackage1",
                       P.Patch . B.pack . unlines $
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
                            , " main-is: \"SATSolver.hs\"" ]]
    , P.Package { P.name = "haskell-seereason-base"
                , P.spec = Darcs "http://src.seereason.com/seereason-base" Nothing
                , P.flags = [] }
    , debianize "semigroups" Newest [P.DebVersion "0.8-1"]
    , apt "haskell-sendfile"
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra" Nothing
                , P.flags = [] }
    , apt "haskell-sha"
    , debianize "shakespeare" Newest [P.DebVersion "0.10.2-1~hackage1"]
    , debianize "shakespeare-css" Newest []
    , debianize "simple-css" Newest [P.DebVersion "0.0.4-1~hackage1"]
    , debianize "SMTPClient" Newest [P.DebVersion "1.0.4-2"]
    , debianize "split" Newest [P.DebVersion "0.1.4.1-1~hackage1"]
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , debianize "haskell-src-exts" Newest [P.ExtraDep "happy", P.DebVersion "1.11.1-1"]
    , apt "haskell-statistics"
    , debianize "stb-image" Newest [P.DebVersion "0.2-1~hackage1"]
    , apt "haskell-stm"
    , apt "haskell-strict" -- for leksah
    , apt "haskell-strict-concurrency"
    , debianize "strict-io" Newest [] -- for GenI
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb" Newest []
    , debianize "syb-with-class" Newest [P.DebVersion "0.6.1.2-1"]
    , apt "haskell-syb-with-class-instances-text"
    , debianize "tagged" Newest [P.DebVersion "0.2.3.1-1"]
    , debianize "tagsoup" Newest [P.DebVersion "0.12.5-1~hackage1"]
    , apt "haskell-tar"
    , apt "haskell-terminfo"
    , debianize "test-framework" Newest [P.ExtraDep "libghc-random-prof", P.DebVersion "0.4.1.1-1~hackage1"]
    , debianize "test-framework-hunit" Newest [P.DebVersion "0.2.6-1~hackage1"]
    , debianize "test-framework-quickcheck" Newest [P.DebVersion "0.2.7-1~hackage1"]
    , debianize "testpack" Newest [P.DebVersion "2.1.1-1~hackage1",
                            P.Patch (B.pack
                                     (unlines
                                      [ "--- testpack-2.1.1/src/Test/QuickCheck/Instances.hs.orig\t2011-09-09 18:47:51.256206942 -0700"
                                      , "+++ testpack-2.1.1/src/Test/QuickCheck/Instances.hs\t2011-09-09 18:47:56.714633473 -0700"
                                      , "@@ -46,9 +46,3 @@"
                                      , "     coarbitrary n = variant (if n >= 0 then 2 * x else 2 * x + 1)"
                                      , "                 where x = abs . fromIntegral $ n"
                                      , " #endif"
                                      , "-"
                                      , "-instance Random Word8 where"
                                      , "-    randomR (a, b) g = (\\(x, y) -> (fromInteger x, y)) $"
                                      , "-                       randomR (toInteger a, toInteger b) g"
                                      , "-    random g = randomR (minBound, maxBound) g"
                                      , "-" ]))]
    , apt "haskell-texmath"
    , debianize "text" Newest []
    , debianize "th-expand-syns" Newest [P.DebVersion "0.3.0.0-1~hackage1"]
    , debianize "th-lift" Newest [P.DebVersion "0.5.4-1~hackage1"]
    , debianize "tls" Newest []
    , debianize "tls-extra" Newest [P.DebVersion "0.4.1-1~hackage1"]
    , P.Package { P.name = "haskell-transformers"
                , P.spec = Apt "sid" "haskell-transformers" (Just "0.2.2.0-3")
                , P.flags = [] }
    , debianize "transformers-base" Newest []
    , debianize "TrieMap" Newest [P.DebVersion "4.0.1-1~hackage1"]
    , debianize "unicode-names" Newest [P.DebVersion "3.2.0.0-1~hackage1"]
    , debianize "unicode-properties" Newest [P.DebVersion "3.2.0.0-1~hackage1"]
    , debianize "uniplate" Newest [P.DebVersion "1.6.5-1~hackage1"]
    , apt "haskell-unix-compat"
{-
    , debianize "Unixutils" Newest [ P.Patch . B.pack . unlines $
                              [ "--- Unixutils/Unixutils.cabal.orig\t2011-10-03 18:12:36.251952798 -0700"
                              , "+++ Unixutils/Unixutils.cabal\t2011-10-03 18:12:42.341296851 -0700"
                              , "@@ -29,7 +29,7 @@"
                              , "         System.Unix.Shadow,"
                              , "         System.Unix.SpecialDevice,"
                              , "         System.Unix.Files"
                              , "-Extra-libraries: crypt"
                              , "+Extra-libraries: crypto++"
                              , " "
                              , " -- For more complex build options see:"
                              , " -- http://www.haskell.org/ghc/docs/latest/html/Cabal/" ] ]
-}
    , P.Package { P.name = "haskell-unixutils"
                , P.spec = Darcs (repo ++ "/haskell-unixutils") Nothing
                , P.flags = [] }
    , debianize "unordered-containers" Newest [P.DebVersion "0.1.4.3-1~hackage1"]
    , debianize "unpack-funcs" Newest [P.DebVersion "0.2.0-1~hackage1"]
    , debianize "utf8-prelude" Newest [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt "sid" "haskell-utf8-string" Nothing
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , apt "haskell-utility-ht"
    , debianize "vacuum" Newest [P.DebVersion "1.0.0.2-1~hackage1"]
    , debianize "vacuum-opengl" Newest [P.DebVersion "0.0.3-1~hackage2"]
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" Newest [P.DebVersion "0.9-2~hackage1"]
    , apt "haskell-vector-algorithms"
    , debianize "virthualenv" Newest [P.DebVersion "0.2-1~hackage1"]
    , debianize "wai" Newest []
    , debianize "web-encodings" Newest []
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Cd "web-routes" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-happstack"
                , P.spec = Cd "web-routes-happstack" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-hsp"
                , P.spec = Cd "web-routes-hsp" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-mtl"
                , P.spec = Cd "web-routes-mtl" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }      
    , P.Package { P.name = "haskell-web-routes-th"
                , P.spec = Cd "web-routes-th" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , debianize "xhtml" Newest
                  [P.DebVersion "3000.2.0.4-1",
                   P.Patch . B.pack . unlines $
                   [ "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/BlockTable.hs xhtml-3000.2.0.4/Text/XHtml/BlockTable.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/BlockTable.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/BlockTable.hs\t2011-09-30 11:31:26.459612132 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -----------------------------------------------------------------------------"
                   , " -- |"
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Debug.hs xhtml-3000.2.0.4/Text/XHtml/Debug.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Debug.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Debug.hs\t2011-09-30 11:32:38.329612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -- | This module contains functions for displaying"
                   , " --   HTML as a pretty tree."
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Extras.hs xhtml-3000.2.0.4/Text/XHtml/Extras.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Extras.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Extras.hs\t2011-09-30 11:32:38.119612144 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Extras where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Frameset/Attributes.hs xhtml-3000.2.0.4/Text/XHtml/Frameset/Attributes.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Frameset/Attributes.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Frameset/Attributes.hs\t2011-09-30 11:32:39.729612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Frameset.Attributes where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Frameset/Elements.hs xhtml-3000.2.0.4/Text/XHtml/Frameset/Elements.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Frameset/Elements.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Frameset/Elements.hs\t2011-09-30 11:32:40.019612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Frameset.Elements where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Frameset.hs xhtml-3000.2.0.4/Text/XHtml/Frameset.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Frameset.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Frameset.hs\t2011-09-30 11:32:37.709612144 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -- | Produces XHTML 1.0 Frameset."
                   , " module Text.XHtml.Frameset ("
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Internals.hs xhtml-3000.2.0.4/Text/XHtml/Internals.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Internals.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Internals.hs\t2011-09-30 11:31:17.389612130 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -----------------------------------------------------------------------------"
                   , " -- |"
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Strict/Attributes.hs xhtml-3000.2.0.4/Text/XHtml/Strict/Attributes.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Strict/Attributes.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Strict/Attributes.hs\t2011-09-30 11:32:39.039612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Strict.Attributes where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Strict/Elements.hs xhtml-3000.2.0.4/Text/XHtml/Strict/Elements.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Strict/Elements.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Strict/Elements.hs\t2011-09-30 11:32:39.379612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Strict.Elements where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Strict.hs xhtml-3000.2.0.4/Text/XHtml/Strict.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Strict.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Strict.hs\t2011-09-30 11:32:34.529612144 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -- | Produces XHTML 1.0 Strict."
                   , " module Text.XHtml.Strict ("
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Table.hs xhtml-3000.2.0.4/Text/XHtml/Table.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Table.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Table.hs\t2011-09-30 11:32:38.209612144 -0700"
                   , "@@ -1,7 +1,4 @@"
                   , " {-# LANGUAGE CPP #-}"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -- | Table combinators for XHTML."
                   , " module Text.XHtml.Table (HtmlTable, HTMLTABLE(..),"
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Transitional/Attributes.hs xhtml-3000.2.0.4/Text/XHtml/Transitional/Attributes.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Transitional/Attributes.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Transitional/Attributes.hs\t2011-09-30 11:32:37.909612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Transitional.Attributes where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Transitional/Elements.hs xhtml-3000.2.0.4/Text/XHtml/Transitional/Elements.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Transitional/Elements.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Transitional/Elements.hs\t2011-09-30 11:32:37.959612144 -0700"
                   , "@@ -1,8 +1,5 @@"
                   , " {-# OPTIONS_HADDOCK hide #-}"
                   , " -- #hide"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " module Text.XHtml.Transitional.Elements where"
                   , " "
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml/Transitional.hs xhtml-3000.2.0.4/Text/XHtml/Transitional.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml/Transitional.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml/Transitional.hs\t2011-09-30 11:32:38.729612144 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -- | Produces XHTML 1.0 Transitional."
                   , " module Text.XHtml.Transitional ("
                   , "Only in xhtml-3000.2.0.4/Text/XHtml: Transitional.hs~"
                   , "diff -ru xhtml-3000.2.0.4.orig/Text/XHtml.hs xhtml-3000.2.0.4/Text/XHtml.hs"
                   , "--- xhtml-3000.2.0.4.orig/Text/XHtml.hs\t2011-09-01 02:55:12.000000000 -0700"
                   , "+++ xhtml-3000.2.0.4/Text/XHtml.hs\t2011-09-30 11:31:45.339612136 -0700"
                   , "@@ -1,6 +1,3 @@"
                   , "-#if __GLASGOW_HASKELL__ >= 701"
                   , "-{-# LANGUAGE Safe #-}"
                   , "-#endif"
                   , " "
                   , " -----------------------------------------------------------------------------"

                   , " -- |" ]]
    , apt "haskell-xml"
    , debianize "xml-enumerator" Newest []
    , debianize "xml-types" Newest [P.DebVersion "0.3-1~hackage1"]
    , debianize "xss-sanitize" Newest []
    , debianize "yaml-light" Newest []
    , apt "haskell-zip-archive"
    , apt "haskell-zlib"
    , apt "haskell-zlib-bindings"
    , apt "haskell-zlib-enum"

    , apt "highlighting-kate"
    , P.Package { P.name = "hscolour"
                , P.spec = Apt "sid" "hscolour" Nothing
                , P.flags = [P.RelaxDep "hscolour"] }
    , apt "hslogger"
    , apt "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery" Nothing)
                , P.flags = [] }
    , apt "jquery-goodies"
    , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "sid" "jqueryui" Nothing)
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "sid" "magic-haskell" (Just "1.0.8-7")) (Darcs (repo ++ "/magic-quilt") Nothing)
                , P.flags = [] }
    , debianize "MissingH" Newest [P.DebVersion "1.1.1.0-1~hackage1"]
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring" Nothing
                , P.flags = [] }
    , apt "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs" Nothing
                , P.flags = [] }
    -- , apt   "wordpress"

{-  -- Algebra cohort
    , debianize "adjunctions" Newest []
    , debianize "algebra" Newest []
    , debianize "bifunctors" Newest []
    , debianize "categories" Newest []
    , debianize "comonad" Newest []
    , debianize "comonads-fd" Newest []
    , debianize "comonad-transformers" Newest []
    , debianize "contravariant" Newest []
    , debianize "data-lens" Newest []
    , debianize "distributive" Newest []
    , debianize "free" Newest []
    , debianize "keys" Newest []
    , debianize "representable-functors" Newest []
    , debianize "representable-tries" Newest []
    , debianize "semigroupoids" Newest []
    , debianize "void" Newest []
-}
    ]
    where
      apt :: String -> P.Package
      apt name =
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
               , P.spec = Apt dist name version
               , P.flags = [] }

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

data CabalVersion = Pin String | Newest

-- | Build a target that pulls the source from hackage and then
-- generates a debianization using cabal-debian.  Note that this will
-- affect the debian source package names for a given cabal package,
-- but it does not affect the dependency names generated by cabal
-- debian when it debianizes a package.
debianize :: String -> CabalVersion -> [P.PackageFlag] -> P.Package
debianize s ver flags =
    P.Package { P.name = debianName s
              , P.spec = Debianize s (case ver of Pin x -> Just x; Newest -> Nothing)
              , P.flags = P.Maintainer "SeeReason Autobuilder <partners@seereason.com>" : P.Revision "" : flags}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "haskell-quickcheck2"
      debianName "parsec" = "haskell-parsec3"
      debianName "gtk2hs-buildtools" = "gtk2hs-buildtools"
      -- The correct name would be haskell-haskell-src-exts, but the package
      -- in sid has the name "haskell-src-exts".
      debianName "haskell-src-exts" = "haskell-src-exts"
      debianName "MissingH" = "missingh"
      debianName _ = "haskell-" ++ map toLower s
{-
hackage :: String -> [Flag] -> P.Package
hackage name fs =
     P.Package { P.name = "haskell-" ++ map toLower name
               , P.spec = proc $ DebDir (Hackage name v) (Darcs (r ++ "/" ++ pre ++ name' ++ suff) Nothing)
               , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
     where
       pre = if elem NP fs then "" else "haskell-"
       suff = if elem NS fs then "" else "-debian"
       name' = if elem UC fs then name else map toLower name
       proc x = if elem P fs then Proc x else x
       v = foldl f Nothing fs
           where f Nothing (Pin ver) = Just ver
                 f s _ = s
       r = foldl f repo fs
           where f _ (Local home) = localRepo home
                 f s _ = s

-- | This is the flag type passed to the hackage function only, not the
-- PackageFlag type used in the Target record.
data Flag
    = Pin String   -- ^ Pin version number instead of using the most recent.  These arise when
                   -- a new hackage version appears but we aren't ready to migrate.
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is generally the _home parameter to targets.
    deriving Eq
-}
