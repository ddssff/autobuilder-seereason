{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Public ( targets, hackage, Flag(..) ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
--import Data.Either (partitionEithers)
--import Data.List (intercalate)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec
import Targets.Common

ring0 = False

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets _home release =
    (if ring0
     then [ lucidNatty (sid "ghc")
                       (P.Package { P.name = "ghc"
                                  , P.spec = Apt "experimental" "ghc" Nothing
                                  , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] })
          , sid "haskell-devscripts"
          , sid "haskell-dummy"
          , sid "hscolour" ]
     else []) ++
    -- Alphabetized by fromMaybe (debian name) (cabal name).
    [ debianize "acid-state" []
    , lucidNatty (hackage release "AES" []) (debianize "AES" [])
    , lucidNatty (hackage release "aeson" []) (debianize "aeson" [])
    , debianize "aeson-native" []
    , sid "agda"
    , sid "agda-bin"
    , sid "agda-stdlib"
    , debianize "ansi-terminal" []
    , debianize "ansi-wl-pprint" []
    -- Our debianization has several important patches.
    , hackage release "applicative-extras" [NP]
    , sid "haskell-asn1-data"
    , lucidNatty (hackage release "attempt" []) (debianize "attempt" [])
    , lucidNatty (sid "haskell-attoparsec") (debianize "attoparsec" [])
    , sid "haskell-attoparsec-enumerator"
    , sid "haskell-attoparsec-text"
    , lucidNatty (hackage release "attoparsec-text-enumerator" []) (debianize "attoparsec-text-enumerator" [])
    , debianize "authenticate" []
    , lucidNatty (sid "haskell-base-unicode-symbols") (debianize "base-unicode-symbols" [])
    , sid "haskell-base64-bytestring"
    , sid "bash-completion"
    , lucidNatty (hackage release "bimap" []) (debianize "bimap" [])
    , sid "haskell-binary"
    , sid "haskell-binary-shared" -- for leksah
    , lucidNatty (hackage release "bitmap" []) (debianize "bitmap" [])
    , lucidNatty (hackage release "bitmap-opengl" []) (debianize "bitmap-opengl" [])
    , lucidNatty (hackage release "bitset" []) (debianize "bitset" [])
    , sid "haskell-blaze-builder"
    , sid "haskell-blaze-builder-enumerator"
    , lucidNatty (hackage release "blaze-from-html" []) (debianize "blaze-from-html" [])
    , lucidNatty (sid "haskell-blaze-html") (debianize "blaze-html" [])
    , lucidNatty (hackage release "blaze-textual" []) (debianize "blaze-textual" [])
    , debianize "blaze-textual-native" []
    , sid "haskell-bytestring-nums"
    , lucidNatty (hackage release "bytestring-trie" []) (debianize "bytestring-trie" [])
    , sid "haskell-bzlib"
    , sid "haskell-cairo" -- for leksah
    , lucidNatty (sid "haskell-case-insensitive") (debianize "case-insensitive" [])
    , lucidNatty (hackage release "CC-delcont" [NP, UC]) (debianize "CC-delcont" [])
    , sid "haskell-cereal"
    , sid "haskell-certificate"
    , sid "haskell-cgi"
    , sid "haskell-citeproc-hs"
    , lucidNatty (sid "cpphs")
                 (P.Package { P.name = "cpphs"
                            , P.spec = Quilt (Apt "sid" "cpphs" Nothing) (Darcs "http://src.seereason.com/cpphs-quilt" Nothing)
                            , P.flags = [] })
    , lucidNatty (sid "haskell-cprng-aes") (debianize "cprng-aes" [])
    , sid "haskell-colour"
    , sid "haskell-configfile"
    , lucidNatty (hackage release "convertible-text" []) (debianize "convertible-text" [])
    , sid "haskell-criterion"
    , lucidNatty (sid "haskell-crypto") (debianize "Crypto" [])
    , sid "haskell-crypto-api"
    , sid "haskell-cryptocipher"
    , sid "haskell-cryptohash"
    , lucidNatty (hackage release "css" []) (debianize "css" [])
    , debianize "language-css" []
    , debianize "simple-css" []
    , debianize "shakespeare-css" []
    , debianize "shakespeare" []
    , debianize "TrieMap" []
    , debianize "HiggsSet" []
    , debianize "th-expand-syns" []
    , debianize "unpack-funcs" []
    , sid "haskell-curl"
    , sid "darcs"
    , sid "haskell-data-accessor"
    , lucidNatty (sid "haskell-data-accessor-template") (debianize "data-accessor-template" [])
    , debianize "data-default" []
    , debianize "data-object" []
    , lucidNatty (hackage release "data-object-json" []) (debianize "data-object-json" [])
    , lucidNatty (sid "haskell-dataenc") (debianize "dataenc" [])
    , sid "haskell-debian"
    , sid "haskell-deepseq"
    , sid "haskell-digest"
    , lucidNatty (hackage release "digestive-functors" []) (debianize "digestive-functors" [])
    , lucidNatty (hackage release "digestive-functors-happstack" []) (debianize "digestive-functors-happstack" [])
    , sid "haskell-diff"
    , sid "haskell-dlist"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Quilt (Apt "sid" "haskell-edison-api" Nothing) (Darcs (repo ++ "/haskell-edison-api-quilt") Nothing)
                , P.flags = [] }
    , sid "haskell-edison-core"
    , sid "haskell-entropy"
    , sid "haskell-enumerator"
    , sid "haskell-erf"
    , sid "haskell-failure"
    , sid "haskell-feed"
    , lucidNatty (sid "haskell-fgl") (debianize "fgl" [])
    , hackage release "formlets" []
    , lucidNatty (hackage release "funsat" []) (debianize "funsat" [])
    , lucidNatty (hackage release "gd" []) (debianize "gd" [P.Patch . B.pack . unlines $
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
                                                            , "   Install-includes: gd-extras.h" ]])
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , sid "geneweb"
    , sid "haskell-ghc-paths" -- for leksah
    , sid "haskell-gio" -- for leksah
    , sid "haskell-glib"
    , sid "haskell-glut"
    , debianize "gnuplot" []
    , sid "haskell-gtk" -- for leksah
    , lucidNatty (sid "gtk2hs-buildtools") (debianize "gtk2hs-buildtools" [])
    , sid "haskell-gtksourceview2" -- for leksah
    , lucidNatty (hackage release "happstack" [NP]) (debianize "happstack" [])
    -- Switch to the hackage target for happstack-data once a new upstream appears in hackage.
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz" "be72c4c11d1317bf52c80782eac28a2d") (Darcs "http://src.seereason.com/happstack-data-debian" Nothing)
                , P.flags = [] }
    -- , lucidNatty (hackage release "happstack-data" [NP]) (debianize "happstack-data" [])
    , lucidNatty (hackage release "happstack-ixset" [NP]) (debianize "happstack-ixset" [])
    , debianize "happstack-plugins" []
    , debianize "happstack-server" []
    , debianize "happstack-jmacro" []
    , lucidNatty (hackage release "happstack-state" [NP]) (hackage release "happstack-state" [NP])
    , lucidNatty (hackage release "happstack-util" [NP]) (debianize "happstack-util" [])
    , lucidNatty (P.Package { P.name = "happy"
                            , P.spec = Apt "sid" "happy" Nothing
                            , P.flags = [ P.RelaxDep "happy" ] })
                 -- Hackage target doesn't build with our
                 -- debianization due to funky bootstrapping files in
                 -- dist/build/happy/happy-tmp.
                 -- (hackage release "happy" [])
                 (P.Package { P.name = "happy"
                            , P.spec = Quilt (Apt "sid" "happy" Nothing) (Darcs (repo ++ "/happy-quilt") Nothing)
                            , P.flags = [ P.RelaxDep "happy" ] })
    , sid "haskell-harp"
    , debianize "hashable" []
    , lucidNatty -- Current version needs a BangPatterns options
                 (sid "haskell-hashed-storage")
                 (debianize "hashed-storage" [])
    , lucidNatty (sid "haskell-haskeline") (debianize "haskeline" [])
    , lucidNatty (sid "haskell-haskell-src") (debianize "haskell-src" [P.ExtraDep "happy"])
    , lucidNatty (hackage release "heap" [NP]) (debianize "heap" [])
    , sid "highlighting-kate"
    , debianize "hinotify" []
    , sid "haskell-hjavascript"
    , sid "haskell-hjscript"
    , debianize "HaXml" [P.Epoch 1]
    , lucidNatty (hackage release "hoauth" []) (debianize "hoauth" [])
    , debianize "hostname" []
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , lucidNatty (hackage release "HPDF" []) (debianize "HPDF" [])
    , sid "haskell-hsemail"
    , sid "hslogger"
    , lucidNatty (hackage release "HsOpenSSL" []) 
                 (debianize "HsOpenSSL"
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
                   , "       CPP-Options:        -DCALLCONV=ccall" ]])
    , lucidNatty (hackage release "hsp" [NP]) (debianize "hsp" [P.ExtraDep "trhsx"])
    , sid "haskell-hstringtemplate"
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , lucidNatty (hackage release "hsx" [NP]) (debianize "hsx" [])
    , sid "haskell-html"
    , sid "html-xml-utils"
    , lucidNatty (sid "haskell-http") (debianize "HTTP" [P.Epoch 1])
    , debianize "http-enumerator" []
    , debianize "http-types" []
    , sid "haskell-hunit"
    , lucidNatty (hackage release "i18n" [NP]) (debianize "i18n" [])
    , hackage release "incremental-sat-solver" []
    , debianize "instant-generics" []
    , sid "haskell-irc"
    , debianize "ixset" []
    , lucidNatty (hackage release "jmacro" [Pin "0.5.1"]) (debianize "jmacro" [])
    , sid "jquery"
    , sid "jqueryui"
    , sid "jquery-goodies"
    , debianize "JSONb" []
    , sid "haskell-json"
    , sid "haskell-largeword"
    , sid "haskell-leksah"
    , sid "haskell-leksah-server" -- for leksah
    , sid "haskell-ltk" -- for leksah
    , debianize "logic-TPTP" [P.ExtraDep "alex", P.ExtraDep "happy"]
    , sid "magic-haskell"
    , sid "haskell-maybet"
    , lucidNatty (sid "missingh") (debianize "MissingH" [])
    , sid "haskell-mmap"
    , sid "haskell-monadcatchio-mtl"
    , debianize "monad-control" []
    , lucidNatty (hackage release "monadLib" []) (debianize "monadLib" [])
    , debianize "monad-par" []
    , lucidNatty (hackage release "monads-tf" []) (debianize "monads-tf" [])
    , sid "haskell-monoid-transformer"
    , sid "haskell-mtl"
    , lucidNatty (hackage release "murmur-hash" [Pin "0.1.0.2"]) (debianize "murmur-hash" [])
    , sid "haskell-mwc-random"
    , lucidNatty (hackage release "nano-hmac" []) (debianize "nano-hmac" [P.Patch . B.pack . unlines $
                                                                          [ "--- nano-hmac/nano-hmac.cabal.orig\t2011-08-14 09:25:43.000000000 -0700"
                                                                          , "+++ nano-hmac/nano-hmac.cabal\t2011-09-10 14:24:25.234226579 -0700"
                                                                          , "@@ -23,5 +23,5 @@"
                                                                          , "   ghc-options:         -Wall -Werror -O2"
                                                                          , "   extensions:          ForeignFunctionInterface, BangPatterns, CPP"
                                                                          , "   includes:            openssl/hmac.h"
                                                                          , "-  extra-libraries:     crypto ssl"
                                                                          , "+  extra-libraries:     crypto++ ssl"
                                                                          , " " ]])
    , lucidNatty (sid "haskell-network") (debianize "network" [])
    , lucidNatty (hackage release "openid" []) (debianize "openid" [])
    , sid "haskell-opengl"
    , debianize "operational" []
    , lucidNatty -- This pandoc debianize target has a dependency on an older version of HTTP
                 -- , debianize "pandoc" []
                 (hackage release "pandoc" [])
                 (debianize "pandoc" [P.Patch . B.pack . unlines $
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
                                      , "                  random >= 1 && < 1.1," ]])
    , sid "haskell-pandoc-types"
    , sid "haskell-pango" -- for leksah
    , sid "haskell-parallel"
    , lucidNatty (hackage release "parseargs" []) (debianize "parseargs" [])
    , sid "haskell-parsec2"
    , sid "haskell-parsec"
    , lucidNatty (hackage release "parse-dimacs" [NP]) (debianize "parse-dimacs" [])
    , hackage release "PBKDF2" [NP]
    , sid "haskell-pcre-light"
    , lucidNatty (hackage release "permutation" [NS]) (debianize "permutation" [])
    , debianize "plugins" []
    , debianize "polyparse" []
    , sid "haskell-primitive"
    -- This is bundled with the compiler
    -- , debianize "process" []
    , lucidNatty (hackage release "PSQueue" []) (debianize "PSQueue" [])
    , sid "haskell-puremd5"
    , lucidNatty (hackage release "pwstore-purehaskell" []) (debianize "pwstore-purehaskell" [])
    , lucidNatty (sid "haskell-quickcheck") (debianize "QuickCheck" [{-P.DebName "quickcheck2"-}])
    , P.Package { P.name = "haskell-quickcheck1"
                , P.spec = Quilt (Apt "sid" "haskell-quickcheck1" Nothing) (Darcs (repo ++ "/haskell-quickcheck-quilt") Nothing)
                , P.flags = [] }
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" []
    , sid "haskell-regex-base"
    , sid "haskell-regex-compat"
    , sid "haskell-regex-posix"
    , sid "haskell-regex-tdfa"
    , lucidNatty (hackage release "RJson" [NP, UC]) (debianize "RJson" [])
    , let t = debianize "RSA" [] in
      case release of
        "natty-seereason" -> t {P.spec = Quilt (P.spec t) (Darcs (repo ++ "/haskell-rsa-quilt") Nothing)}
        _  -> t
    , sid "haskell-safe"
    -- Depends on pandoc
    , lucidNatty (hackage release "safecopy" []) (debianize "safecopy" [])
    , lucidNatty (hackage release "sat" []) (debianize "sat" [P.Patch . B.pack . unlines $
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
                                                                , " main-is: \"SATSolver.hs\"" ]])
    , lucidNatty (sid "haskell-semigroups") (debianize "semigroups" [])
    , sid "haskell-sendfile"
    , sid "haskell-sha"
    , lucidNatty (sid "haskell-smtpclient") (debianize "SMTPClient" [])
    , debianize "split" []
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , lucidNatty (sid "haskell-src-exts") (debianize "haskell-src-exts" [])
    , lucidNatty (hackage release "haskell-src-meta" [Pin "0.4.0.1"]) (debianize "haskell-src-meta" [])
    , sid "haskell-statistics"
    , lucidNatty (hackage release "stb-image" []) (debianize "stb-image" [])
    , sid "haskell-stm"
    , sid "haskell-strict" -- for leksah
    , sid "haskell-strict-concurrency"
    , lucidNatty (sid "haskell-syb") (debianize "syb" [])
    , lucidNatty (sid "haskell-syb-with-class") (debianize "syb-with-class" [])
    , sid "haskell-syb-with-class-instances-text"
    , lucidNatty (sid "haskell-tagged") (debianize "tagged" [])
    , lucidNatty (hackage release "tagsoup" []) (debianize "tagsoup" [])
    , sid "haskell-tar"
    , sid "haskell-terminfo"
    , debianize "test-framework" []
    , debianize "test-framework-hunit" []
    , debianize "test-framework-quickcheck" []
    , lucidNatty (sid "haskell-text") (debianize "text" [])
    , lucidNatty (hackage release "th-lift" [Pin "0.5.3"]) (debianize "th-lift" [])
    , sid "haskell-tls"
    , sid "haskell-tls-extra"
    , sid "haskell-transformers"
    , debianize "testpack" [P.Patch (B.pack
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
    , sid "haskell-texmath"
    , sid "tinymce"
    , lucidNatty (hackage release "unicode-names" []) (debianize "unicode-names" [])
    , lucidNatty (hackage release "unicode-properties" []) (debianize "unicode-properties" [])
    , lucidNatty (sid "haskell-uniplate") (debianize "uniplate" [])
    , sid "haskell-unix-compat"
    , debianize "unordered-containers" []
    , lucidNatty (hackage release "utf8-prelude" [NP]) (debianize "utf8-prelude" [])
    , sid "haskell-utf8-string"
    , sid "haskell-utility-ht"
    , lucidNatty (hackage release "vacuum" [])
                 (debianize "vacuum"
                  [P.Patch . B.pack . unlines $
                   [ "--- haskell-vacuum-1.0.0/src/GHC/Vacuum.hs.orig\t2011-09-10 10:16:35.000000000 -0700"
                   , "+++ haskell-vacuum-1.0.0/src/GHC/Vacuum.hs\t2011-09-10 14:04:24.614335577 -0700"
                   , "@@ -98,7 +98,7 @@"
                   , " import Prelude hiding(catch)"
                   , " import Control.Concurrent"
                   , " "
                   , "-import Foreign"
                   , "+import Foreign hiding (unsafePerformIO)"
                   , " import GHC.Arr(Array(..))"
                   , " import GHC.Exts"
                   , " " ]])
    , lucidNatty (hackage release "vacuum-opengl" []) (debianize "vacuum-opengl" [])
    -- Requires devscripts 0.8.9, restore when that gets built
    -- sid "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" [P.DebVersion "0.9-2~hackage1"]
    , sid "haskell-vector-algorithms"
    , sid "haskell-wai"
    , lucidNatty (hackage release "web-encodings" []) (debianize "web-encodings" [])
    , sid "wordpress"
    , sid "haskell-xhtml"
    , sid "haskell-xml"
    , debianize "xml-enumerator" []
    , lucidNatty (hackage release "xml-types" []) (debianize "xml-types" [])
    , sid "haskell-xss-sanitize"
    , sid "haskell-zip-archive"
    , sid "haskell-zlib"
    , sid "haskell-zlib-bindings"
    , sid "haskell-zlib-enum"

{-  -- Algebra cohort
    , debianize "adjunctions" []
    , debianize "algebra" []
    , debianize "bifunctors" []
    , debianize "categories" []
    , debianize "comonad" []
    , debianize "comonads-fd" []
    , debianize "comonad-transformers" []
    , debianize "contravariant" []
    , debianize "data-lens" []
    , debianize "distributive" []
    , debianize "free" []
    , debianize "keys" []
    , debianize "representable-functors" []
    , debianize "representable-tries" []
    , debianize "semigroupoids" []
    , debianize "void" []
-}
    ] ++
    -- Natty only
    lucidNatty [] [ debianize "random" [] ] ++
    -- Natty only(?)
    lucidNatty [] [ debianize "double-conversion" []
                  -- Version 2.9.2 specifies ghc < 7.2 and base == 4.3.*
                  -- , debianize "haddock" []
                  , sid "haskell-haddock" ] -- for leksah

    where
      lucidNatty x _ | release == "lucid-seereason" = x
      lucidNatty _ x | release == "natty-seereason" = x
      lucidNatty _ _ = error $ "lucidNatty: Unexpected release: " ++ release
      sid name =
          P.Package { P.name = name
                    , P.spec = getSourceSpec name
                    , P.flags = getFlags name }
          where
            getFlags = map P.RelaxDep . getRelaxInfo
            -- Special cases of non-empty flags lists
            getRelaxInfo "ghc" = ["ghc","happy","alex","xsltproc","debhelper","quilt"]
            getRelaxInfo "hscolour" = ["hscolour"]
            getRelaxInfo "happy" = ["happy"]
            getRelaxInfo "haskell-utf8-string" = ["hscolour", "cpphs"]
            getRelaxInfo "haskell-debian" = ["cabal-debian"]
            getRelaxInfo _ = []

            -- Special case specs
            -- getSourceSpec "ghc" = Quilt (Apt "sid" "ghc" Nothing) (Darcs (repo ++ "/ghc7-quilt") Nothing)
            getSourceSpec "haskell-bzlib" = Quilt (Apt "sid" "haskell-bzlib" Nothing) (Darcs "http://src.seereason.com/haskell-bzlib-quilt" Nothing)
            getSourceSpec "haskell-json" = Quilt (Apt "sid" "haskell-json" (Just "0.4.4-2")) (Darcs (repo ++ "/haskell-json-quilt") Nothing)
            getSourceSpec "haskell-uniplate" = Quilt (Apt "sid" "haskell-uniplate" Nothing) (Darcs "http://src.seereason.com/haskell-uniplate-quilt" Nothing)
            -- Apply a patch I sent to marco
            -- getSourceSpec "haskell-debian" = Quilt "(apt:sid:haskell-debian)" "(darcs:" ++ localRepo _home ++ "/haskell-debian-quilt)"
            getSourceSpec "haskell-debian" = Darcs (repo ++ "/haskell-debian-new") Nothing
            -- getSourceSpec "haskell-debian" = Apt "sid" "haskell-debian" Nothing
            -- Try removing the quilt when a revision newer than 0.5.0.2-2 appears in sid
            getSourceSpec "haskell-binary" = Quilt (Apt "sid" "haskell-binary" (Just "0.5.0.2-2")) (Darcs "http://src.seereason.com/haskell-binary-quilt" Nothing)
            getSourceSpec "haskell-hjavascript" = Quilt (Apt "sid" "haskell-hjavascript" Nothing) (Darcs (repo ++ "/hjavascript-quilt") Nothing)
            -- sid version is 0.2.2.1-1, too old
            getSourceSpec "haskell-wai" = DebDir (Uri "http://hackage.haskell.org/packages/archive/wai/0.3.1/wai-0.3.1.tar.gz" "5a777cf08713a55818955ec4d0748622") (Darcs "http://src.seereason.com/haskell-wai-debian" Nothing)
            -- Version in sid is 3001.1.7.4-1, our version is 3001.1.8.2
            getSourceSpec "haskell-cgi" = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian" Nothing)
            -- Sid version needs cdbs >= 0.4.70~, which I can't seem to build.
            -- , spec = "apt:sid:pandoc"
            getSourceSpec "jquery" = Proc (Apt "sid" "jquery" Nothing)
            getSourceSpec "jqueryui" = Proc (Apt "sid" "jqueryui" Nothing)
            -- The sid version has dependencies on the old libghc6 packages.
            -- getSourceSpec "pandoc" = P.spec (hackage release "pandoc" [Pin "1.5.1.1", Local _home])
            -- Quit adds a dependency on libmagic-dev to libghc-magic-dev.  Next upstream release should have this fix.
            getSourceSpec "magic-haskell" = Quilt (Apt "sid" "magic-haskell" (Just "1.0.8-7")) (Darcs (repo ++ "/magic-quilt") Nothing)
            -- Pinned version numbers, when these are bumped we want to move to hackage targets.
            getSourceSpec "haskell-deepseq" = Apt "sid" "haskell-deepseq" (Just "1.1.0.2-2")
            getSourceSpec "haskell-transformers" = Apt "sid" "haskell-transformers" (Just "0.2.2.0-3")
            getSourceSpec "haskell-mtl" = Apt "sid" "haskell-mtl" (Just "2.0.1.0-2")
            -- The normal case
            getSourceSpec n = Apt "sid" n Nothing

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
-- generates a debianization using cabal-debian.
debianize :: String -> [P.PackageFlag] -> P.Package
debianize s flags =
    P.Package { P.name = "haskell-" ++ debianName s
              , P.spec = Debianize s Nothing
              , P.flags = P.Maintainer "SeeReason Autobuilder <partners@seereason.com>" : flags}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "quickcheck2"
      debianName "parsec" = "parsec3"
      debianName _ = map toLower s

hackage :: String -> String -> [Flag] -> P.Package
hackage _ name fs =
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

-- |Build a hackage Target (a target that pulls the source code from
-- hackage.haskell.org) from a the cabal package and some flags
-- describing common variations on the mapping from one to another.
{-
hackage "natty-seereason" name flags =
    let debName = map toLower name in
    P.Package { P.name = "haskell-" ++ debName
              , P.spec = proc $ Debianize name v
              , P.flags = [] }
    where
      proc spec = if elem P flags then Proc spec else spec
      v = foldl f Nothing flags
          where f Nothing (Pin ver) = Just ver
                f s _ = s
-}

data Flag
    = Pin String   -- ^ Pin version number instead of using the most recent.  These arise when
                   -- a new hackage version appears but we aren't ready to migrate.
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is generally the _home parameter to targets.
    deriving Eq
