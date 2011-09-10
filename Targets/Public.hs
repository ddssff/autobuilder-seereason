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
    -- Alphabetized
    [ debianize "acid-state" []
    , hackdeb release "AES" []
    , hackdeb release "aeson" []
    , debianize "aeson-native" []
    , sid "agda"
    , sid "agda-bin"
    , sid "agda-stdlib"
    , debianize "ansi-terminal" []
    , debianize "ansi-wl-pprint" []
    -- Our debianization has several important patches.
    , hackage release "applicative-extras" [NP]
    , sid "haskell-asn1-data"
    , hackdeb release "attempt" []
    , lucidNatty (sid "haskell-attoparsec") (debianize "attoparsec" [])
    , sid "haskell-attoparsec-enumerator"
    , sid "haskell-attoparsec-text"
    , hackdeb release "attoparsec-text-enumerator" []
    , debianize "authenticate" []
    , lucidNatty (sid "haskell-base-unicode-symbols") (debianize "base-unicode-symbols" [])
    , sid "haskell-base64-bytestring"
    , sid "bash-completion"
    , hackdeb release "bimap" []
    , sid "haskell-binary"
    , sid "haskell-binary-shared" -- for leksah
    , hackdeb release "bitmap" []
    , hackdeb release "bitmap-opengl" []
    , hackdeb release "bitset" []
    , sid "haskell-blaze-builder"
    , sid "haskell-blaze-builder-enumerator"
    , hackdeb release "blaze-from-html" []
    , lucidNatty (sid "haskell-blaze-html") (debianize "blaze-html" [])
    , hackdeb release "blaze-textual" []
    , debianize "blaze-textual-native" []
    , sid "haskell-bytestring-nums"
    , hackdeb release "bytestring-trie" []
    , sid "haskell-bzlib"
    , sid "haskell-cairo" -- for leksah
    , lucidNatty (sid "haskell-case-insensitive") (debianize "case-insensitive" [])
    , hackdeb release "CC-delcont" [NP, UC]
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
    , hackdeb release "convertible-text" []
    , sid "haskell-criterion"
    , lucidNatty (sid "haskell-crypto") (debianize "Crypto" [])
    , sid "haskell-crypto-api"
    , sid "haskell-cryptocipher"
    , sid "haskell-cryptohash"
    , sid "haskell-curl"
    , sid "darcs"
    , sid "haskell-data-accessor"
    , lucidNatty (sid "haskell-data-accessor-template") (debianize "data-accessor-template" [])
    , debianize "data-default" []
    , debianize "data-object" []
    , hackdeb release "data-object-json" []
    , lucidNatty (sid "haskell-dataenc") (debianize "dataenc" [])
    , sid "haskell-debian"
    , sid "haskell-deepseq"
    , sid "haskell-digest"
    , hackdeb release "digestive-functors" []
    , hackdeb release "digestive-functors-happstack" []
    , sid "haskell-diff"
    , sid "haskell-dlist"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , hackdeb release "double-conversion" []
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
    , hackdeb release "funsat" []
    , hackdeb release "gd" []
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , sid "geneweb"
    , sid "haskell-ghc-paths" -- for leksah
    , sid "haskell-gio" -- for leksah
    , sid "haskell-glib"
    , sid "haskell-glut"
    , hackdeb release "gnuplot" []
    , sid "haskell-gtk" -- for leksah
    , lucidNatty (sid "gtk2hs-buildtools") (debianize "gtk2hs-buildtools" [])
    , sid "haskell-gtksourceview2" -- for leksah
    , hackdeb release "happstack" [NP]
    -- Switch to the hackage target for happstack-data once a new upstream appears in hackage.
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz" "be72c4c11d1317bf52c80782eac28a2d") (Darcs "http://src.seereason.com/happstack-data-debian" Nothing)
                , P.flags = [] }
    -- , hackdeb release "happstack-data" [NP]
    , hackdeb release "happstack-ixset" [NP]
    , debianize "happstack-plugins" []
    , debianize "happstack-server" []
    , lucidNatty (hackage release "happstack-state" [NP]) (hackage release "happstack-state" [NP])
    , hackdeb release "happstack-util" [NP]
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
    , hackdeb release "heap" [NP]
    , sid "highlighting-kate"
    , debianize "hinotify" []
    , sid "haskell-hjavascript"
    , sid "haskell-hjscript"
    , debianize "HaXml" [P.Epoch 1]
    , hackdeb release "hoauth" []
    , debianize "hostname" []
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , hackdeb release "HPDF" []
    , sid "haskell-hsemail"
    , sid "hslogger"
    , lucidNatty (hackage release "HsOpenSSL" []) (debianize "HsOpenSSL" [P.ExtraDep "libcrypto++-dev"])
    , lucidNatty (hackage release "hsp" [NP]) (debianize "hsp" [P.ExtraDep "trhsx"])
    , sid "haskell-hstringtemplate"
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , hackdeb release "hsx" [NP]
    , sid "haskell-html"
    , sid "html-xml-utils"
    , lucidNatty (sid "haskell-http") (debianize "HTTP" [P.Epoch 1])
    , debianize "http-enumerator" []
    , debianize "http-types" []
    , sid "haskell-hunit"
    , hackdeb release "i18n" [NP]
    , hackage release "incremental-sat-solver" []
    , debianize "instant-generics" []
    , sid "haskell-irc"
    , debianize "ixset" []
    , lucidNatty (hackage release "jmacro" [Pin "0.5.1"]) (debianize "jmacro" [])
    , sid "jquery"
    , sid "jqueryui"
    , sid "jquery-goodies"
    , hackdeb release "JSONb" []
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
    , hackdeb release "monadLib" []
    , debianize "monad-par" []
    , hackdeb release "monads-tf" []
    , sid "haskell-monoid-transformer"
    , sid "haskell-mtl"
    , lucidNatty (hackage release "murmur-hash" [Pin "0.1.0.2"]) (debianize "murmur-hash" [])
    , sid "haskell-mwc-random"
    , lucidNatty (hackage release "nano-hmac" []) (debianize "nano-hmac" [P.ExtraDep "libcrypto++-dev"])
    , lucidNatty (sid "haskell-network") (debianize "network" [])
    , hackdeb release "openid" []
    , sid "haskell-opengl"
    , debianize "operational" []
    , lucidNatty -- This pandoc debianize target has a dependency on an older version of HTTP
                 -- , debianize "pandoc" []
                 (hackage release "pandoc" [])
                 -- When a version newer than 0.12.0 comes out this should be
                 -- switched to a debianize target.
                 (sid "pandoc")
    , sid "haskell-pandoc-types"
    , sid "haskell-pango" -- for leksah
    , sid "haskell-parallel"
    , hackdeb release "parseargs" []
    , sid "haskell-parsec2"
    , sid "haskell-parsec"
    , hackdeb release "parse-dimacs" [NP]
    , hackage release "PBKDF2" [NP]
    , sid "haskell-pcre-light"
    , hackdeb release "permutation" [NS]
    , debianize "plugins" []
    , debianize "polyparse" []
    , sid "haskell-primitive"
    -- This is bundled with the compiler
    -- , debianize "process" []
    , hackdeb release "PSQueue" []
    , sid "haskell-puremd5"
    , hackdeb release "pwstore-purehaskell" []
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
    , hackdeb release "RJson" [NP, UC]
    , let t = debianize "RSA" [] in
      case release of
        "natty-seereason" -> t {P.spec = Quilt (P.spec t) (Darcs (repo ++ "/haskell-rsa-quilt") Nothing)}
        _  -> t
    , sid "haskell-safe"
    -- Depends on pandoc
    , hackdeb release "safecopy" []
    , hackdeb release "sat" []
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
    -- , hackdeb release "haskell-src-exts" [NP]
    , lucidNatty (sid "haskell-src-exts") (debianize "haskell-src-exts" [])
    , lucidNatty (hackage release "haskell-src-meta" [Pin "0.4.0.1"]) (debianize "haskell-src-meta" [])
    , sid "haskell-statistics"
    , hackdeb release "stb-image" []
    , sid "haskell-stm"
    , sid "haskell-strict" -- for leksah
    , sid "haskell-strict-concurrency"
    , lucidNatty (sid "haskell-syb") (debianize "syb" [])
    , lucidNatty (sid "haskell-syb-with-class") (debianize "syb-with-class" [])
    , sid "haskell-syb-with-class-instances-text"
    , lucidNatty (sid "haskell-tagged") (debianize "tagged" [])
    , hackdeb release "tagsoup" []
    , sid "haskell-tar"
    , sid "haskell-terminfo"
    , lucidNatty (hackage release "test-framework" [Pin "0.4.0"]) (debianize "test-framework" [])
    , hackdeb release "test-framework-hunit" []
    , hackdeb release "test-framework-quickcheck" []
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
    , hackdeb release "unicode-names" []
    , hackdeb release "unicode-properties" []
    , lucidNatty (sid "haskell-uniplate") (debianize "uniplate" [])
    , sid "haskell-unix-compat"
    , debianize "unordered-containers" []
    , hackdeb release "utf8-prelude" [NP]
    , sid "haskell-utf8-string"
    , sid "haskell-utility-ht"
    , hackdeb release "vacuum" []
    , hackdeb release "vacuum-opengl" []
    -- Requires devscripts 0.8.9, restore when that gets built
    -- sid "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" [P.DebVersion "0.9-2~hackage1"]
    , sid "haskell-vector-algorithms"
    , sid "haskell-wai"
    , hackdeb release "web-encodings" []
    , sid "wordpress"
    , sid "haskell-xhtml"
    , sid "haskell-xml"
    , debianize "xml-enumerator" []
    , hackdeb release "xml-types" []
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

-- |Transitional - hackage for lucid, debianize for natty
hackdeb "natty-seereason" name _ = debianize name []
hackdeb release@"lucid-seereason" name flags = hackage release name flags
hackdeb release _ _ = error $ "Unexpected release: " ++ release

data Flag
    = Pin String   -- ^ Pin version number instead of using the most recent.  These arise when
                   -- a new hackage version appears but we aren't ready to migrate.
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is generally the _home parameter to targets.
    deriving Eq
