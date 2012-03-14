{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Targets.Public ( targets ) where

import Data.Char (toLower)
import Data.Set (empty, singleton)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Targets.Common (repo, localRepo, happstackRepo)

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> P.Packages
targets _home release =
    P.Packages empty $
    [ main _home release
    , autobuilder _home
    , authenticate
    , happstackdotcom
    -- , fixme
    -- , higgsset
    -- , jsonb
    -- , glib
    -- , plugins
    -- , frisby
    -- , failing
    -- , algebra
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
    [ -- , debianize "progress" []
      -- , debianize "Unixutils" []
      P.Package { P.name = "Unixutils"
                , P.spec = Darcs (repo ++ "/haskell-unixutils")
                , P.flags = [] }
    , P.Package { P.name = "haskell-progress"
                , P.spec = Debianize (Cd "progress" (Darcs (repo ++ "/haskell-unixutils")))
                , P.flags = [] }
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra"
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help"
                , P.flags = [] } ]

autobuilder _home =
    P.Packages (singleton "autobuilder") $
    [ unixutils _home
    , P.Package { P.name = "autobuilder"
                , P.spec = Darcs (repo ++ "/autobuilder")
                , P.flags = [] }
    , P.Package { P.name = "haskell-cabal-debian"
                , P.spec = Darcs (repo ++ "/cabal-debian")
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian"
                , P.spec = Darcs (repo ++ "/haskell-debian")
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror"
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Darcs "http://src.seereason.com/haskell-debian-repo"
                , P.flags = [] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive"
                , P.flags = [] } ]

main :: String -> String -> P.Packages
main _home release =
    P.Packages (singleton "main") $
    [ -- ghc,
      -- platform release,
      debianize "hashtables" []
    , P.Package { P.name = "cpphs"
                , P.spec = Apt "sid" "cpphs"
                , P.flags = [] }
    , P.Package { P.name = "debootstrap"
                , P.spec = Apt "sid" "debootstrap"
                , P.flags = [P.UDeb "debootstrap-udeb"] }
    , apt release "geneweb"
    , P.Package { P.name = "gtk2hs-buildtools"
                , P.spec = Debianize (Hackage "gtk2hs-buildtools")
                , P.flags =
                    [ P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"
                    , P.ExtraDep "alex"
                    , P.ExtraDep "happy"
                    , P.Revision "" ] }
    , P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization"
                , P.flags = [] }
    , debianize "acid-state" []
    , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize "aeson" []
    , P.Package { P.name = "haskell-agi"
                , P.spec = Darcs "http://src.seereason.com/haskell-agi"
                , P.flags = [] }
    , debianize "ansi-terminal" [P.DebVersion "0.5.5-1"]
    , debianize "ansi-wl-pprint" []
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = DebDir (Hackage "applicative-extras")
                                  (Darcs "http://src.seereason.com/applicative-extras-debian"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "asn1-data" []
    , debianize "attempt" []
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
    -- , debianize "fb" [] []
    , debianize "base-unicode-symbols" []
    , apt release "haskell-base64-bytestring"
    , debianize "bimap" [P.DebVersion "0.2.4-1~hackage1"]
    , debianize "bitmap" []
    , debianize "bitset" [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-blaze-builder"
    , apt release "haskell-blaze-builder-enumerator"
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
                , flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
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
    , clckwrks
    , case release of
        "natty-seereason" -> debianize "colour" []
        _ -> apt release "haskell-colour"
    -- , apt release "haskell-configfile"
    , debianize "ConfigFile" []
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer"
                , P.flags = [] }
    , debianize "cprng-aes" [P.DebVersion "0.2.3-1~hackage1"]
    , patched "Crypto"
                    [ P.DebVersion "4.2.4-1~hackage1"]
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
                      , " instance Hash Hash384 where" ])
    , debianize "crypto-api" []
    , debianize "crypto-pubkey-types" []
    , debianize "cryptocipher" [P.DebVersion "0.3.0-1~hackage1"]
    , debianize "cryptohash" [P.DebVersion "0.7.4-1~hackage1"]
    , debianize "css" [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" []
    , apt release "haskell-curl"
    , debianize "data-accessor" []
    , debianize "data-accessor-template" []
    , debianize "data-default" [P.DebVersion "0.3.0-1~hackage1"]
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

    , apt release "haskell-diff"
    , apt release "haskell-digest"
    , debianize "digestive-functors" []
    , debianize "digestive-functors-blaze" [P.DebVersion "0.2.1.0-1~hackage1"]
    , debianize "digestive-functors-happstack" []
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Darcs (repo ++ "/digestive-functors-hsp")
                , P.flags = [] }
    , apt release "haskell-dlist"
    -- Natty only(?)
    , debianize "double-conversion" []
    , apt release "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Apt "sid" "haskell-edison-api"
                , P.flags = [] }
    , apt release "haskell-edison-core"
    , apt release "haskell-entropy"
    , apt release "haskell-enumerator"
    , apt release "haskell-erf"
    , apt release "haskell-feed"
    , debianize "file-embed" [P.DebVersion "0.0.4.1-1~hackage1"]
    , P.Package { P.name = "haskell-formlets"
                , P.spec = DebDir (Hackage "formlets") (Darcs "http://src.seereason.com/haskell-formlets-debian")
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp")
                , P.flags = [] }
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
    , patched "happstack" []
                    (unlines
                      [ "--- old/happstack.cabal\t2012-02-17 10:29:02.000000000 -0800"
                      , "+++ new/happstack.cabal\t2012-02-19 07:54:49.703458782 -0800"
                      , "@@ -32,7 +32,6 @@"
                      , "                        happstack-data   >= 6.0 && < 6.1,"
                      , "                        happstack-ixset  >= 6.0 && < 6.2,"
                      , "                        happstack-server >= 6.0 && < 6.7,"
                      , "-                       happstack-state  >= 6.0 && < 6.2,"
                      , "                        happstack-util   >= 6.0 && < 6.1"
                      , "                        " ])
    , debianize "happstack-data" []
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-facebook"
                , P.spec = Darcs (repo ++ "/happstack-facebook")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-hsp"
                , P.spec = DebDir (Cd "happstack-hsp" (Darcs happstackRepo)) (Darcs (repo ++ "/happstack-hsp-debian"))
                , P.flags = [] }
    -- Version 6.1.0, which is just a wrapper around the non-happstack
    -- ixset package, has not yet been uploaded to hackage.
    -- , debianize "happstack-ixset" []
    , P.Package { P.name = "haskell-happstack-ixset"
                , P.spec = DebDir (Cd "happstack-ixset" (Darcs happstackRepo)) (Darcs (repo ++ "/happstack-ixset-debian"))
                , P.flags = [] }

    , debianize "happstack-jmacro" []
    , P.Package { P.name = "haskell-happstack-scaffolding"
                , P.spec = Darcs (repo ++ "/happstack-scaffolding")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search")
                , P.flags = [] }
    , debianize "happstack-server" []
    , debianize "happstack-util" []
    , apt release "haskell-harp"
    , debianize "hashable" []
    , debianize "hashed-storage" [P.DebVersion "0.5.9-1"]
    , debianize "haskeline" []
    , debianize "haskell-src-meta" []
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize "HaXml" [P.DebVersion "1:1.22.5-1~hackage1"]
    , debianize "heap" [P.DebVersion "1.0.0-1~hackage1"]
    , debianize "heist" []
    , debianize "xmlhtml" []
    , debianize "directory-tree" []
    , debianize "MonadCatchIO-transformers" []
    , debianize "hinotify" [P.DebVersion "0.3.2-1~hackage1"]
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    , patched "HJScript"
                    [ P.DebVersion "0.5.0-2" ]
                    (unlines
                      [ "--- old-HJScript-0.5.0/src/HJScript/XMLGenerator.hs\t2012-01-01 14:25:05.000000000 -0800"
                      , "+++ new-HJScript-0.5.0/src/HJScript/XMLGenerator.hs\t2012-01-02 09:18:41.226482198 -0800"
                      , "@@ -6,8 +6,7 @@"
                      , "         ) where"
                      , " "
                      , " import qualified HSX.XMLGenerator as HSX (XMLGen(..))"
                      , "-import HSX.XMLGenerator hiding (XMLGen(..))"
                      , "-import HSX.XMLGenerator (genElement, genEElement)"
                      , "+import HSX.XMLGenerator"
                      , " "
                      , " import HJScript.Monad"
                      , " import HJScript.Lang"
                      , "@@ -18,14 +18,14 @@"
                      , " import HJScript.DOM.TextNode"
                      , " import HJScript.DOM.Document"
                      , " "
                      , "-type XML   = Exp ElementNode"
                      , "-type Child = Exp Node"
                      , "-type Attribute = Exp AttributeNode"
                      , "+type XML'   = Exp ElementNode"
                      , "+type Child' = Exp Node"
                      , "+type Attribute' = Exp AttributeNode"
                      , " "
                      , " instance HSX.XMLGen HJScript' where"
                      , "- type HSX.XML          HJScript' = XML"
                      , "- newtype HSX.Child     HJScript' = HJSChild Child"
                      , "- newtype HSX.Attribute HJScript' = HJSAttr Attribute"
                      , "+ type XML          HJScript' = XML'"
                      , "+ newtype Child     HJScript' = HJSChild Child'"
                      , "+ newtype Attribute HJScript' = HJSAttr Attribute'"
                      , "  genElement = element"
                      , "  genEElement = eElement"
                      , "  xmlToChild = HJSChild . castToNode"
                      , "@@ -33,7 +33,7 @@"
                      , " "
                      , " element :: (EmbedAsChild HJScript' c, "
                      , "             EmbedAsAttr HJScript' a) "
                      , "-            => Name -> [a] -> [c] -> HJScript XML"
                      , "+            => Name -> [a] -> [c] -> HJScript XML'"
                      , " element (ns, ln) atts xmls = do"
                      , "   let name = (maybe id (\\x y -> y ++ ':':x) ns) ln"
                      , "   elem <- fmap val $ varWith $ document # createElement (string name)"
                      , "@@ -43,8 +43,8 @@"
                      , "   mapM (\\child -> elem # appendChild child) $ map stripChild cxml"
                      , "   return elem"
                      , " "
                      , "-eElement :: EmbedAsAttr HJScript' a => Name -> [a] -> HJScript XML"
                      , "-eElement n attrs = element n attrs ([] :: [Child])"
                      , "+eElement :: EmbedAsAttr HJScript' a => Name -> [a] -> HJScript XML'"
                      , "+eElement n attrs = element n attrs ([] :: [Child'])"
                      , " "
                      , " "
                      , " instance XMLGenerator HJScript'"
                      , "@@ -52,7 +52,7 @@"
                      , " --------------------------------------------"
                      , " -- EmbedAsChild and EmbedAsAttr"
                      , " "
                      , "-instance EmbedAsChild HJScript' Child where"
                      , "+instance EmbedAsChild HJScript' Child' where"
                      , "  asChild = asChild . HJSChild"
                      , " "
                      , " instance EmbedAsChild HJScript' JString where"
                      , "@@ -66,10 +66,10 @@"
                      , " "
                      , " -- This instance should already be there, probably doesn't work due"
                      , " -- to type families not being fully supported yet."
                      , "-instance EmbedAsChild HJScript' XML where"
                      , "+instance EmbedAsChild HJScript' XML' where"
                      , "  asChild = return . return . HSX.xmlToChild"
                      , " "
                      , "-instance EmbedAsAttr HJScript' Attribute where"
                      , "+instance EmbedAsAttr HJScript' Attribute' where"
                      , "  asAttr = asAttr . HJSAttr"
                      , " "
                      , " instance (IsName n, IsAttrNodeValue a) => EmbedAsAttr HJScript' (Attr n a) where"
                      , "@@ -94,14 +94,14 @@"
                      , " -----------------------------------"
                      , " -- SetAttr and AppendChild."
                      , " "
                      , "-instance SetAttr HJScript' XML where"
                      , "+instance SetAttr HJScript' XML' where"
                      , "  setAll en ats = do"
                      , "         ev <- inVar en"
                      , "         as <- ats"
                      , "         mapM (\\attr -> ev # setAttributeNode attr) (map stripAttr as)"
                      , "         return ev"
                      , " "
                      , "-instance AppendChild HJScript' XML where"
                      , "+instance AppendChild HJScript' XML' where"
                      , "  appAll en cns = do"
                      , "         ev <- inVar en"
                      , "         cs <- cns" ])
    , debianize "hoauth" []
    , debianize "hostname" [P.DebVersion "1.0-1~hackage1"]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" []
    , debianize "hs-bibutils" [P.DebVersion "4.12-1~hackage1"]
    , apt release "haskell-hsemail"
    , patched "HsOpenSSL"
                    [ P.ExtraDevDep "libssl-dev"
                    , P.ExtraDevDep "libcrypto++-dev" ]
                    (unlines
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
                      , "       CPP-Options:        -DCALLCONV=ccall" ])
    , patched "hsp"
                    [ P.ExtraDep "trhsx"
                    , P.DebVersion "0.6.1-1" ]
                    (unlines
                      [ "--- old/src/HSP/XMLGenerator.hs\t2012-01-04 09:58:57.966483200 -0800"
                      , "+++ new/src/HSP/XMLGenerator.hs\t2012-01-04 10:00:35.316482037 -0800"
                      , "@@ -31,9 +31,15 @@"
                      , " \r"
                      , " -- | We can use literal XML syntax to generate values of type XML in the HSP monad.\r"
                      , " instance Monad m => HSX.XMLGen (HSPT' m) where\r"
                      , "+#if __GLASGOW_HASKELL__ < 702\r"
                      , "  type HSX.XML (HSPT' m) = XML\r"
                      , "  newtype HSX.Attribute (HSPT' m) = HSPAttr Attribute \r"
                      , "  newtype HSX.Child     (HSPT' m) = HSPChild XML\r"
                      , "+#else\r"
                      , "+ type XML (HSPT' m) = XML\r"
                      , "+ newtype Attribute (HSPT' m) = HSPAttr Attribute \r"
                      , "+ newtype Child     (HSPT' m) = HSPChild XML\r"
                      , "+#endif\r"
                      , "  xmlToChild = HSPChild\r"
                      , "  pcdataToChild = HSX.xmlToChild . pcdata\r"
                      , "  genElement = element\r" ])
    , debianize "HsSyck" []
    , debianize "HStringTemplate" []
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize "hsx" [P.DebVersion "0.9.1-1"]
    , P.Package { P.name = "haskell-hsx-jmacro"
                , P.spec = DebDir (Cd "hsx-jmacro" (Darcs happstackRepo)) (Darcs (repo ++ "/haskell-hsx-jmacro-debian"))
                , P.flags = []
                }
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities"
                , P.flags = [] }
    -- We need the epoch to stay ahead of the debian and ubuntu packages.
    , debianize "http-types" []
    , debianize "i18n" [P.DebVersion "0.3-1~hackage1"]
    , debianize "iconv" []
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian")
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "instant-generics" []
    , debianize "irc" []
    , debianize "ixset" [P.DebVersion "1.0.2-1~hackage1"]
    , patched "jmacro"
                    [ P.DebVersion "0.5.2-1~hackage1" ]
                    (unlines
                      [ "--- old/Language/Javascript/JMacro/Base.hs\t2012-01-20 11:07:56.000000000 -0800"
                      , "+++ new/Language/Javascript/JMacro/Base.hs\t2012-01-20 11:32:03.207161474 -0800"
                      , "@@ -46,7 +46,7 @@"
                      , " import qualified Data.Set as S"
                      , " import qualified Data.Map as M"
                      , " import Data.Generics"
                      , "-import Data.Monoid"
                      , "+import Data.Monoid (Monoid(..))"
                      , " "
                      , " import Numeric(showHex)"
                      , " import Safe"
                      , "--- old/Language/Javascript/JMacro/Util.hs\t2012-01-02 10:57:46.000000000 -0800"
                      , "+++ new/Language/Javascript/JMacro/Util.hs\t2012-01-02 11:01:26.866482026 -0800"
                      , "@@ -1,6 +1,6 @@"
                      , " module Language.Javascript.JMacro.Util where"
                      , " "
                      , "-import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)"
                      , "+import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read, (&&), (<))"
                      , " "
                      , " import qualified Prelude as P"
                      , " import Language.Javascript.JMacro.Base" ])
    , P.Package { P.name = "haskell-json"
                , P.spec = Darcs (repo ++ "/haskell-json")
                , P.flags = [] }
    , debianize "language-css" [P.DebVersion "0.0.4.1-1~hackage1"]
    , apt release "haskell-largeword"
{-  , apt release "haskell-leksah"
    , apt release "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , patched "logic-TPTP" [ P.ExtraDep "alex"
                           , P.ExtraDep "happy"
                           , P.DebVersion "0.3.0.1-1~hackage1" ]
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
    -- "0.2.0.3" is the version to get from hackage,
    -- "0.2.0.3-1~hackage1" is the version to base our debian version
    -- on.  Both flags should be removed when we move to 0.3, but we
    -- need base 4.4 for that.
    , debianize "monad-control" []
    , debianize "monad-par" []
    , apt release "haskell-monadcatchio-mtl"
    , debianize "monadLib" [P.DebVersion "3.6.2-1~hackage1"]
    , debianize "monads-tf" [P.DebVersion "0.1.0.0-1~hackage1"]
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
                      [ "--- old/openid.cabal\t2012-01-23 15:04:29.547162493 -0800"
                      , "+++ new/openid.cabal\t2012-01-23 15:04:38.637160245 -0800"
                      , "@@ -28,11 +28,11 @@"
                      , " library"
                      , "   build-depends:   base       >= 4.0.0.0  && < 5.0.0.0,"
                      , "                    bytestring >= 0.9.1.0  && < 0.10.0.0,"
                      , "-                   containers >= 0.2.0.0  && < 0.4.1.0,"
                      , "-                   HTTP       >= 4000.0.9 && < 4000.2,"
                      , "+                   containers >= 0.2.0.0,"
                      , "+                   HTTP       >= 4000.0.9,"
                      , "                    monadLib   >= 3.6.0.0  && < 3.7.0.0,"
                      , "                    network    >= 2.2.0.0  && < 2.4.0.0,"
                      , "-                   time       >= 1.1.0.0  && < 1.3.0.0,"
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
    , debianize "operational" [P.DebVersion "0.2.0.3-1~hackage1", P.OmitLTDeps]
    , debianize "ordered" []
    , debianize "pandoc" []
    , debianize "texmath" []
    , debianize "temporary" []
    , debianize "pandoc-types" []
    , debianize "parse-dimacs" [P.DebVersion "1.2-1~hackage1"]
    , debianize "parseargs" [P.DebVersion "0.1.3.2-1~hackage1"]
    , apt release "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2") (Darcs "http://src.seereason.com/pbkdf2-debian"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt release "haskell-pcre-light"
    , debianize "permutation" [P.DebVersion "0.4.1-1~hackage1"]
    , debianize "polyparse" []
    , apt release "haskell-primitive"
    , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs (repo ++ "/haskell-proplogic-debian"))
                , P.flags = [] }
    {- , P.Package { P.name = "haskell-propositional-classes"
                , P.spec = Darcs (repo ++ "/propositional-classes")
                , P.flags = [] } -}
    , debianize "PSQueue" [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-puremd5"
    , debianize "pwstore-purehaskell" [P.DebVersion "2.1-1~hackage1"]
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1. 
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    , apt release "haskell-quickcheck1"
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- , apt release "haskell-regex-tdfa"
    , debianize "regex-tdfa" []
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision"
                , P.flags = [] }
    , debianize "RJson" []
    , debianize "RSA" [P.DebVersion "1.0.6.2-1~hackage1"]
    , apt release "haskell-safe"
    -- Depends on pandoc
    --, P.Package {P.name = "haskell-safecopy", P.spec = DebDir (Hackage "safecopy" [P.CabalPin "0.5.1"])) (Darcs "http://src.seereason.com/haskell-safecopy-debian" []), P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , debianize "safecopy" []
{-  , P.Package { P.name = "haskell-safecopy05"
                , P.spec = Quilt (Hackage "safecopy" [P.CabalPin "0.5.1"])) (Darcs (repo ++ "/safecopy05-quilt") [])
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] } -}
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
    , P.Package { P.name = "haskell-seereason-base"
                , P.spec = Darcs (repo ++ "/seereason-base")
                , P.flags = [] }
    , debianize "semigroups" [P.DebVersion "0.8-1"]
    , debianize "sendfile" []
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra"
                , P.flags = [] }
    , apt release "haskell-sha"
    , debianize "shakespeare" []
    , debianize "shakespeare-css" []
    , debianize "simple-css" [P.DebVersion "0.0.4-1~hackage1"]
    , debianize "SMTPClient" [P.DebVersion "1.0.4-2"]
    , debianize "socks" []
    , debianize "split" []
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , debianize "haskell-src-exts" [P.ExtraDep "happy", P.DebVersion "1.11.1-1"]
    , debianize "stb-image" []
    , apt release "haskell-strict" -- for leksah
    , apt release "haskell-strict-concurrency"
    , patched "strict-io" -- for GenI
                    []
                    (unlines
                      [ "--- old/strict-io.cabal\t2012-01-04 10:25:10.046482116 -0800"
                      , "+++ new/strict-io.cabal\t2012-01-04 10:25:32.276482160 -0800"
                      , "@@ -14,7 +14,7 @@"
                      , " build-type:      Simple"
                      , " "
                      , " library"
                      , "-  build-depends:   base>=3.0 && <5, deepseq==1.1.*, extensible-exceptions"
                      , "+  build-depends:   base>=3.0 && <5, deepseq>=1.1 && <1.4, extensible-exceptions"
                      , "   exposed-modules: System.IO.Strict"
                      , "                    System.IO.Strict.Internals"
                      , "                    Data.IORef.Strict" ])
    , debianize "smallcheck" []
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb-with-class" []
    , apt release "haskell-syb-with-class-instances-text"
    , debianize "tagged" [P.DebVersion "0.2.3.1-1"]
    , debianize "tagsoup" []
    , debianize "tar" []
    , apt release "haskell-terminfo"
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
    , patched "test-framework-quickcheck" []
                    (unlines
                      [ "--- old/test-framework-quickcheck.cabal\t2012-02-02 16:33:53.000000000 -0800"
                      , "+++ new/test-framework-quickcheck.cabal\t2012-02-02 18:10:11.000000000 -0800"
                      , "@@ -26,8 +26,7 @@"
                      , "         if flag(base3)"
                      , "                 Build-Depends:          base >= 3 && < 4, random >= 1, deepseq >= 1.1 && < 1.3"
                      , "         else"
                      , "-                if flag(base4)"
                      , "-                        Build-Depends:          base >= 4 && < 5, random >= 1, deepseq >= 1.1 && < 1.3"
                      , "+                Build-Depends:          base >= 4 && < 5, random >= 1, deepseq >= 1.1"
                      , " "
                      , "         Extensions:             TypeSynonymInstances"
                      , "                                 TypeOperators" ])
    , debianize "test-framework-quickcheck2" []
    , debianize "testpack" []
    , debianize "th-expand-syns" []
    , debianize "th-lift" []
    , debianize "transformers-base" []
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
    , debianize "unix-compat" []
    , debianize "Unixutils-shadow" []
    , debianize "unordered-containers" []
    , debianize "utf8-prelude" [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt "sid" "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize "unification-fd" []
    , debianize "logict" []
    , apt release "haskell-utility-ht"
    , debianize "vacuum" [P.DebVersion "1.0.0.2-1~hackage1"]
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt release "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" []
    , apt release "haskell-vector-algorithms"
    , patched "virthualenv"
                    [ P.DebVersion "0.2-1~hackage1" ]
                    (unlines
                      [ "--- old/virthualenv.cabal\t2012-01-20 11:56:21.000000000 -0800"
                      , "+++ new/virthualenv.cabal\t2012-01-20 13:09:01.237161527 -0800"
                      , "@@ -98,11 +98,11 @@"
                      , " "
                      , "   Ghc-options: -threaded -Wall"
                      , " "
                      , "-  Build-depends: base >= 4.2.0.0 && < 4.5"
                      , "+  Build-depends: base >= 4.2.0.0"
                      , "                , process >= 1.0.1.2 && < 1.2"
                      , "-               , filepath >= 1.1.0.3 && < 1.3"
                      , "+               , filepath >= 1.1.0.3"
                      , "                , directory >= 1.0.1.0 && < 1.2"
                      , "-               , Cabal >= 1.8.0.6 && < 1.13"
                      , "+               , Cabal >= 1.8.0.6"
                      , "                , mtl >= 1.1.0.2 && < 2.1"
                      , "                , bytestring >= 0.9.1.7 && < 0.10"
                      , "                , file-embed >= 0.0.4.1 && < 0.1" ])
    , debianize "vault" []
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
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Cd "web-routes" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-boomerang"
                , P.spec = Cd "web-routes-boomerang" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-happstack"
                , P.spec = Cd "web-routes-happstack" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-hsp"
                , P.spec = Cd "web-routes-hsp" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-mtl"
                , P.spec = Cd "web-routes-mtl" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }      
    , P.Package { P.name = "haskell-web-routes-th"
                , P.spec = Cd "web-routes-th" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , apt release "haskell-xml"
    , debianize "cookie" []
    , debianize "lifted-base" []
    , debianize "system-filepath" []
    , patched "xml-enumerator" []
                 (unlines
                   [ "--- old/xml-enumerator.cabal\t2012-01-20 06:47:15.000000000 -0800"
                   , "+++ new/xml-enumerator.cabal\t2012-01-20 09:54:58.246541244 -0800"
                   , "@@ -43,7 +43,7 @@"
                   , "                    , blaze-builder             >= 0.2      && < 0.4"
                   , "                    , blaze-builder-enumerator  >= 0.2      && < 0.3"
                   , "                    , transformers              >= 0.2      && < 0.3"
                   , "-                   , failure                   >= 0.1      && < 0.2"
                   , "+                   , failure                   >= 0.1"
                   , "                    , data-default              >= 0.2      && < 0.4"
                   , "     exposed-modules: Text.XML.Stream.Parse"
                   , "                      Text.XML.Stream.Render" ])
    , debianize "xml-types" []
    , debianize "xss-sanitize" []
    , debianize "yaml-light" []
    , apt release "haskell-zip-archive"
    , debianize "zlib-bindings" []
    , apt release "haskell-zlib-enum"

    , debianize "highlighting-kate" []
    , debianize "regex-pcre-builtin" []
    , P.Package { P.name = "hscolour"
                , P.spec = Apt "sid" "hscolour"
                , P.flags = [P.RelaxDep "hscolour"] }
    , apt release "hslogger"
    , apt release "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery")
                , P.flags = [] }
    , P.Package { P.name = "jquery-goodies"
                , P.spec = Proc (Apt "sid" "jquery-goodies")
                , P.flags = [] }
    , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "sid" "jqueryui")
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "sid" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] }
    , debianize "MissingH" [P.DebVersion "1.1.1.0-1~hackage1"]
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring"
                , P.flags = [P.UDeb "seereason-keyring-udeb"] }
    , apt release "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs"
                , P.flags = [] }
    -- , debianize "hlatex" []
    ]

ghc release =
    let ghc740 = P.Package { P.name = "ghc"
                           , P.spec = Apt "experimental" "ghc"
                           , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] }
        ghc741 = P.Package { P.name = "ghc"
                           , P.spec = Apt "sid" "ghc"
                           , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt","python-minimal","libgmp-dev"] } in
    case release of
      "natty-seereason" -> ghc741 -- P.NoPackage
      _ -> ghc741 -- P.NoPackage

platform release =
    P.Packages (singleton "platform") $
    [ P.Package { P.name = "haskell-devscripts"
                , P.spec = Apt "sid" "haskell-devscripts"
                , P.flags = [P.RelaxDep "python-minimal"] }
    , -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happy/1.18.8/happy-1.18.8.tar.gz" "907d79425351e826fb0bb8b677620418")
                           (Darcs "http://src.seereason.com/happy-debian"),
                           P.flags = [P.RelaxDep "happy", P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    {- , debianize "happy" [] -}
    , apt release "haskell-stm"
    , apt release "haskell-zlib"
    -- , apt "haskell-deepseq"
    , case release of
        "natty-seereason" -> P.NoPackage -- a version newer than the latest in hackage is bundled with ghc
        _ -> P.NoPackage -- debianize "deepseq" []

    , P.Package { P.name = "haskell-mtl"
                , P.spec = Apt "sid" "haskell-mtl"
                , P.flags = [P.AptPin "2.0.1.0-2"] }
    , P.Package { P.name = "haskell-transformers"
                , P.spec = Apt "sid" "haskell-transformers"
                , P.flags = [P.AptPin "0.2.2.0-3"] }
    , debianize "parallel" []
    , debianize "syb" []
    , debianize "fgl" [P.DebVersion "5.4.2.4-1"]
    , debianize "text" []
    , P.Package { P.name = "alex"
                , P.spec = Apt "sid" "alex"
                , P.flags = [P.RelaxDep "alex"] }
    , opengl release
    -- , haddock release
    , debianize "haskell-src" [ P.ExtraDep "happy" ]
    , debianize "network" []
    , debianize "HTTP" []
    , P.Package { P.name = "haskell-cgi"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian")
                , P.flags = [] }
    -- This is bundled with the compiler
    -- , debianize "process" []
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" []
    , apt release "haskell-hunit"
    , debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]
    , apt release "haskell-parsec"
    , apt release "haskell-html"
    , apt release "haskell-regex-compat"
    , apt release "haskell-regex-base"
    , apt release "haskell-regex-posix"
    , debianize "xhtml" []
    ]

-- | Packages pinned pending update of happstack-authenticate
authenticate = P.Packages (singleton "authenticate") $
    let -- pin _ = []
        pin x = [P.CabalPin x] in
    [ debianize "wai" [P.CabalPin "1.0.0"]
    , debianize "xml-conduit" [P.CabalPin "0.5.1.2"]
    , debianize "http-conduit" [P.CabalPin "1.2.0"]
    , debianize "zlib-conduit" [P.CabalPin "0.0.1"]
    , debianize "conduit" [P.CabalPin "0.1.1.1"]
    , debianize "attoparsec-conduit" [P.CabalPin "0.0.1"]
    , debianize "blaze-builder-conduit" [P.CabalPin "0.0.1"]
    , debianize "http-enumerator" [P.CabalPin "0.7.2.5"]
    , debianize "tls" [P.CabalPin "0.8.5"]
    , debianize "tls-extra" [P.CabalPin "0.4.2.1"]
    , debianize "certificate" [P.CabalPin "1.0.1", P.DebVersion "1.0.1-1~hackage1"]
    , patched "authenticate"
                    [ P.CabalPin "0.11.1" ]
                    (unlines
                      [ "--- old/authenticate.cabal\t2012-01-19 19:39:56.000000000 -0800"
                      , "+++ new/authenticate.cabal\t2012-01-20 11:48:58.976223078 -0800"
                      , "@@ -16,9 +16,9 @@"
                      , " library"
                      , "     build-depends:   base >= 4 && < 5,"
                      , "                      aeson >= 0.5,"
                      , "-                     http-conduit >= 1.1 && < 1.2,"
                      , "+                     http-conduit >= 1.1,"
                      , "                      tagsoup >= 0.12 && < 0.13,"
                      , "-                     failure >= 0.0.0 && < 0.2,"
                      , "+                     failure >= 0.0.0,"
                      , "                      transformers >= 0.1 && < 0.3,"
                      , "                      bytestring >= 0.9 && < 0.10,"
                      , "                      network >= 2.2.1 && < 2.4,"
                      , "@@ -37,7 +37,7 @@"
                      , "                      containers,"
                      , "                      unordered-containers,"
                      , "                      process >= 1.0.1.1 && < 1.2,"
                      , "-                     conduit >= 0.0 && < 0.1,"
                      , "+                     conduit >= 0.0,"
                      , "                      blaze-builder-conduit >= 0.0 && < 0.1"
                      , "     exposed-modules: Web.Authenticate.Rpxnow,"
                      , "                      Web.Authenticate.OpenId," ])
    , P.Package { P.name = "haskell-happstack-authenticate"
                , P.spec = Darcs (repo ++ "/happstack-authenticate")
                , P.flags = [] }
    ]

clckwrks =
    P.Packages (singleton "clckwrks") $
        [ P.Package { P.name = "haskell-clckwrks"
                    , P.spec = Debianize (Patch
                                          (DataFiles
                                           (DataFiles
                                            (Cd "clckwrks" (Darcs "http://src.clckwrks.com/clckwrks"))
                                            (Uri "http://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                 "e211065e573ea0239d6449882c9d860d")
                                            "jstree")
                                           (Uri "https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
                                                "2ee84c1e82528e5e09c645cf07c97877")
                                           "json2")
                                          (unlines
                                           [ "--- old/clckwrks.cabal.orig\t2012-03-06 11:34:28.000000000 -0800"
                                           , "+++ new/clckwrks.cabal\t2012-03-07 20:17:25.465527760 -0800"
                                           , "@@ -15,6 +15,79 @@"
                                           , " Cabal-version:       >=1.2"
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
                                           , "   Exposed-modules: Clckwrks" ]))
                    , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
        , P.Package { P.name = "haskell-clckwrks-cli"
                    , P.spec = Debianize (Cd "clckwrks-cli" (Darcs "http://src.clckwrks.com/clckwrks"))
                    , P.flags = [] }
        , P.Package { P.name = "haskell-clckwrks-plugin-media"
                    , P.spec = Debianize (Cd "clckwrks-plugin-media" (Darcs "http://src.clckwrks.com/clckwrks"))
                    , P.flags = [] }
        , P.Package { P.name = "haskell-clckwrks-theme-basic"
                    , P.spec = Debianize (Cd "clckwrks-theme-basic" (Darcs "http://src.clckwrks.com/clckwrks"))
                    , P.flags = [] }
        ]

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
    , debianize "vacuum-opengl" [P.DebVersion "0.0.3-1~hackage2"]
    , debianize "bitmap-opengl" [P.DebVersion "0.0.0-1~hackage1"]
    , apt release "haskell-glut"
    , debianize "StateVar" []
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
    [ patched "plugins"
                    [ P.DebVersion "1.5.1.4-1~hackage1" ]
                    (unlines
                      [ "--- haskell-plugins-1.5.1.4.orig/src/System/Plugins/Load.hs\t2011-12-07 07:13:54.000000000 -0800"
                      , "+++ haskell-plugins-1.5.1.4/src/System/Plugins/Load.hs\t2012-01-02 10:16:25.766481952 -0800"
                      , "@@ -84,7 +84,9 @@"
                      , " import System.Directory         ( doesFileExist, removeFile )"
                      , " import Foreign.C.String         ( CString, withCString, peekCString )"
                      , " "
                      , "+#if !MIN_VERSION_ghc(7,2,0)"
                      , " import GHC                      ( defaultCallbacks )"
                      , "+#endif"
                      , " import GHC.Ptr                  ( Ptr(..), nullPtr )"
                      , " import GHC.Exts                 ( addrToHValue# )"
                      , " import GHC.Prim                 ( unsafeCoerce# )"
                      , "@@ -99,7 +101,11 @@"
                      , " readBinIface' :: FilePath -> IO ModIface"
                      , " readBinIface' hi_path = do"
                      , "     -- kludgy as hell"
                      , "+#if MIN_VERSION_ghc(7,2,0)"
                      , "+    e <- newHscEnv undefined"
                      , "+#else"
                      , "     e <- newHscEnv defaultCallbacks undefined"
                      , "+#endif"
                      , "     initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)"
                      , " "
                      , " -- TODO need a loadPackage p package.conf :: IO () primitive"
                      , "@@ -679,7 +685,11 @@"
                      , " "
                      , "                 -- and find some packages to load, as well."
                      , "                 let ps = dep_pkgs ds"
                      , "+#if MIN_VERSION_ghc(7,2,0)"
                      , "+                ps' <- filterM loaded . map packageIdString . nub $ map fst ps"
                      , "+#else"
                      , "                 ps' <- filterM loaded . map packageIdString . nub $ ps"
                      , "+#endif"
                      , " "
                      , " #if DEBUG"
                      , "                 when (not (null ps')) $" ])
    , debianize "happstack-plugins" []
    , debianize "plugins-auto" [] ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = P.Packages (singleton "higgsset") $
    [ debianize "unpack-funcs" []
    , debianize "HiggsSet" []
    , debianize "TrieMap" [P.DebVersion "4.0.1-1~hackage1"] ]

-- ircbot needs a dependency on containers
happstackdotcom = P.Packages (singleton "happstackdotcom") $
    [ P.Package { P.name = "haskell-ircbot"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/ircbot"
                , P.flags = []
                }
    , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/happstackDotCom"
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc"
                , P.flags = [] } ]

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

algebra =
    [ debianize "adjunctions" []
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
    , debianize "void" [] ]

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
              , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] ++ flags ++ [P.Revision ""]}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "haskell-quickcheck2"
      debianName "parsec" = "haskell-parsec3"
      debianName "gtk2hs-buildtools" = "gtk2hs-buildtools"
      -- The correct name would be haskell-haskell-src-exts, but the package
      -- in sid has the name "haskell-src-exts".
      debianName "haskell-src-exts" = "haskell-src-exts"
      debianName "MissingH" = "haskell-missingh"
      debianName _ = "haskell-" ++ map toLower s

patched :: String -> [P.PackageFlag] -> String -> P.Packages
patched s flags patch =
    let p = debianize s flags in
    let (Debianize (Hackage s)) = P.spec p in
    p {P.spec = Debianize (Patch (Hackage s) patch)}

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
