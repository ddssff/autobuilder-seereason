{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds #-}
module Targets.Public ( targets ) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (toLower)
import Data.Monoid (mconcat)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec (Spec(..))
import Targets.Common (repo, localRepo, checkUnique, happstackRepo)

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> P.Packages
targets _home release =
    checkUnique $ mconcat $
    [ -- platform release,
      main _home release
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
    [ debianize "test-framework-smallcheck" Newest []
    , P.Package { P.name = "haskell-geni"
                , P.spec = DebDir (Darcs "http://code.haskell.org/GenI" Nothing) (Darcs (repo ++ "/haskell-geni-debian") Nothing)
                , P.flags = [] }
    ]

main :: String -> String -> P.Packages
main _home release =
    checkUnique $ mconcat $
    [ -- platform
      debianize "hashtables" Newest []
    , P.Package { P.name = "autobuilder"
                , P.spec = Darcs (repo ++ "/autobuilder") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-cabal-debian"
                , P.spec = Darcs (repo ++ "/cabal-debian") Nothing
                , P.flags = [] }
    , P.Package { P.name = "cpphs"
                , P.spec = Quilt (Apt "sid" "cpphs" Nothing) (Darcs "http://src.seereason.com/cpphs-quilt" Nothing)
                , P.flags = [] }
    , apt release "debootstrap"
    , apt release "geneweb"
    , debianize "gtk2hs-buildtools" Newest
                                    [ P.ExtraDep "alex"
                                    , P.ExtraDep "happy"
                                    , P.Patch . B.pack . unlines $
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
                                      , "+        GHC-options:    -XBangPatterns" ] ]
    , P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization" Nothing
                , P.flags = [] }
    , debianize "acid-state" Newest []
    , debianize "AES" Newest [P.DebVersion "0.2.8-1~hackage1"]
    , debianize "aeson" Newest []
    , P.Package { P.name = "haskell-agi"
                , P.spec=Darcs "http://src.seereason.com/haskell-agi" Nothing
                , P.flags = [] }
    , debianize "ansi-terminal" Newest [P.DebVersion "0.5.5-1"]
    , debianize "ansi-wl-pprint" Newest
                    [ {- P.DebVersion "0.6.3-1~hackage1",
                      P.Patch . B.pack . unlines $
                      [ "--- old/Text/PrettyPrint/ANSI/Leijen.hs\t2012-01-20 06:40:42.000000000 -0800"
                      , "+++ new/Text/PrettyPrint/ANSI/Leijen.hs\t2012-01-20 08:26:55.847161750 -0800"
                      , "@@ -152,7 +152,7 @@"
                      , " "
                      , " import Data.String (IsString(..))"
                      , " import Data.Maybe (isNothing, fromMaybe, catMaybes)"
                      , "-import Data.Monoid"
                      , "+import Data.Monoid (Monoid(..))"
                      , " "
                      , " "
                      , " infixr 5 </>,<//>,<$>,<$$>" ] -} ]
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = DebDir (Hackage "applicative-extras" Nothing)
                                  (Darcs "http://src.seereason.com/applicative-extras-debian" Nothing),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive" Nothing
                , P.flags = [] }
    , debianize "asn1-data" Newest []
    , debianize "attempt" Newest []
    , debianize "failure" Newest []
    , debianize "attoparsec" Newest []
    , debianize "attoparsec-enumerator" Newest []
    , debianize "attoparsec-text" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "                    Data.Attoparsec.Text.FastSet" ] ]
    , debianize "attoparsec-text-enumerator" Newest []
    , debianize "authenticate" (Pin "0.11.1")
                    [ P.Patch . B.pack . unlines $
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
                      , "                      Web.Authenticate.OpenId," ] ]
    , debianize "base-unicode-symbols" Newest []
    , apt release "haskell-base64-bytestring"
    , debianize "bimap" Newest [P.DebVersion "0.2.4-1~hackage1"]
    , debianize "bitmap" Newest
                    [ P.Patch . B.pack . unlines $
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
                    , P.DebVersion "0.0.1-1~hackage1" ]
    , debianize "bitset" Newest [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-blaze-builder"
    , apt release "haskell-blaze-builder-enumerator"
    , debianize "blaze-from-html" Newest []
    , debianize "blaze-html" Newest []
    , debianize "blaze-textual" Newest []
    , debianize "blaze-textual-native" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , " {-# RULES \"integral/Int16\" integral = bounded :: Int16 -> Builder #-}" ] ]
    , apt release "haskell-bytestring-nums"
    , debianize "bytestring-trie" Newest []
    , P.Package { P.name = "haskell-bzlib"
                , P.spec = Quilt (Apt "sid" "haskell-bzlib" Nothing) (Darcs "http://src.seereason.com/haskell-bzlib-quilt" Nothing)
                , P.flags = [] }
    -- , debianize "cairo-pdf" Newest []
    , debianize "case-insensitive" Newest []
    , debianize "CC-delcont" Newest [P.DebVersion "0.2-1~hackage1"]
    , apt release "haskell-cereal"
    , debianize "certificate" Newest [P.DebVersion "1.0.1-1~hackage1"]
    , debianize "citeproc-hs" Newest []
    , case release of
        "natty-seereason" -> debianize "colour" Newest []
        _ -> apt release "haskell-colour"
    -- , apt release "haskell-configfile"
    , debianize "ConfigFile" Newest []
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer" Nothing
                , P.flags = [] }
    , debianize "cprng-aes" Newest [P.DebVersion "0.2.3-1~hackage1"]
    , debianize "Crypto" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , " instance Hash Hash384 where" ]
                    , P.DebVersion "4.2.4-1~hackage1"]
    , debianize "crypto-api" Newest []
    , debianize "crypto-pubkey-types" Newest [P.DebVersion "0.1.0-1~hackage1"]
    , debianize "cryptocipher" Newest [P.DebVersion "0.3.0-1~hackage1"]
    , debianize "cryptohash" Newest [P.DebVersion "0.7.4-1~hackage1"]
    , debianize "css" Newest [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" Newest []
    , apt release "haskell-curl"
    , debianize "data-accessor" Newest []
    , debianize "data-accessor-template" Newest []
    , debianize "data-default" Newest [P.DebVersion "0.3.0-1~hackage1"]
    , debianize "data-object" Newest
                    [ P.Patch . B.pack . unlines $
                      [ "--- old/data-object.cabal\t2012-01-20 06:42:12.000000000 -0800"
                      , "+++ new/data-object.cabal\t2012-01-20 10:13:25.147160370 -0800"
                      , "@@ -20,6 +20,6 @@"
                      , "                      bytestring >= 0.9.1.4,"
                      , "                      text >= 0.5,"
                      , "                      time >= 1.1.4,"
                      , "-                     failure >= 0.1.0 && < 0.2"
                      , "+                     failure >= 0.1.0"
                      , "     exposed-modules: Data.Object"
                      , "     ghc-options:     -Wall" ] ]
    , debianize "dataenc" Newest []
    , P.Package { P.name = "haskell-debian"
                , P.spec = Darcs (repo ++ "/haskell-debian-new") Nothing
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Darcs "http://src.seereason.com/haskell-debian-repo" Nothing
                , P.flags = [] }

    , apt release "haskell-diff"
    , apt release "haskell-digest"
    , debianize "digestive-functors" Newest []
    , debianize "digestive-functors-blaze" Newest [P.DebVersion "0.2.1.0-1~hackage1"]
    , debianize "digestive-functors-happstack" Newest []
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Darcs (repo ++ "/digestive-functors-hsp") Nothing
                , P.flags = [] }
    , apt release "haskell-dlist"
    -- Natty only(?)
    , debianize "double-conversion" Newest []
    , apt release "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Quilt (Apt "sid" "haskell-edison-api" Nothing) (Darcs (repo ++ "/haskell-edison-api-quilt") Nothing)
                , P.flags = [] }
    , apt release "haskell-edison-core"
    , apt release "haskell-entropy"
    , apt release "haskell-enumerator"
    , apt release "haskell-erf"
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra" Nothing
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , apt release "haskell-feed"
    , debianize "file-embed" Newest [P.DebVersion "0.0.4.1-1~hackage1"]
    , P.Package { P.name = "haskell-formlets"
                , P.spec = DebDir (Hackage "formlets" Nothing) (Darcs "http://src.seereason.com/haskell-formlets-debian" Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp") Nothing
                , P.flags = [] }
    , debianize "gd" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "   Install-includes: gd-extras.h" ] ]
    -- , debianize "gd" Newest [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , debianize "cabal-macosx" Newest []
    , apt release "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , debianize "happstack" Newest []
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

    , debianize "happstack-jmacro" Newest []
    , P.Package { P.name = "haskell-happstack-scaffolding"
                , P.spec = Darcs (repo ++ "/happstack-scaffolding") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search") Nothing
                , P.flags = [] }
    , debianize "happstack-server" Newest []
    , debianize "happstack-util" Newest []
    , apt release "haskell-harp"
    , debianize "hashable" Newest [P.DebVersion "1.1.2.2-1"]
    , debianize "hashed-storage" Newest [P.DebVersion "0.5.9-1"]
    , debianize "haskeline" Newest []
    , debianize "haskell-src-meta" Newest []
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize "HaXml" Newest [P.Epoch 1, P.DebVersion "1:1.22.5-1~hackage1"]
    , debianize "heap" Newest [P.DebVersion "1.0.0-1~hackage1"]
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help" Nothing
                , P.flags = [] }
    , debianize "heist" Newest []
    , debianize "xmlhtml" Newest []
    , debianize "directory-tree" Newest []
    , debianize "MonadCatchIO-transformers" Newest []
    , debianize "hinotify" Newest [P.DebVersion "0.3.2-1~hackage1"]
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript" Nothing) (Darcs (repo ++ "/hjavascript-quilt") Nothing)
                , P.flags = [] }
    , debianize "HJScript" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "         cs <- cns" ]
                    , P.DebVersion "0.5.0-2" ]
    , debianize "hoauth" Newest []
    , debianize "hostname" Newest [P.DebVersion "1.0-1~hackage1"]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" Newest []
    , debianize "hs-bibutils" Newest [P.DebVersion "4.12-1~hackage1"]
    , apt release "haskell-hsemail"
    , debianize "HsOpenSSL" Newest
                    [ P.ExtraDevDep "libssl-dev"
                    , P.ExtraDevDep "libcrypto++-dev"
                    , P.Patch . B.pack . unlines $
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
    , debianize "hsp" Newest
                    [ P.ExtraDep "trhsx"
                    , P.Patch . B.pack . unlines $
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
                      , "  genElement = element\r" ]
                    , P.DebVersion "0.6.1-1" ]
    , debianize "HsSyck" Newest []
    , debianize "HStringTemplate" Newest []
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize "hsx" Newest [P.DebVersion "0.9.1-1"]
    , P.Package { P.name = "haskell-hsx-jmacro"
                , P.spec = DebDir (Cd "hsx-jmacro" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/haskell-hsx-jmacro-debian") Nothing)
                , P.flags = []
                }
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities" Nothing
                , P.flags = [] }
    -- We need the epoch to stay ahead of the debian and ubuntu packages.
    , debianize "http-enumerator" Newest []
    , debianize "http-types" Newest []
    , debianize "i18n" Newest [P.DebVersion "0.3-1~hackage1"]
    , debianize "iconv" Newest []
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver" Nothing) (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian" Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "instant-generics" Newest []
    , debianize "irc" Newest []
    , debianize "ixset" Newest [P.DebVersion "1.0.2-1~hackage1"]
    , debianize "jmacro" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , " import Language.Javascript.JMacro.Base" ]
                    , P.DebVersion "0.5.2-1~hackage1" ]
    , P.Package { P.name = "haskell-json"
                , P.spec = Darcs (repo ++ "/haskell-json") Nothing
                , P.flags = [] }
    , debianize "language-css" Newest [P.DebVersion "0.0.4.1-1~hackage1"]
    , apt release "haskell-largeword"
{-  , apt release "haskell-leksah"
    , apt release "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic") Nothing
                , P.flags = [] }
    , debianize "logic-TPTP" Newest [
                               P.ExtraDep "alex", P.ExtraDep "happy"
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
                               , " import System.IO.Unsafe" ]
                    , P.DebVersion "0.3.0.1-1~hackage1" ]
    , apt release "haskell-maybet"
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime" Nothing
                , P.flags = [] }
    , apt release "haskell-mmap"
    -- "0.2.0.3" is the version to get from hackage,
    -- "0.2.0.3-1~hackage1" is the version to base our debian version
    -- on.  Both flags should be removed when we move to 0.3, but we
    -- need base 4.4 for that.
    , debianize "monad-control" Newest []
    , debianize "monad-par" Newest []
    , apt release "haskell-monadcatchio-mtl"
    , debianize "monadLib" Newest [P.DebVersion "3.6.2-1~hackage1"]
    , debianize "monads-tf" Newest [P.DebVersion "0.1.0.0-1~hackage1"]
    , apt release "haskell-monoid-transformer"
    , debianize "murmur-hash" Newest []
    , apt release "haskell-mwc-random"
    , debianize "nano-hmac" Newest [ P.Patch . B.pack . unlines $
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
                                          [ "--- old/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 16:39:39.603631778 -0700"
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
                                          , "                  x   -> x" ]
                                   , P.DebVersion "0.2.0ubuntu1" ]
    , debianize "openid" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "                  x   -> x" ] ]
    , debianize "operational" Newest [P.OmitLTDeps, P.DebVersion "0.2.0.3-1~hackage1"]
    , debianize "ordered" Newest []
    , debianize "pandoc" Newest
                    [ P.Patch . B.pack . unlines $
                      [ "--- old/pandoc.cabal\t2012-01-02 11:56:02.636482563 -0800"
                      , "+++ new/pandoc.cabal\t2012-01-02 11:56:56.906482704 -0800"
                      , "@@ -194,21 +194,21 @@"
                      , "                  xhtml >= 3000.0 && < 3000.3,"
                      , "                  mtl >= 1.1 && < 2.1,"
                      , "                  network >= 2 && < 2.4,"
                      , "-                 filepath >= 1.1 && < 1.3,"
                      , "-                 process >= 1 && < 1.1,"
                      , "+                 filepath >= 1.1 && < 1.4,"
                      , "+                 process >= 1 && < 1.2,"
                      , "                  directory >= 1 && < 1.2,"
                      , "                  bytestring >= 0.9 && < 1.0,"
                      , "                  zip-archive >= 0.1.1.7 && < 0.2,"
                      , "                  utf8-string >= 0.3 && < 0.4,"
                      , "-                 old-time >= 1 && < 1.1,"
                      , "-                 HTTP >= 4000.0.5 && < 4000.2,"
                      , "+                 old-time >= 1 && < 1.2,"
                      , "+                 HTTP >= 4000.0.5,"
                      , "                  texmath >= 0.5 && < 0.6,"
                      , "                  xml >= 1.3.5 && < 1.4,"
                      , "                  random >= 1 && < 1.1,"
                      , "                  extensible-exceptions >= 0.1 && < 0.2,"
                      , "                  citeproc-hs >= 0.3.1 && < 0.4,"
                      , "                  pandoc-types == 1.8.*,"
                      , "-                 json >= 0.4 && < 0.5,"
                      , "+                 json >= 0.4 && < 0.6,"
                      , "                  dlist >= 0.4 && < 0.6,"
                      , "                  tagsoup >= 0.12 && < 0.13,"
                      , "                  base64-bytestring >= 0.1 && < 0.2"
                      , "@@ -280,21 +280,21 @@"
                      , "                  xhtml >= 3000.0 && < 3000.3,"
                      , "                  mtl >= 1.1 && < 2.1,"
                      , "                  network >= 2 && < 2.4,"
                      , "-                 filepath >= 1.1 && < 1.3,"
                      , "-                 process >= 1 && < 1.1,"
                      , "+                 filepath >= 1.1 && < 1.4,"
                      , "+                 process >= 1 && < 1.2,"
                      , "                  directory >= 1 && < 1.2,"
                      , "                  bytestring >= 0.9 && < 1.0,"
                      , "                  zip-archive >= 0.1.1.7 && < 0.2,"
                      , "                  utf8-string >= 0.3 && < 0.4,"
                      , "-                 old-time >= 1 && < 1.1,"
                      , "-                 HTTP >= 4000.0.5 && < 4000.2,"
                      , "+                 old-time >= 1 && < 1.2,"
                      , "+                 HTTP >= 4000.0.5,"
                      , "                  texmath >= 0.5 && < 0.6,"
                      , "                  xml >= 1.3.5 && < 1.4,"
                      , "                  random >= 1 && < 1.1,"
                      , "                  extensible-exceptions >= 0.1 && < 0.2,"
                      , "                  citeproc-hs >= 0.3.1 && < 0.4,"
                      , "                  pandoc-types == 1.8.*,"
                      , "-                 json >= 0.4 && < 0.5,"
                      , "+                 json >= 0.4 && < 0.6,"
                      , "                  dlist >= 0.4 && < 0.6,"
                      , "                  tagsoup >= 0.12 && < 0.13,"
                      , "                  base64-bytestring >= 0.1 && < 0.2"
                      , "--- old/src/Text/Pandoc/Pretty.hs\t2012-01-20 06:44:57.000000000 -0800"
                      , "+++ new/src/Text/Pandoc/Pretty.hs\t2012-01-20 09:09:11.246816683 -0800"
                      , "@@ -70,7 +70,7 @@"
                      , " where"
                      , " import Data.DList (DList, fromList, toList, cons, singleton)"
                      , " import Data.List (intercalate)"
                      , "-import Data.Monoid"
                      , "+import Data.Monoid (Monoid(..))"
                      , " import Data.String"
                      , " import Control.Monad.State"
                      , " import Data.Char (isSpace)" ]
                    , P.DebVersion "1.8.2.1-1~hackage1" ]
    , apt release "haskell-pandoc-types"
    , debianize "parse-dimacs" Newest [P.DebVersion "1.2-1~hackage1"]
    , debianize "parseargs" Newest [P.DebVersion "0.1.3.2-1~hackage1"]
    , apt release "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2" Nothing) (Darcs "http://src.seereason.com/pbkdf2-debian" Nothing),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt release "haskell-pcre-light"
    , debianize "permutation" Newest [P.DebVersion "0.4.1-1~hackage1"]
    , debianize "polyparse" Newest [P.DebVersion "1.7-1~hackage1"]
    , apt release "haskell-primitive"
    , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs (repo ++ "/haskell-proplogic-debian") Nothing)
                , P.flags = [] }
    {- , P.Package { P.name = "haskell-propositional-classes"
                , P.spec = Darcs (repo ++ "/propositional-classes") Nothing
                , P.flags = [] } -}
    , debianize "PSQueue" Newest [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-puremd5"
    , debianize "pwstore-purehaskell" Newest [P.DebVersion "2.1-1~hackage1"]
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1. 
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    , P.Package { P.name = "haskell-quickcheck1"
                , P.spec = Quilt (Apt "sid" "haskell-quickcheck1" Nothing) (Darcs (repo ++ "/haskell-quickcheck-quilt") Nothing)
                , P.flags = [] }
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- , apt release "haskell-regex-tdfa"
    , debianize "regex-tdfa" Newest []
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision" Nothing
                , P.flags = [] }
    , debianize "RJson" Newest []
    , debianize "RSA" Newest [P.DebVersion "1.0.6.2-1~hackage1"]
    , apt release "haskell-safe"
    -- Depends on pandoc
    --, P.Package {P.name = "haskell-safecopy", P.spec = DebDir (Hackage "safecopy" (Just "0.5.1")) (Darcs "http://src.seereason.com/haskell-safecopy-debian" Nothing), P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , debianize "safecopy" Newest []
{-  , P.Package { P.name = "haskell-safecopy05"
                , P.spec = Quilt (Hackage "safecopy" (Just "0.5.1")) (Darcs (repo ++ "/safecopy05-quilt") Nothing)
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] } -}
    , debianize "sat" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , " main-is: \"SATSolver.hs\"" ]
                    , P.DebVersion "1.1.1-1~hackage1" ]
    , P.Package { P.name = "haskell-seereason-base"
                , P.spec = Darcs (repo ++ "/seereason-base") Nothing
                , P.flags = [] }
    , debianize "semigroups" Newest [P.DebVersion "0.8-1"]
    , debianize "sendfile" Newest []
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra" Nothing
                , P.flags = [] }
    , apt release "haskell-sha"
    , debianize "shakespeare" Newest [P.DebVersion "0.10.2-1~hackage1"]
    , debianize "shakespeare-css" Newest []
    , debianize "simple-css" Newest [P.DebVersion "0.0.4-1~hackage1"]
    , debianize "SMTPClient" Newest [P.DebVersion "1.0.4-2"]
    , debianize "socks" Newest []
    , debianize "split" Newest []
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , debianize "haskell-src-exts" Newest [P.ExtraDep "happy", P.DebVersion "1.11.1-1"]
    , debianize "stb-image" Newest [P.DebVersion "0.2-1~hackage1"]
    , apt release "haskell-strict" -- for leksah
    , apt release "haskell-strict-concurrency"
    , debianize "strict-io" Newest -- for GenI
                    [ P.Patch . B.pack. unlines $
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
                      , "                    Data.IORef.Strict" ] ]
    , debianize "smallcheck" Newest []
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb-with-class" Newest []
    , apt release "haskell-syb-with-class-instances-text"
    , debianize "tagged" Newest [P.DebVersion "0.2.3.1-1"]
    , debianize "tagsoup" Newest []
    , debianize "tar" Newest []
    , apt release "haskell-terminfo"
    , debianize "test-framework" Newest
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
    , debianize "test-framework-hunit" Newest []
    , debianize "test-framework-quickcheck" Newest
                    [ P.Patch . B.pack. unlines $
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
                      , "                                 TypeOperators" ] ]
    , debianize "test-framework-quickcheck2" Newest []
    , debianize "testpack" Newest
                    [ P.Patch . B.pack. unlines $
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
                      , "-" ]
                    , P.DebVersion "2.1.1-1~hackage1" ]
    , apt release "haskell-texmath"
    , debianize "th-expand-syns" Newest []
    , debianize "th-lift" Newest
                    [ P.Patch . B.pack . unlines $
                      [ "--- x/th-lift.cabal.orig\t2012-01-01 11:57:27.446480543 -0800"
                      , "+++ x/th-lift.cabal\t2012-01-01 11:58:07.646482909 -0800"
                      , "@@ -24,7 +24,7 @@"
                      , "     Build-Depends: packedstring == 0.1.*,"
                      , "                    template-haskell >= 2.2 && < 2.4"
                      , "   else"
                      , "-    Build-Depends: template-haskell >= 2.4 && < 2.7"
                      , "+    Build-Depends: template-haskell >= 2.4 && < 2.8"
                      , " "
                      , " source-repository head"
                      , "   type:     git" ]
                    , P.DebVersion "0.5.4-1~hackage1" ]
    , debianize "tls" Newest []
    , debianize "tls-extra" Newest []
    , debianize "transformers-base" Newest []
    , debianize "unicode-names" Newest [P.DebVersion "3.2.0.0-1~hackage1"]
    , debianize "unicode-properties" Newest
                    [ P.Patch . B.pack . unlines $
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
                          , " \timport Data.Map;" ]
                    , P.DebVersion "3.2.0.0-1~hackage1" ]
    , debianize "uniplate" Newest [P.DebVersion "1.6.5-1~hackage1"]
    -- , apt release "haskell-unix-compat"
    , debianize "unix-compat" Newest []
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
    , debianize "unordered-containers" Newest []
    , debianize "utf8-prelude" Newest [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt "sid" "haskell-utf8-string" Nothing
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize "unification-fd" Newest []
    , debianize "logict" Newest []
    , apt release "haskell-utility-ht"
    , debianize "vacuum" Newest [P.DebVersion "1.0.0.2-1~hackage1"]
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt release "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" Newest []
    , apt release "haskell-vector-algorithms"
    , debianize "virthualenv" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "                , file-embed >= 0.0.4.1 && < 0.1" ]
                    , P.DebVersion "0.2-1~hackage1" ]
    , debianize "wai" (Pin "1.0.0") []
    , debianize "vault" Newest []
    , debianize "web-encodings" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "                      Web.Encodings.MimeHeader," ]
                    ]
    , debianize "boomerang" Newest []
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Cd "web-routes" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-boomerang"
                , P.spec = Cd "web-routes-boomerang" (Darcs (repo ++ "/web-routes") Nothing)
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
    , apt release "haskell-xml"
    , debianize "xml-conduit" (Pin "0.5.1.2") []
    , debianize "http-conduit" (Pin "1.2.0") []
    , debianize "zlib-conduit" (Pin "0.0.1") []
    , debianize "conduit" (Pin "0.1.1.1") []
    , debianize "lifted-base" Newest []
    , debianize "system-filepath" Newest []
    , debianize "attoparsec-conduit" (Pin "0.0.1") []
    , debianize "blaze-builder-conduit" (Pin "0.0.1") []
    , debianize "xml-enumerator" Newest
                 [ P.Patch . B.pack . unlines $
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
                   , "                      Text.XML.Stream.Render" ] ]
    , debianize "xml-types" Newest []
    , debianize "xss-sanitize" Newest []
    , debianize "yaml-light" Newest []
    , apt release "haskell-zip-archive"
    , debianize "zlib-bindings" Newest []
    , apt release "haskell-zlib-enum"

    , apt release "highlighting-kate"
    , P.Package { P.name = "hscolour"
                , P.spec = Apt "sid" "hscolour" Nothing
                , P.flags = [P.RelaxDep "hscolour"] }
    , apt release "hslogger"
    , apt release "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery" Nothing)
                , P.flags = [] }
    , apt release "jquery-goodies"
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
    , apt release "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs" Nothing
                , P.flags = [] }
    -- , apt   "wordpress"
    ]

platform release =
    mconcat
    [ let ghc740 = P.Package { P.name = "ghc"
                             , P.spec = Apt "experimental" "ghc" Nothing
                             , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] }
          ghc741 = P.Package { P.name = "ghc"
                             , P.spec = Apt "sid" "ghc" Nothing
                             , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] } in
      case release of
        "natty-seereason" -> ghc741 -- P.NoPackage
        _ -> ghc741 -- P.NoPackage
    , -- Patch haskell-devscripts to generate the correct haddock
      -- dependency in the doc packages (haddock-interface-19 rather than
      -- just 19), and to remove the conflict with ghc 7.4 that Joachim
      -- added.
      apt release "haskell-devscripts"
{-  , P.Package { P.name = "haskell-deepseq"
                , P.spec = Apt "sid" "haskell-deepseq" (Just "1.1.0.2-2")
                , P.flags = [] } -}
    , -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happy/1.18.8/happy-1.18.8.tar.gz" "907d79425351e826fb0bb8b677620418")
                           (Darcs "http://src.seereason.com/happy-debian" Nothing),
                           P.flags = [P.RelaxDep "happy", P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    {- , debianize "happy" Newest [] -}
    , apt release "haskell-stm"
    , apt release "haskell-zlib"
    -- , apt "haskell-deepseq"
    , case release of
        "natty-seereason" -> P.NoPackage -- a version newer than the latest in hackage is bundled with ghc
        _ -> P.NoPackage -- debianize "deepseq" Newest []

    , P.Package { P.name = "haskell-mtl"
                , P.spec = Apt "sid" "haskell-mtl" (Just "2.0.1.0-2")
                , P.flags = [] }
    , P.Package { P.name = "haskell-transformers"
                , P.spec = Apt "sid" "haskell-transformers" (Just "0.2.2.0-3")
                , P.flags = [] }
    , debianize "parallel" Newest []
    , case release of
        "natty-seereason" -> P.NoPackage -- ghc 7.4 ships with a binary package not available in hackage
        _ -> P.Package { P.name = "haskell-binary"
                       , P.spec = Quilt (Apt "sid" "haskell-binary" (Just "0.5.0.2-2")) (Darcs "http://src.seereason.com/haskell-binary-quilt" Nothing)
                       , P.flags = [] }
    , debianize "syb" Newest []
    , debianize "fgl" Newest [P.DebVersion "5.4.2.4-1"]
    , debianize "text" Newest []
    , P.Package { P.name = "alex"
                , P.spec = Apt "sid" "alex" Nothing
                , P.flags = [P.RelaxDep "alex"] }
    -- , opengl release
    -- , haddock release
    , debianize "haskell-src" Newest [ P.ExtraDep "happy" ]
    , debianize "network" Newest []
    , debianize "HTTP" Newest [P.Epoch 1]
    , P.Package { P.name = "haskell-cgi"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian" Nothing)
                , P.flags = [] }
    -- This is bundled with the compiler
    -- , debianize "process" Newest []
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" Newest []
    , apt release "haskell-hunit"
    , debianize "QuickCheck" Newest [P.ExtraDep "libghc-random-prof"]
    , apt release "haskell-parsec"
    , apt release "haskell-html"
    , apt release "haskell-regex-compat"
    , apt release "haskell-regex-base"
    , apt release "haskell-regex-posix"
    , debianize "xhtml" Newest []
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
jsonb = mconcat $
    [ debianize "JSONb" Newest [P.DebVersion "1.0.7-1~hackage1"]
    , debianize "data-object-json" Newest [] ]

-- May work with these added dependencies (statevar thru openglraw)
opengl release = mconcat $
    [ debianize "OpenGL" Newest []
    , debianize "vacuum-opengl" Newest [{-P.DebVersion "0.0.3-1~hackage2"-}]
    , debianize "bitmap-opengl" Newest [{-P.DebVersion "0.0.0-1~hackage1"-}]
    , apt release "haskell-glut"
    , debianize "statevar" Newest []
    , debianize "tensor" Newest []
    , debianize "gluraw" Newest []
    , debianize "objectname" Newest []
    , debianize "openglraw" Newest [] ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib release = mconcat $
    [ debianize "glib" Newest [P.ExtraDep "haskell-gtk2hs-buildtools-utils", P.ExtraDep "libglib2.0-dev"]
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
plugins = mconcat $
    [ debianize "plugins" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "                 when (not (null ps')) $" ]
                    , P.DebVersion "1.5.1.4-1~hackage1" ]
    , debianize "happstack-plugins" Newest []
    , debianize "plugins-auto" Newest [] ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = mconcat
    [ debianize "unpack-funcs" Newest []
    , debianize "HiggsSet" Newest []
    , debianize "TrieMap" Newest [P.DebVersion "4.0.1-1~hackage1"] ]

-- ircbot needs a dependency on containers
happstackdotcom = mconcat
    [ P.Package { P.name = "haskell-ircbot"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/ircbot" Nothing
                , P.flags = []
                }
    , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/happstackDotCom" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc" Nothing
                , P.flags = [] } ]

frisby = mconcat
    [ P.Package { P.name = "haskell-frisby"
                , P.spec = DebDir (Cd "frisby" (Darcs "http://src.seereason.com/frisby" Nothing)) (Darcs "http://src.seereason.com/frisby-debian" Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-decimal"
                , P.spec = Darcs "http://src.seereason.com/decimal" Nothing
                , P.flags = [] } ]

haddock release =
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base ==
    -- 4.3.* so we can't use "debianize "haddock" []".  I don't think
    -- we really need this, or the hackage version.  Version 2.10.0 is
    -- included with ghc 7.4.0.
    [ apt release "haskell-haddock" ]

-- These have been failing for some time, and I don't think we've missed them.
failing release =
    [ debianize "funsat" Newest []
    , apt release "haskell-statistics" ]

algebra =
    [ debianize "adjunctions" Newest []
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
    , debianize "void" Newest [] ]

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
    , debianize "aeson-native" Newest
                    [ P.Patch . B.pack . unlines $
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
                      , "     old-locale," ] ]
    , apt release "haskell-binary-shared" -- for leksah
    , debianize "cairo" Newest [P.ExtraDep "haskell-gtk2hs-buildtools-utils"] -- for leksah
    , debianize "convertible-text" Newest
                    [ P.Patch . B.pack . unlines $
                      [ "--- old/convertible-text.cabal\t2012-01-20 09:58:49.087163068 -0800"
                      , "+++ new/convertible-text.cabal\t2012-01-20 09:57:30.017160919 -0800"
                      , "@@ -36,11 +36,11 @@"
                      , "   else"
                      , "       Buildable: True"
                      , "   Build-Depends: base >= 4 && < 5,"
                      , "-                 old-time >= 1.0.0.2 && < 1.1,"
                      , "+                 old-time >= 1.0.0.2,"
                      , "                  containers >= 0.2.0.1 && < 0.5,"
                      , "                  text >= 0.5 && < 0.12,"
                      , "                  bytestring >= 0.9.1.4 && < 0.10,"
                      , "-                 attempt >= 0.3.0 && < 0.4,"
                      , "+                 attempt >= 0.3.0,"
                      , "                  template-haskell,"
                      , "                  time"
                      , " " ] ]
    , debianize "gnuplot" Newest [P.DebVersion "0.4.2-1~hackage1"]
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
debianize :: String -> CabalVersion -> [P.PackageFlag] -> P.Packages
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
