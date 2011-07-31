{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Sid ( ring0, ring1 ) where

import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common
import Targets.Hackage (hackage, Flag(..))

-- |The _home parameter has an underscore because it is normally
-- unused, but can be used temporarily to generate a path using the
-- localRepo function.
sid _home name =
    Target { sourcePackageName = name
           , sourceSpec = getSourceSpec name
           , relaxInfo = getRelaxInfo name }
    where
      -- Special cases of non-empty relaxInfo lists
      getRelaxInfo "ghc" = ["ghc","happy","alex","xsltproc","debhelper","quilt"]
      getRelaxInfo "hscolour" = ["hscolour"]
      getRelaxInfo "happy" = ["happy"]
      getRelaxInfo "haskell-utf8-string" = ["hscolour", "cpphs"]
      getRelaxInfo "haskell-debian" = ["cabal-debian"]
      getRelaxInfo _ = []

      -- Special case sourceSpecs
      getSourceSpec "ghc" = "quilt:(apt:sid:ghc):(darcs:" ++ repo ++ "/ghc7-quilt)"
      getSourceSpec "haskell-bzlib" = "quilt:(apt:sid:haskell-bzlib):(darcs:http://src.seereason.com/haskell-bzlib-quilt)"
      getSourceSpec "haskell-json" = "quilt:(apt:sid:haskell-json=0.4.4-2):(darcs:" ++ repo ++ "/haskell-json-quilt)"
      getSourceSpec "haskell-uniplate" = "quilt:(apt:sid:haskell-uniplate):(darcs:http://src.seereason.com/haskell-uniplate-quilt)"
      -- Try removing the quilt when a revision newer than 0.5.0.2-2 appears in sid
      getSourceSpec "haskell-binary" = "quilt:(apt:sid:haskell-binary=0.5.0.2-2):(darcs:http://src.seereason.com/haskell-binary-quilt)"
      -- sid version is 0.2.2.1-1, too old
      getSourceSpec "haskell-wai" = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/wai/0.3.1/wai-0.3.1.tar.gz:5a777cf08713a55818955ec4d0748622):(darcs:http://src.seereason.com/haskell-wai-debian)"
      -- Version in sid is 3001.1.7.4-1, our version is 3001.1.8.2
      getSourceSpec "haskell-cgi" = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz:4092efaf00ac329b9771879f57a95323):(darcs:http://src.seereason.com/haskell-cgi-debian)"
      -- Sid version needs cdbs >= 0.4.70~, which I can't seem to build.
      -- , sourceSpec = "apt:sid:pandoc"
      getSourceSpec "jquery" = "proc:apt:sid:jquery"
      getSourceSpec "jqueryui" = "proc:apt:sid:jqueryui"
      -- We have to build pandoc from hackage because the sid version depends on a version of cdbs that
      -- can't be built for lucid.
      getSourceSpec "pandoc" = sourceSpec (hackage "pandoc" [Pin "1.5.1.1"])
      -- Add a dependency on libmagic-dev to libghc-magic-dev.  Next upstream release should have this fix.
      getSourceSpec "magic-haskell" = "quilt:(apt:sid:magic-haskell=1.0.8-7):(darcs:" ++ repo ++ "/magic-quilt)"
      -- The normal case
      getSourceSpec n = "apt:sid:" ++ n

{-
instance Eq Target where
    t1 == t2 = sourcePackageName t1 == sourcePackageName t2 &&
               sourceSpec t1 == sourceSpec t2 &&
               Set.fromList (relaxInfo t1) == Set.fromList (relaxInfo t2)
-}

ring0 _home =
    map (sid _home)
            [ "ghc"
            , "haskell-devscripts"
            , "haskell-dummy"
            , "hscolour" ]

ring1 _home =
    map (sid _home)
            [ "cpphs"
            , "haskell-transformers"
            , "haskell-mtl"
            , "happy"
            , "haskell-utf8-string"
            , "haskell-deepseq"
            , "haskell-text"
            , "haskell-hjavascript"
            , "haskell-hjscript"
            , "haskell-harp"
            , "darcs"
            , "agda"
            , "agda-bin"
            , "agda-stdlib"
            , "haskell-attoparsec-enumerator"
            , "haskell-attoparsec"
            , "haskell-base64-bytestring"
            , "haskell-blaze-builder"
            , "haskell-blaze-html"
            , "haskell-blaze-builder-enumerator"
            , "haskell-bytestring-nums"
            , "haskell-bzlib"
            , "haskell-cereal"
            , "haskell-citeproc-hs"
            , "haskell-colour"
            , "haskell-criterion"
            , "haskell-crypto"
            , "haskell-crypto-api"
            , "haskell-cryptocipher"
            , "haskell-cryptohash"
            , "haskell-curl"
            , "haskell-data-accessor"
            , "haskell-data-accessor-template"
            , "haskell-data-default"
            , "haskell-dataenc"
            , "haskell-debian"
            , "haskell-digest"
            , "haskell-diff"
            , "haskell-dlist"
            , "haskell-edison-api"
            , "haskell-edison-core"
            , "haskell-enumerator"
            , "haskell-erf"
            , "haskell-failure"
            , "haskell-feed"
            , "haskell-fgl"
            , "haskell-hashed-storage"
            , "haskell-haskeline"
            , "haskell-haskell-src"
            , "highlighting-kate"
            , "haskell-hsemail"
            , "hslogger"
            , "haskell-hstringtemplate"
            , "haskell-html"
            , "haskell-case-insensitive"
            , "haskell-cprng-aes"
            , "haskell-http-types"
            , "haskell-monad-control"
            , "haskell-base-unicode-symbols"
            , "haskell-zlib-enum"
            , "haskell-http"
            , "haskell-hunit"
            , "haskell-irc"
            , "haskell-json"
            , "haskell-largeword"
            , "haskell-leksah"
            , "haskell-leksah-server" -- for leksah
            , "haskell-strict" -- for leksah
            , "haskell-binary-shared" -- for leksah
            , "haskell-glib" -- for leksah
            , "haskell-gtk" -- for leksah
            , "haskell-gtksourceview2" -- for leksah
            , "haskell-ltk" -- for leksah
            , "gtk2hs-buildtools" -- for leksah
            , "haskell-cairo" -- for leksah
            , "haskell-pango" -- for leksah
            , "haskell-gio" -- for leksah
            , "haskell-haddock" -- for leksah
            , "haskell-ghc-paths" -- for leksah
            , "magic-haskell"
            , "haskell-maybet"
            , "haskell-mmap"
            , "haskell-monadcatchio-mtl"
            , "haskell-monoid-transformer"
            , "haskell-mwc-random"
            , "haskell-network"
            , "haskell-pandoc-types"
            , "haskell-parallel"
            , "haskell-parsec2"
            , "haskell-parsec"
            , "haskell-pcre-light"
            , "haskell-polyparse"
            , "haskell-primitive"
            , "haskell-quickcheck"
            , "haskell-quickcheck1"
            , "haskell-regex-base"
            , "haskell-regex-compat"
            , "haskell-regex-posix"
            , "haskell-regex-tdfa"
            , "haskell-safe"
            , "haskell-semigroups"
            , "haskell-sendfile"
            , "haskell-sha"
            , "haskell-smtpclient"
            , "haskell-split"
            , "haskell-stm"
            , "haskell-strict-concurrency"
            , "haskell-syb"
            , "haskell-syb-with-class"
            , "haskell-syb-with-class-instances-text"
            , "haskell-tagged"
            , "haskell-tagsoup"
            , "haskell-tar"
            , "haskell-terminfo"
            , "haskell-testpack"
            , "haskell-texmath"
            , "haskell-uniplate"
            , "haskell-unix-compat"
            , "haskell-utility-ht"
            , "haskell-vector"
            , "haskell-xhtml"
            , "haskell-xml"
            , "haskell-xss-sanitize"
            , "haskell-zip-archive"
            , "haskell-zlib-bindings"
            , "haskell-zlib"
            , "haxml"
            , "missingh"
            , "haskell-pcre-light"
            , "haskell-configfile"
            , "haskell-statistics"
            , "haskell-vector"
            , "haskell-vector-algorithms"
            , "haskell-opengl"
            , "haskell-glut"
            , "haskell-puremd5"
            , "haskell-binary"
            , "haskell-src-exts"
            , "haskell-hsx"
            , "haskell-hsp"
            , "haskell-tls"
            , "haskell-tls-extra"
            , "haskell-certificate"
            , "haskell-asn1-data"
            , "haskell-wai"
            , "haskell-cgi"
            -- , "cdbs"
            , "bash-completion"
            , "geneweb"
            , "html-xml-utils"
            , "jquery"
            , "jqueryui"
            , "pandoc"
            , "tinymce"
            , "wordpress"
            -- , "haskell-hsx-jmacro"
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
