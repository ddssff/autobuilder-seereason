{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Sid ( ring0, ring1 ) where

import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec
import Targets.Common

-- |The _home parameter has an underscore because it is normally
-- unused, but can be used temporarily to generate a path using the
-- localRepo function.
sid _home _release name =
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

ring0 _home release =
    [ ghc ] ++ map (sid _home release)  ["haskell-devscripts", "haskell-dummy", "hscolour"]
    where
      ghc =
          case release of
            "natty-seereason" ->
                P.Package { P.name = "ghc"
                          , P.spec = Apt "experimental" "ghc" Nothing
                          , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt"] }
            "lucid-seereason" -> sid _home release "ghc"
            _ -> error ("Unexpected release: " ++ show release)

commonSidPackages _home release =
    map (sid _home release)
            [ "haskell-deepseq"
            , "haskell-transformers"
            , "haskell-mtl"
            , "haskell-utf8-string"
            , "haskell-hjavascript"
            , "haskell-hjscript"
            , "haskell-harp"
            , "darcs"
            , "agda"
            , "agda-bin"
            , "agda-stdlib"
            , "haskell-attoparsec-enumerator"
            , "haskell-attoparsec-text"
            , "haskell-base64-bytestring"
            , "haskell-blaze-builder"
            , "haskell-blaze-builder-enumerator"
            , "haskell-bytestring-nums"
            , "haskell-bzlib"
            , "haskell-cereal"
            , "haskell-citeproc-hs"
            , "haskell-colour"
            , "haskell-criterion"
            , "haskell-crypto-api"
            , "haskell-entropy"
            , "haskell-cryptocipher"
            , "haskell-cryptohash"
            , "haskell-curl"
            , "haskell-data-accessor"
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
            , "highlighting-kate"
            , "haskell-hsemail"
            , "hslogger"
            , "haskell-hstringtemplate"
            , "haskell-html"
            , "haskell-zlib-enum"
            , "haskell-hunit"
            , "haskell-irc"
            , "haskell-largeword"
            , "haskell-leksah"
            , "haskell-leksah-server" -- for leksah
            , "haskell-strict" -- for leksah
            , "haskell-binary-shared" -- for leksah
            , "haskell-gtk" -- for leksah
            , "haskell-gtksourceview2" -- for leksah
            , "haskell-ltk" -- for leksah
            , "haskell-cairo" -- for leksah
            , "haskell-pango" -- for leksah
            , "haskell-gio" -- for leksah
            , "haskell-ghc-paths" -- for leksah
            , "magic-haskell"
            , "haskell-maybet"
            , "haskell-mmap"
            , "haskell-monadcatchio-mtl"
            , "haskell-monoid-transformer"
            , "haskell-mwc-random"
            , "haskell-pandoc-types"
            , "haskell-parallel"
            , "haskell-parsec2"
            , "haskell-parsec"
            , "haskell-pcre-light"
            , "haskell-primitive"
            , "haskell-quickcheck1"
            , "haskell-regex-base"
            , "haskell-regex-compat"
            , "haskell-regex-posix"
            , "haskell-regex-tdfa"
            , "haskell-safe"
            , "haskell-sendfile"
            , "haskell-sha"
            , "haskell-stm"
            , "haskell-strict-concurrency"
            , "haskell-syb-with-class-instances-text"
            -- , "haskell-tagged"
            -- , "haskell-tagsoup" -- Moved to Hackage
            , "haskell-tar"
            , "haskell-terminfo"
            , "haskell-testpack"
            , "haskell-texmath"
            , "haskell-unix-compat"
            , "haskell-utility-ht"
            -- Requires devscripts 0.8.9, restore when that gets built
            -- , "haskell-vector"
            , "haskell-xhtml"
            , "haskell-xml"
            , "haskell-xss-sanitize"
            , "haskell-zip-archive"
            , "haskell-zlib-bindings"
            , "haskell-zlib"
            , "haskell-pcre-light"
            , "haskell-configfile"
            , "haskell-statistics"
            , "haskell-vector-algorithms"
            , "haskell-opengl"
            , "haskell-glut"
            , "haskell-puremd5"
            , "haskell-binary"
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
            , "tinymce"
            , "wordpress"
            -- , "haskell-hsx-jmacro"
            , "haskell-json"
            ]

releaseSidPackages _home _release@"natty-seereason" =
    [ P.Package { P.name = "cpphs"
                , P.spec = Quilt (Apt "sid" "cpphs" Nothing) (Darcs "http://src.seereason.com/cpphs-quilt" Nothing)
                , P.flags = [] }
    , P.Package { P.name = "happy"
                , P.spec = Quilt (Apt "sid" "happy" Nothing) (Darcs (repo ++ "/happy-quilt") Nothing)
                , P.flags = [ P.RelaxDep "happy" ] } ] ++
    -- When a version newer than 0.12.0 comes out this should be
    -- switched to a debianize target.
     map (sid _home _release)
         [ "haskell-glib"
         , "pandoc" ]-- for leksah

releaseSidPackages _home _release@"lucid-seereason" =
    map (sid _home _release)
        [ "cpphs"
        , "haskell-text"
        , "haskell-http"
        , "gtk2hs-buildtools" -- for leksah
        , "haskell-network"
        , "haskell-quickcheck"
        , "haskell-syb"
        , "haskell-syb-with-class"
        , "haskell-src-exts"
        , "haskell-tagged"
        , "haskell-semigroups"
        , "haskell-polyparse"
        , "haskell-haskeline"
        , "haskell-glib" -- for leksah
        , "haskell-uniplate"
        , "haskell-blaze-html"
        , "haskell-crypto"
        , "missingh"
        , "haskell-attoparsec"
        , "haskell-dataenc"
        , "haskell-fgl"
        , "haskell-case-insensitive"
        , "haskell-haskell-src"
        , "haskell-base-unicode-symbols"
        , "haskell-haddock" -- for leksah
        , "haskell-data-accessor-template"
        , "haskell-cprng-aes"
        -- , "haskell-split"
        -- , "haskell-http-types"
        , "haskell-smtpclient"
        , "haskell-data-accessor-template"
        -- Current version needs a BangPatterns options
        , "haskell-hashed-storage"
        ] ++
    [ P.Package { P.name = "happy"
                , P.spec = Apt "sid" "happy" Nothing
                , P.flags = [ P.RelaxDep "happy" ] } ]

releaseSidPackages _ release = error ("Unexpected release: " ++ show release)

ring1 _home release = commonSidPackages _home release ++ releaseSidPackages _home release

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
