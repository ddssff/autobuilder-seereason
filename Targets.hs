module Targets where

import Data.List (isPrefixOf)
import Debian.AutoBuilder.ParamClass (Target(..))

------------------------ TARGETS ---------------------

publicTargets release =
    ghc610CoreTargets release ++	-- The compiler and our packaging tools
    autobuilderTargets release ++	-- The autobuilder and its dependencies
    ghc610Targets release ++		-- Haskell targets
    otherTargets release		-- Non-haskell targets

-- useGHC6102 release = True
ghcRelease = "6.10.3"

useGHC6102 = (/= "6.10.1")		-- Obsolete, do comparisons with ghcRelease instead

-- This module defines how we obtain and assemble the source code for
-- the packages we want to build.

ghc610CoreTargets release =
    [ -- GHC 6.10.4 Provides:
      --           ghc-prof,
      --           libghc6-array-prof,
      --           libghc6-base-prof,
      --           libghc6-bytestring-prof,
      --           libghc6-cabal-prof,
      --           libghc6-containers-prof,
      --           libghc6-directory-prof,
      --           libghc6-extensible-exceptions-prof,
      --           libghc6-filepath-prof,
      --           libghc6-ghc-prim-prof,
      --           libghc6-haskell98-prof,
      --           libghc6-hpc-prof,
      --           libghc6-integer-prof,
      --           libghc6-old-locale-prof,
      --           libghc6-old-time-prof,
      --           libghc6-packedstring-prof,
      --           libghc6-pretty-prof,
      --           libghc6-process-prof,
      --           libghc6-random-prof,
      --           libghc6-rts-prof,
      --           libghc6-syb-prof,
      --           libghc6-template-haskell-prof,
      --           libghc6-unix-prof
      Target { sourcePackageName = "ghc6"
             -- Use this spec to build a bootstrap version of ghc6
             -- using an older haddock.  You should next build
             -- haddock, and then comment this quilt target out and
             -- uncomment the regular ghc6 target, and do a full
             -- build.  Not sure if its ok to rebuild haddock after
             -- that.  
             -- , sourceSpec = "quilt:(apt:sid:ghc6):(darcs:http://src.seereason.com/ghc6103/ghc6-bootstrap-quilt)"
             , sourceSpec = aptSidOrKarmic "ghc6"
             , relaxInfo = ["ghc6"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"debhelper"
                           ,"quilt"] }
{-
      -- This ghc6 supplies haddock, so comment out the target below to use it.
      Target { sourcePackageName = "ghc6"
             , sourceSpec = "deb-dir:(uri:http://darcs.haskell.org/~ghc/dist/6.12.1rc1/ghc-6.12.0.20091010-src.tar.bz2:5ca685d5fc1c1d6924656a092f4d9b34):(darcs:http://src.seereason.com/ghc612-debian)"
             , relaxInfo = ["ghc6"
                           ,"happy"
                           ,"alex"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"debhelper"
                           ,"quilt"] }
-}
    , Target { sourcePackageName = "haddock"
             , sourceSpec = "apt:sid:haddock"
             , relaxInfo = ["happy", "ghc6", "debhelper"] }
    , Target { sourcePackageName = "hscolour"
             , sourceSpec = "apt:sid:hscolour"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happy/1.18.2/happy-1.18.2.tar.gz:adb1679a1fa8cec74a6e621a4a277e98):(darcs:http://src.seereason.com/ghc6103/happy-debian)"
             , relaxInfo = ["happy"] }
    , Target { sourcePackageName = "haskell-utf8-string"
             , sourceSpec = aptSidOrKarmic "haskell-utf8-string"
             , relaxInfo = [] }
    -- The normal haskell-http package requires cdbs >> 0.4.58, which
    -- is only in sid.  CDBS fails its unit tests when build on older
    -- dists.  So there's no point in having a target for this, if its
    -- present we'll use it, if not we can't.
{-  , Target { sourcePackageName = "cdbs"
             , sourceSpec = "apt:sid:cdbs"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-time"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/time/1.1.4/time-1.1.4.tar.gz:b6768582908be4fc31570462dce2eee9):(darcs:http://src.seereason.com/ghc6103/haskell-time-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zlib"
             , sourceSpec = "apt:sid:haskell-zlib"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hpdf"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HPDF/1.4.2/HPDF-1.4.2.tar.gz:ba364b6e397413931b65a56e63b19bb4):(darcs:http://src.seereason.com/ghc6103/haskell-hpdf-debian)"
             , relaxInfo = [] }
{- currently fails to build
    , Target { sourcePackageName = "haskell-pcre-light"
             , sourceSpec = "apt:sid:haskell-pcre-light"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-configfile"
             , sourceSpec = "apt:sid:haskell-configfile"
             , relaxInfo = [] }
-}

    , Target { sourcePackageName = "haskell-bzlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/ghc6103/haskell-bzlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskell-src"
             , sourceSpec = aptSidOrKarmic "haskell-haskell-src"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html"
             , sourceSpec = aptSidOrKarmic "haskell-html"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hunit"
             , sourceSpec = aptSidOrKarmic "haskell-hunit"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mtl"
             , sourceSpec = aptSidOrKarmic "haskell-mtl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-network"
             , sourceSpec = "apt:sid:haskell-network"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parallel"
             , sourceSpec = "apt:sid:haskell-parallel"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parsec"
             , sourceSpec = aptSidOrKarmic "haskell-parsec"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parsec2"
             , sourceSpec = "apt:sid:haskell-parsec2"
             , relaxInfo = [] }
    -- Binary packages: libghc6-quickcheck2-dev, libghc6-quickcheck2-prof, libghc6-quickcheck2-doc
    , Target { sourcePackageName = "haskell-quickcheck"
             , sourceSpec = aptSidOrKarmic "haskell-quickcheck"
             , relaxInfo = [] }
    -- Binary packages: libghc6-quickcheck1-dev, libghc6-quickcheck1-prof, libghc6-quickcheck1-doc
    , Target { sourcePackageName = "haskell-quickcheck1"
             , sourceSpec = aptSidOrKarmic "haskell-quickcheck1"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-base"
             , sourceSpec = "apt:sid:haskell-regex-base"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-compat"
             , sourceSpec = "apt:sid:haskell-regex-compat"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-posix"
             , sourceSpec = "apt:sid:haskell-regex-posix"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stm"
             , sourceSpec = aptSidOrKarmic "haskell-stm"
             , relaxInfo = [] }
    -- Patch to add the bareAttr function and remove the custom show
    -- instance Specify a particular version of xhtml so we can
    -- perhaps eliminate this the next time the upstream package is
    -- revved.
    , Target { sourcePackageName = "haskell-xhtml"
             , sourceSpec = "quilt:(apt:sid:haskell-xhtml=3000.2.0.1-5):(darcs:http://src.seereason.com/ghc6103/haskell-xhtml-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "html-xml-utils"
           , sourceSpec = "apt:sid:html-xml-utils"
           , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-devscripts"
             -- Patch to install executables into a similarly named deb.
             , sourceSpec = "quilt:(apt:sid:haskell-devscripts):(darcs:http://src.seereason.com/ghc6103/haskell-devscripts-quilt)"
             , relaxInfo = ["hscolour"] }
    , Target { sourcePackageName = "haskell-debian"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "darcs:http://src.seereason.com/ghc610/haskell-debian-3"
                              "6.10.2" -> "darcs:http://src.seereason.com/ghc6102/haskell-debian-3"
                              _ -> "darcs:http://src.seereason.com/ghc6103/haskell-debian-3"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "darcs:http://src.seereason.com/haskell-debian-repo"
                              "6.10.2" -> "darcs:http://src.seereason.com/ghc6102/haskell-debian-repo"
                              _ -> "darcs:http://src.seereason.com/ghc6103/haskell-debian-repo"
             , relaxInfo = [] }
    -- Known dependants in public:
    -- [haskell-happstack-extra, haskell-happstack-facebook, autobuilder, haskell-archive, haskell-debian,
    --  haskell-debian-repo, build-env, haskell-debian-mirror, haskell-help]
    , Target { sourcePackageName = "haskell-extra"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/haskell-extra"
             , relaxInfo = ["cabal-debian"] }
    -- Known dependants in public:
    -- [haskell-happstack-extra, haskell-happstack-facebook, autobuilder, haskell-archive, haskell-debian,
    --  haskell-debian-repo, build-env, haskell-debian-mirror, haskell-extra, haskell-help]
    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cpphs"
             , sourceSpec = "apt:sid:cpphs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-tagsoup"
             , sourceSpec = aptSidOrKarmic "haskell-tagsoup"
             , relaxInfo = []
             }
--     , Target { sourcePackageName = "haxml"
--              , sourceSpec = "quilt:(apt:sid:haxml):(darcs:http://src.seereason.com/ghc6103/haxml-quilt)"
--              , relaxInfo = [] }
    ]
        where aptSidOrKarmic name = "apt:" ++ (if isPrefixOf "karmic-" release then "karmic" else "sid") ++ ":" ++ name

{-
ghc610CoreTargets release =
    [ Target { sourcePackageName = "haskell-bzlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/ghc610/debian/haskell-bzlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zlib/0.5.0.0/zlib-0.5.0.0.tar.gz:22fa6d394c42c8584b234799b923f860):(darcs:http://src.seereason.com/ghc610/debian/haskell-zlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cdbs"
             , sourceSpec = if useStandardDevscripts release
                            then "darcs:http://src.seereason.com/haskell-cdbs-dummy"
                            else "darcs:http://src.seereason.com/haskell-cdbs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cpphs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cpphs/1.6/cpphs-1.6.tar.gz:8a7565ff3b2d7bdb594af4c10c594951):(darcs:http://src.seereason.com/ghc610/debian/cpphs-debian)"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haxml"
             , sourceSpec = "quilt:(apt:sid:haxml):(darcs:http://src.seereason.com/ghc610/quilt/haxml-quilt)"
             , relaxInfo = [] }
{-
-}
    , Target { sourcePackageName = "haskell-debian"
             , sourceSpec = if useGHC6102 release
                            then "darcs:http://src.seereason.com/ghc6102/haskell-debian-3"
                            else "darcs:http://src.seereason.com/ghc610/haskell-debian-3"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = if useGHC6102 release
                            then "darcs:http://src.seereason.com/ghc6102/haskell-debian-repo"
                            else "darcs:http://src.seereason.com/haskell-debian-repo"
             , relaxInfo = [] }
    , Target { sourcePackageName = "html-xml-utils"
           , sourceSpec = "apt:sid:html-xml-utils"
           , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-devscripts"
             , sourceSpec = if useStandardDevscripts release
                            then "quilt:(apt:sid:haskell-devscripts):(darcs:http://src.seereason.com/ghc6102/haskell-devscripts-quilt)"
                            else "quilt:(apt:sid:haskell-devscripts):(darcs:http://src.seereason.com/ghc6102/haskell-devscripts-quilt)"
                            -- "quilt:(uri:http://ftp.de.debian.org/debian/pool/main/h/haskell-devscripts/haskell-devscripts_0.6.15.tar.gz:996acac2c6fb2da2be9c5016f93a3c67):(darcs:http://src.seereason.com/ghc610/quilt/haskell-devscripts-quilt)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "ghc6"
             , sourceSpec = if useGHC6102 release
                            then "deb-dir:(uri:http://www.haskell.org/ghc/dist/6.10.2/ghc-6.10.2-src.tar.bz2:243d5857e5aa5f2f86e5e4c4437973fb):(darcs:http://src.seereason.com/debian/ghc6102-debian)"
                            else "deb-dir:(uri:http://www.haskell.org/ghc/dist/6.10.1/ghc-6.10.1-src.tar.bz2:54c676a632b3d73cf526b06347522c32):(darcs:http://src.seereason.com/ghc610/debian/ghc610-debian)"
             , relaxInfo = ["ghc6"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"haddock"] }
{-
    -- Unpatched haskell-devscripts.  Using this leads to build
    -- failures because the postinst and postrm scripts don't pass the
    -- --global-conf argument to ghc-pkg.
    , Target { sourcePackageName = "haskell-devscripts"
             , sourceSpec = "apt:sid:haskell-devscripts"
             , relaxInfo = [] }
-}
    ] ++
    if useGHC6102 release
    then [ -- The haskell-time package is no longer included in the 6.10.2 compiler
           Target { sourcePackageName = "haskell-time"
                  , sourceSpec = "apt:sid:haskell-time"
                  , relaxInfo = [] }
         , Target { sourcePackageName = "haskell-extra"
                  , sourceSpec = "darcs:http://src.seereason.com/ghc6102/haskell-extra"
                  , relaxInfo = ["cabal-debian"] } ]
    else [ Target { sourcePackageName = "haskell-extra"
                  , sourceSpec = "darcs:http://src.seereason.com/ghc610/haskell-extra"
                  , relaxInfo = ["cabal-debian"] } ]
-}

autobuilderTargets release =
    [ Target { sourcePackageName = "build-env"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/build-env"
             , relaxInfo = [] }
    , Target { sourcePackageName = "autobuilder"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "darcs:http://src.seereason.com/ghc610/autobuilder"
                              "6.10.2" -> "darcs:http://src.seereason.com/ghc6102/autobuilder"
                              _ -> "darcs:http://src.seereason.com/ghc6103/autobuilder"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cgi"
             , sourceSpec = aptSidOrKarmic "haskell-cgi"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mime"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "darcs:http://src.seereason.com/ghc610/haskell-mime"
                              "6.10.2" -> "darcs:http://src.seereason.com/ghc6102/haskell-mime"
                              _ -> "darcs:http://src.seereason.com/ghc6103/haskell-mime"
             , relaxInfo = [] }
{-
    , Target { sourcePackageName = "magic-haskell"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/ghc610/quilt/magic-haskell-quilt)"
                              "6.10.2" -> "quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/ghc610/quilt/magic-haskell-quilt)"
                              _ -> "apt:sid:magic-haskell"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "haskell-magic"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/magic/1.0.8/magic-1.0.8.tar.gz:e81c493fe185431a5b70d4855ed4b87f):(darcs:http://src.seereason.com/magic-debian)"
             , relaxInfo = [] }
    ]
         where aptSidOrKarmic name = "apt:" ++ (if isPrefixOf "karmic-" release then "karmic" else "sid") ++ ":" ++ name

-- Note that some of the debian source packages have names that don't begin with haskell-:
--   pandoc, magic-haskell, hslogger
ghc610Targets release =
    [ Target { sourcePackageName = "haskell-http"
             , sourceSpec = "quilt:(apt:sid:haskell-http):(darcs:http://src.seereason.com/ghc6103/haskell-http-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.5/applicative-extras-0.1.5.tar.gz:b5a6288629133529c0951d5b71a21954):(darcs:http://src.seereason.com/ghc6103/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-binary"
             , sourceSpec = aptSidOrKarmic "haskell-binary"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-src-exts"
             , sourceSpec = "apt:sid:haskell-src-exts"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-maybet"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/MaybeT/0.1.2/MaybeT-0.1.2.tar.gz:9864a3f34151217004f8c968fda5b427):(darcs:http://src.seereason.com/ghc6103/MaybeT-debian)"
             , relaxInfo = [] }               
    , Target { sourcePackageName = "haskell-happstack-util"
             , sourceSpec = "cd:happstack-util:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             , sourceSpec = "cd:happstack-data:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-ixset"
             , sourceSpec = "cd:happstack-ixset:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-server"
             , sourceSpec = "cd:happstack-server:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-state"
             , sourceSpec = "cd:happstack-state:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack"
             , sourceSpec = "cd:happstack:darcs:http://src.seereason.com/ghc6103/happstack"
             , relaxInfo = [] }
    -- Depends on pandoc
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html-entities"
             , sourceSpec = "darcs:http://src.seereason.com/html-entities"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pandoc"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pandoc/1.2/pandoc-1.2.tar.gz:402999cf17dd7072e4c8c7b6b6050ec3):(darcs:http://src.seereason.com/ghc6103/haskell-pandoc-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-facebook"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/happstack-facebook"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/ghc6103/hinotify-debian)"
             , relaxInfo = [] }
    -- Required by happstack-state
    , Target { sourcePackageName = "haskell-hspread"
             , sourceSpec = "quilt:(apt:lenny:haskell-hspread):(darcs:http://src.seereason.com/ghc6103/haskell-hspread-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/ghc6103/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-json"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/json/0.4.3/json-0.4.3.tar.gz:1af33c67594f69048b69d4aeafeea03e):(darcs:http://src.seereason.com/ghc6103/json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/iconv"
             , relaxInfo = [] }

--  The Sid package has no profiling libraries, so dependent packages
--  won't build.  Use our debianization instead.
{-  , Target { sourcePackageName = "hslogger"
             , sourceSpec = "apt:sid:hslogger"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-hslogger"
             , sourceSpec = case ghcRelease of
                              "6.10.1" -> "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc610/debian/hslogger-debian)"
                              "6.10.2" -> "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc6102/debian/hslogger-debian)"
                              _ -> "deb-dir:(uri:http://ftp.de.debian.org/debian/pool/main/h/hslogger/hslogger_1.0.8.orig.tar.gz:5ce575887d2b6076793f8736b077626a):(darcs:http://src.seereason.com/ghc6103/hslogger-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-harp"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/harp):(darcs:http://src.seereason.com/ghc6103/harp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjavascript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjavascript):(darcs:http://src.seereason.com/ghc6103/hjavascript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/hsx"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsp"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/ghc610/hsp):(darcs:http://src.seereason.com/ghc6103/hsp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/happs-hsp-formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hsx-xhtml):(darcs:http://src.seereason.com/ghc6103/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjscript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjscript):(darcs:http://src.seereason.com/ghc6103/hjscript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/ghc6103/shellac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-frisby"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/frisby"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    -- Requires Cabal >= 1.7.3, ghc 6.10.3 comes with Cabal-1.6.0.3
{-  , Target { sourcePackageName = "haskell-cabal-install"
             , sourceSpec = "deb-dir:(darcs:http://darcs.haskell.org/cabal-install):(darcs:http://src.seereason.com/ghc6103/cabal-install-debian)"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-uniplate"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/uniplate/1.2.0.3/uniplate-1.2.0.3.tar.gz:e0e10700870f5b9756d4097e640164ca):(darcs:http://src.seereason.com/ghc6103/uniplate-debian)"
             , sourceSpec = aptSidOrKarmic "haskell-uniplate"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/ghc6103/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/ghc6103/stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/ghc6103/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delconto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/ghc6103/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/archive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-orphanage"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/haskell-orphanage"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hstringtemplate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HStringTemplate/0.4.3/HStringTemplate-0.4.3.tar.gz:57139f6695f1c268ed38c34336191636):(darcs:http://src.seereason.com/ghc6103/HStringTemplate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-consumer"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/haskell-consumer"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-urlt"
             , sourceSpec = "darcs:http://src.seereason.com/ghc6103/urlt"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-xml"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/xml/1.3.4/xml-1.3.4.tar.gz:841c3a36a0bfb2e46f88448ecc227cad):(darcs:http://src.seereason.com/debian/haskell-xml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-feed"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/feed/0.3.6/feed-0.3.6.tar.gz:0abeeaa7870ef241ccdc5785e608a599):(darcs:http://src.seereason.com/debian/haskell-feed-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digest"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digest/0.0.0.5/digest-0.0.0.5.tar.gz:ba60cc9d1ad6d0795ad84390976699d1):(darcs:http://src.seereason.com/debian/haskell-digest-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zip-archive"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zip-archive/0.1.1.3/zip-archive-0.1.1.3.tar.gz:edf3924c929b5592b1b9dbf4853b754f):(darcs:http://src.seereason.com/ghc6103/haskell-zip-archive-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-utility-ht"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/ghc610/debian/haskell-utility-ht)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gnuplot"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gnuplot/0.3/gnuplot-0.3.tar.gz:4432a0e0b44ca7ceae8e6737cf0258fa):(darcs:http://src.seereason.com/ghc610/debian/haskell-gnuplot)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class"
             , sourceSpec = "quilt:(apt:sid:haskell-syb-with-class):(darcs:http://src.seereason.com/ghc6103/haskell-syb-with-class-quilt)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-hsemail"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsemail/1.3/hsemail-1.3.tar.gz:e1b84af260aa36c017c190ce481e2c1a):(darcs:http://src.seereason.com/debian/haskell-hsemail-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-smtpclient"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/SMTPClient/1.0.1/SMTPClient-1.0.1.tar.gz:c6e02189636b608e27942dbb9af9732a):(darcs:http://src.seereason.com/debian/haskell-smtpclient-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-strict-concurrency"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/strict-concurrency/0.2.1/strict-concurrency-0.2.1.tar.gz:974cda4fa4d4a2d9082f31160f57f707):(darcs:http://src.seereason.com/debian/haskell-strict-concurrency-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-unix-compat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/unix-compat/0.1.2.1/unix-compat-0.1.2.1.tar.gz:6ecfc3922fce2e96922af3a636b061f9):(darcs:http://src.seereason.com/debian/haskell-unix-compat-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-sendfile"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/sendfile/0.5/sendfile-0.5.tar.gz:649434b84c3c716840861b85c6b0bb05):(darcs:http://src.seereason.com/debian/haskell-sendfile-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-text"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/text/0.5/text-0.5.tar.gz:f506b0866aa9b476a35e0960870cc4d1):(darcs:http://src.seereason.com/haskell-text-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-benchpress"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "darcs"
             , sourceSpec = aptSidOrKarmic "darcs"
             , relaxInfo = [] }
    -- Required by the darcs 2.3.0-3 in sid.
    , Target { sourcePackageName = "bash-completion"
             , sourceSpec = aptSidOrKarmic "bash-completion"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskeline"
             , sourceSpec = aptSidOrKarmic "haskell-haskeline"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mmap"
             , sourceSpec = "apt:sid:haskell-mmap"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hashed-storage"
             , sourceSpec = "quilt:(apt:sid:haskell-hashed-storage):(darcs:http://src.seereason.com/hashed-storage-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-terminfo"
             , sourceSpec = aptSidOrKarmic "haskell-terminfo"
             , relaxInfo = [] }
    -- Required for darcs 2.3.0
    , Target { sourcePackageName = "curl"
             , sourceSpec = aptSidOrKarmic "curl"
             , relaxInfo = [] }
    -- We need debhelper >= 7.0.50 for darcs 2.3.0
    , Target { sourcePackageName = "debhelper"
             , sourceSpec = aptSidOrKarmic "debhelper"
             , relaxInfo = [] }
    -- dh --with quilt uses Debian/Debhelper/Sequence/quilt.pm
{-
    -- Problem fixed in 0.48-2?
    , Target { sourcePackageName = "quilt"
             , sourceSpec = "proc:quilt:(apt:sid:quilt):(darcs:http://src.seereason.com/quilt-quilt)"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "haskell-permutation"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/permutation/0.4.1/permutation-0.4.1.tar.gz:a9e0b6231d7a085719188406f59ab1aa):(darcs:http://src.seereason.com/haskell-permutation)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "gtk2hs"
             , sourceSpec = "apt:sid:gtk2hs"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-data-accessor"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor/0.2.1/data-accessor-0.2.1.tar.gz:a03da4e9f70a188a421eb2b7144bcaf7):(darcs:http://src.seereason.com/ghc6103/haskell-data-accessor-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-data-accessor-template"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor-template/0.2.1.2/data-accessor-template-0.2.1.2.tar.gz:0feb98757d889bbc9fbbe0379cdb3c05):(darcs:http://src.seereason.com/ghc6103/haskell-data-accessor-template-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-erf"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/erf/1.0.0.0/erf-1.0.0.0.tar.gz:1bdb56838565abfaa7d7ab6e2870ddaa):(darcs:http://src.seereason.com/ghc6103/haskell-erf-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-criterion"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/criterion/0.1.2/criterion-0.1.2.tar.gz:0e4d1c2f546ab650e03c610034c20226):(darcs:http://src.seereason.com/ghc6103/haskell-criterion-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-colour"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/colour/2.3.1/colour-2.3.1.tar.gz:5edced36d4c27393ae1ce1389eeb25ad):(darcs:http://src.seereason.com/ghc6103/haskell-colour-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-chart"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.11/Chart-0.11.tar.gz:b7f67defe06694eef580542947106fc0):(darcs:http://src.seereason.com/ghc6103/haskell-chart-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-statistics"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/statistics/0.3.5/statistics-0.3.5.tar.gz:b351bee9514e26555f170676b3c66139):(darcs:http://src.seereason.com/ghc6103/haskell-statistics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-uvector"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.0.4/uvector-0.1.0.4.tar.gz:8ec55bdff3da6f9658b78878cbaddfbe):(darcs:http://src.seereason.com/ghc6103/haskell-uvector-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-uvector-algorithms"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/ghc6103/haskell-uvector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-transformers"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/transformers/0.1.4.0/transformers-0.1.4.0.tar.gz:6edd0f22594c477b05fd059fdac2c5a9):(darcs:http://src.seereason.com/ghc6103/haskell-transformers-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-utility-ht"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/ghc6103/haskell-utility-ht-debian)"
             , relaxInfo = [] }
    ]
        where aptSidOrKarmic name = "apt:" ++ (if isPrefixOf "karmic-" release then "karmic" else "sid") ++ ":" ++ name

{-
ghc610Targets release =
    [ Target { sourcePackageName = "haskell-utils"
             , sourceSpec = "quilt:(apt:sid:haskell-utils):(darcs:http://src.seereason.com/ghc610/quilt/haskell-utils-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.3/applicative-extras-0.1.3.tar.gz:50fa4c61e89654ea9858c304b4682680):(darcs:http://src.seereason.com/ghc610/debian/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "darcs:http://src.seereason.com/formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-binary"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/binary/0.4.4/binary-0.4.4.tar.gz:48fc6454e82e0aec7f648be107bfc0b8):(darcs:http://src.seereason.com/ghc610/debian/binary-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-extensible-exceptions"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/extensible-exceptions/0.1.1.0/extensible-exceptions-0.1.1.0.tar.gz:7aba82acc64fa2f2dc89d8ac27e24a43):(darcs:http://src.seereason.com/ghc610/debian/extensible-exceptions-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-maybet"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/MaybeT/0.1.2/MaybeT-0.1.2.tar.gz:9864a3f34151217004f8c968fda5b427):(darcs:http://src.seereason.com/debian/MaybeT-debian)"
             , relaxInfo = [] }               
    , Target { sourcePackageName = "haskell-happstack-util"
             , sourceSpec = "cd:happstack-util:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             , sourceSpec = "cd:happstack-data:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-ixset"
             , sourceSpec = "cd:happstack-ixset:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-server"
             , sourceSpec = "cd:happstack-server:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-state"
             , sourceSpec = "cd:happstack-state:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack"
             , sourceSpec = "cd:happstack:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-contrib"
             , sourceSpec = "cd:happstack-contrib:darcs:http://src.seereason.com/happstack"
             , relaxInfo = [] }
    -- Depends on pandoc
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-facebook"
             , sourceSpec = case ghcRelease of
                              "6.10.2" -> "darcs:http://src.seereason.com/happstack-facebook"
                              _ -> "darcs:http://src.seereason.com/ghc6103/happstack-facebook"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/ghc610/debian/hinotify-debian)"
             , relaxInfo = [] }
{-
    , Target { sourcePackageName = "haskell-hspread"
             , sourceSpec = "quilt:(apt:sid:haskell-hspread):(darcs:http://src.seereason.com/ghc610/quilt/haskell-hspread-quilt)"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "haskell-utf8-string"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utf8-string/0.3.4/utf8-string-0.3.4.tar.gz:72d13d9453cdf721dd95bc18144a120a):(darcs:http://src.seereason.com/ghc610/debian/utf8-string-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happy/1.18.2/happy-1.18.2.tar.gz:adb1679a1fa8cec74a6e621a4a277e98):(darcs:http://src.seereason.com/ghc610/debian/happy-debian)"
             , relaxInfo = ["happy"] }
    , Target { sourcePackageName = "haskell-src-exts"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/haskell-src-exts/0.4.3.1/haskell-src-exts-0.4.3.1.tar.gz:4ff97fdae2bca0da0194fcb80974b188):(darcs:http://src.seereason.com/ghc610/debian/haskell-src-exts-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/ghc610/debian/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-json"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/json/0.4.3/json-0.4.3.tar.gz:1af33c67594f69048b69d4aeafeea03e):(darcs:http://src.seereason.com/debian/json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/iconv"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hslogger"
             , sourceSpec = if useGHC6102 release
                            then "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc6102/debian/hslogger-debian)"
                            else "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.7/hslogger-1.0.7.tar.gz:74ff79b2abfec7e24b96925f06112c9f):(darcs:http://src.seereason.com/ghc610/debian/hslogger-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HTTP/4000.0.4/HTTP-4000.0.4.tar.gz:6526c1ee59cd3aedc7aa380673c80ef1):(darcs:http://src.seereason.com/ghc610/debian/haskell-http-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/syb-with-class"
             , relaxInfo = [] }
{-
    , Target { sourcePackageName = "happs-util"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Util"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-data"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Data"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-ixset"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-IxSet"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-state"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-State"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happs-server"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Server"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happs-extra"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/HAppS-Extra"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "haskell-harp"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/harp):(darcs:http://src.seereason.com/ghc610/debian/harp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjavascript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjavascript):(darcs:http://src.seereason.com/ghc610/debian/hjavascript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/hsx"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsp"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/ghc610/hsp):(darcs:http://src.seereason.com/ghc610/debian/hsp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/happs-hsp-formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hsx-xhtml):(darcs:http://src.seereason.com/ghc610/debian/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjscript"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hjscript):(darcs:http://src.seereason.com/ghc610/debian/hjscript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/ghc610/debian/shellac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-frisby"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/frisby"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/ghc610/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cabal-install"
             , sourceSpec = "deb-dir:(darcs:http://darcs.haskell.org/cabal-install):(darcs:http://src.seereason.com/ghc610/debian/cabal-install-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uniplate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/uniplate/1.2.0.3/uniplate-1.2.0.3.tar.gz:e0e10700870f5b9756d4097e640164ca):(darcs:http://src.seereason.com/ghc610/debian/uniplate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/ghc610/debian/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/ghc610/debian/stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/ghc610/debian/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delconto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/ghc610/debian/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = if useGHC6102 release
                            then "darcs:http://src.seereason.com/ghc6102/mirror"
                            else "darcs:http://src.seereason.com/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = if useGHC6102 release
                            then "darcs:http://src.seereason.com/ghc6102/archive"
                            else "darcs:http://src.seereason.com/archive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-orphanage"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-orphanage"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hstringtemplate"
             , sourceSpec = if useGHC6102 release
                            then "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HStringTemplate/0.4.3/HStringTemplate-0.4.3.tar.gz:57139f6695f1c268ed38c34336191636):(darcs:http://src.seereason.com/ghc6102/debian/HStringTemplate-debian)"
                            else "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HStringTemplate/0.4.3/HStringTemplate-0.4.3.tar.gz:57139f6695f1c268ed38c34336191636):(darcs:http://src.seereason.com/debian/HStringTemplate-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-consumer"
             , sourceSpec = "deb-dir:(darcs:http://www.n-heptane.com/nhlab/repos/haskell-consumer):(darcs:http://src.seereason.com/debian/haskell-consumer-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-urlt"
             , sourceSpec = "darcs:http://src.seereason.com/urlt"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-xml"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/xml/1.3.4/xml-1.3.4.tar.gz:841c3a36a0bfb2e46f88448ecc227cad):(darcs:http://src.seereason.com/debian/haskell-xml-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-feed"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/feed/0.3.6/feed-0.3.6.tar.gz:0abeeaa7870ef241ccdc5785e608a599):(darcs:http://src.seereason.com/debian/haskell-feed-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digest"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digest/0.0.0.5/digest-0.0.0.5.tar.gz:ba60cc9d1ad6d0795ad84390976699d1):(darcs:http://src.seereason.com/debian/haskell-digest-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zip-archive"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zip-archive/0.1.1.3/zip-archive-0.1.1.3.tar.gz:edf3924c929b5592b1b9dbf4853b754f):(darcs:http://src.seereason.com/debian/haskell-zip-archive-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pandoc"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pandoc/1.2/pandoc-1.2.tar.gz:402999cf17dd7072e4c8c7b6b6050ec3):(darcs:http://src.seereason.com/debian/haskell-pandoc-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-opengl"
             , sourceSpec = "apt:sid:haskell-opengl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-glut"
             , sourceSpec = "apt:sid:haskell-glut"
             , relaxInfo = [] }

    , Target { sourcePackageName = "happstack-blog"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-blog"
             , relaxInfo = []
             }

    , Target { sourcePackageName = "haskell-utility-ht"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/ghc610/debian/haskell-utility-ht)"
            , relaxInfo = []
            }

    , Target { sourcePackageName = "haskell-gnuplot"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gnuplot/0.3/gnuplot-0.3.tar.gz:4432a0e0b44ca7ceae8e6737cf0258fa):(darcs:http://src.seereason.com/ghc610/debian/haskell-gnuplot)"
            , relaxInfo = []
            }

{-
  "quilt:(apt:sid:hs-plugins):(darcs:http://src.seereason.com/ghc610/quilt/hs-plugins-quilt)"
    - Needs an older cabal
  "deb-dir:(darcs:http://src.seereason.com/HSP/happs-hsp-template):(darcs:http://src.seereason.com/debian/happs-hsp-template-debian)"
    - Depends on hs-plugins
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cabal-install/0.6.0/cabal-install-0.6.0.tar.gz:ddce0bda54a99d816091e77ab6e4b39f):(darcs:http://src.seereason.com/ghc610/debian/cabal-install-debian)"
    - Requires 3000 < HTTP < 3002
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/readline/1.0.1.0/readline-1.0.1.0.tar.gz:eade9576def53ed293628a2f8580007e):(darcs:http://src.seereason.com/ghc610/debian/readline-debian)"
    - Can't find HsReadline.h
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac-readline/0.9/Shellac-readline-0.9.tar.gz:ffea10846cc5f40b84d6a4fe97c35ec9):(darcs:http://src.seereason.com/ghc610/debian/shellac-readline-debian)"
    - Requires readline
  "quilt:(apt:sid:darcs):(darcs:http://src.seereason.com/ghc610/quilt/darcs-quilt)"
    - Version 2.2.0 hangs when compiled with ghc 6.10 
  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/regex-pcre-builtin/0.94.2.0.7.7/regex-pcre-builtin-0.94.2.0.7.7.tar.gz:1e7f7ca729d344caa20c8f57d18239dd):(darcs:http://src.seereason.com/ghc610/debian/regex-pcre-builtin-debian)"
    - setup-bin: At least the following dependencies are missing: regex-base >=0.93
-}
    ]
-}

otherTargets release =
    [ Target { sourcePackageName = "tree-widget"
             , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
             , relaxInfo = [] }
    -- This target fails during an arch only build, because it has no architecture dependent files.
    , Target { sourcePackageName = "seereason-keyring"
             , sourceSpec = "darcs:http://src.seereason.com/seereason-keyring"
             , relaxInfo = [] 
             }
    , Target { sourcePackageName = "jquery"
             , sourceSpec = "apt:sid:jquery"
             , relaxInfo = [] 
             }
    , Target { sourcePackageName = "jqueryui"
             , sourceSpec = "apt:sid:jqueryui"
             , relaxInfo = [] 
             }
    , Target { sourcePackageName = "haskell-restarter"
             , sourceSpec = "darcs:http://src.seereason.com/Restarter"
             , relaxInfo = [] 
             }
    ]

privateTargets release =
    [ Target { sourcePackageName = "haskell-filecache"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-filecache"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-document"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-document"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-appraisal"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-appraisal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-mailinglist"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/mailingList"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-generic-formlets"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/generic-formlets-2"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-examples"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/happstack-examples"
             , relaxInfo = [] }
{-
    , Target { sourcePackageName = "happstack-blog"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/happstack-cms"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "happstack-imagegallery"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/imagegallery"
             , relaxInfo = [] }
{-  -- Uses newSession, which was removed from happstack
    , Target { sourcePackageName = "haskell-algebrazam"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/AlgebraZam"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-algebrazam-quiz"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/algebrazam-quiz"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-senioritymatters"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/SeniorityMatters"
             , relaxInfo = [] }
    ]
    where privateDarcsURI = "ssh://upload@deb.seereason.com/srv/darcs"


{-
  deb-dir:(darcs:http://code.haskell.org/checkers):(darcs:http://src.seereason.com/debian/checkers-debian)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/MemoTrie/0.0/MemoTrie-0.0.tar.gz):(darcs:http://src.seereason.com/debian/MemoTrie-debian)
  deb-dir:(darcs:http://darcs.haskell.org/packages/TypeCompose):(darcs:http://src.seereason.com/debian/TypeCompose-debian)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/Cabal/1.4.0.0/Cabal-1.4.0.0.tar.gz:5d8f10b95c42ac7419ac9673bfb6a607):(darcs:http://src.seereason.com/debian/cabal-debianization)
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/ghc-paths/0.1.0.4/ghc-paths-0.1.0.4.tar.gz:a8f36dcb5407de9907b7d78b31fc24a1):(darcs:http://src.seereason.com/debian/ghc-paths-debian)
  quilt:(darcs:http://www.cs.york.ac.uk/fp/darcs/hscolour):(darcs:http://src.seereason.com/quilt/hscolour-quilt)
  darcs:http://src.seereason.com/haskell-ugly
  darcs:http://src.seereason.com/mirror
  darcs:http://src.seereason.com/backups
  quilt:(apt:sid:xtla):(darcs:http://src.seereason.com/xtla-quilt)
  proc:apt:gutsy:neko
  proc:apt:gutsy:haxe
  deb-dir:(uri:http://hackage.haskell.org/packages/archive/colour/1.0.0/colour-1.0.0.tar.gz:97b0802abbf3a71a3606642850fe46c7):(darcs:http://src.seereason.com/debian/colour-debian)
  apt:sid:alex
  apt:sid:bnfc
  deb-dir:(darcs:http://darcs.haskell.org/crypto):(darcs:http://src.seereason.com/debian/haskell-crypto-debian)
  apt:sid:darcs
  apt:sid:darcs-monitor
  apt:sid:drift
  apt:sid:frown
  darcs:http://www.n-heptane.com/nhlab/repos/haskell-agi
  quilt:(apt:hardy:haskell-binary):(darcs:http://src.seereason.com/quilt/haskell-binary-quilt)
  apt:sid:haskell-doc
  quilt:(apt:sid:haskell-edison):(darcs:http://src.seereason.com/quilt/edison-quilt)
  apt:sid:haskell-hlist
  quilt:(apt:sid:haskell-http):(darcs:http://src.seereason.com/quilt/haskell-http-quilt)
  apt:sid:haskell-mode
  apt:sid:haskell-uulib
  apt:sid:helium
  apt:hardy:hmake
  quilt:(apt:sid:hslogger):(darcs:http://src.seereason.com/quilt/hslogger-quilt)
  quilt:(apt:sid:ldap-haskell):(darcs:http://src.seereason.com/quilt/ldap-haskell-quilt)
  apt:sid:lhs2tex
  quilt:(apt:sid:magic-haskell):(darcs:http://src.seereason.com/quilt/magic-haskell-quilt)
  apt:sid:uuagc
  apt:sid:whitespace
  sourcedeb:darcs:http://src.seereason.com/haskell-wordnet
  quilt:(apt:sid:xmonad):(darcs:http://src.seereason.com/quilt/xmonad-quilt)
  darcs:http://src.seereason.com/hlibrary
  apt:sid:haskelldb
    - Needs doc architecture fix
  apt:hardy:haskell-hsql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-mysql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-odbc
    - Needs doc architecture fix
  apt:sid:haskell-hsql-postgresql
    - Needs doc architecture fix
  apt:hardy:haskell-hsql-sqlite3
    - Needs doc architecture fix
  apt:sid:c2hs
    - Needs doc architecture fix
  apt:sid:washngo
    - Needs patch
  apt:sid:ftphs
    - Needs patch
  apt:sid:haskell-anydbm
    - Needs patch
  apt:sid:haskell-configfile
    - Needs patch
  apt:sid:haskell-hsh
    - Needs patch
  apt:sid:listlike
    - Needs patch
  quilt:(apt:sid:missingh):(darcs:http://src.seereason.com/quilt/missingh-quilt)
    - Needs patch
  apt:sid:gtkrsync
    - Needs patch
  quilt:(apt:sid:gtk2hs):(darcs:http://src.seereason.com/quilt/gtk2hs-quilt)
    - Needs patch
  apt:sid:arch2darcs
    - Needs patch
  apt:sid:hg-buildpackage
    - Needs patch
  apt:sid:srcinst
    - Needs patch
  apt:sid:dfsbuild
    - Needs patch
  apt:sid:darcs-buildpackage
    - Needs patch
  apt:sid:hat
    - Needs patch
  gtkrsync
    - depends on gtk2hs
  arch2darcs
    - depends on missingh
  darcs-buildpackage
    - depends on missingh and haskell-configfile
  dfsbuild
    - depends on missingh, haskell-configfile, haskell-hsh
  hg-buildpackage
    - depends on ???
  srcinst
    - depends on ???
  deb-dir:(darcs:http://code.haskell.org/vector-space):(darcs:http://src.seereason.com/debian/vector-space-debian)
    - broken
  apt:sid:ghc-cvs
    - broken
  apt:sid:haskell98-report
    - broken
  quilt:(apt:sid:hdbc):(darcs:http://src.seereason.com/quilt/hdbc-quilt)
    - broken
  apt:sid:hdbc-odbc
    - broken
  apt:sid:hdbc-postgresql
    - broken
  apt:sid:hdbc-sqlite3
    - broken
  apt:sid:hpodder
    - broken
  quilt:(darcs:http://code.haskell.org/encoding):(darcs:file:///home/david/darcs/haskell-encoding)
    - Patches won't apply:
  apt:sid:hdbc-missingh
    - Depends on ghc6 (<< 6.6+) or (<< 6.6-999)
  apt:sid:kaya
    - Error parsing build depends (unexpected #):
  apt:sid:missingpy
    - Disabled due to flaw in the autobuilder's build dependency parser:
  sourcedeb:tla:dsf@foxthompson.net--2004/haskell-binary--dsf--0.3.0
  tla:dsf@foxthompson.net--2004/hxt--dsf--7.0
  sourcedeb:tla:dsf@foxthompson.net--2004/cpphs--dsf--1.3
  quilt:(apt:feisty:haskell-http):(tla:dsf@foxthompson.net--2004/haskell-http-quilt--dsf--0)
  sourcedeb:tla:dsf@foxthompson.net--2004/yhc--dsf--0.7.0
Name: kernel-targets
Targets:
  apt:gutsy:linux-source-2.6.22
  apt:gutsy:linux-meta
  quilt:(apt:gutsy:linux-restricted-modules-2.6.22):(tla:tos@linspire.com--skipjack/linux-restricted-modules-quilt--ubuntu--0)
Comment: Here are some more proposed targets
  tla:tos@linspire.com--skipjack/forward-oss-kernel-module--cnr--20070605
  tla:tos@linspire.com--skipjack/forward-oss--build-skipjack--0.3
  quilt:(apt:${base}:bcm43xx-fwcutter):(tla:tos@linspire.com--skipjack/bcm43xx-fwcutter-quilt--cnr--0)
-}
