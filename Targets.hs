module Targets where

import Data.List (isPrefixOf)
import Debian.AutoBuilder.ParamClass (Target(..))

------------------------ TARGETS ---------------------

publicTargets release =
    ghc6CoreTargets release ++	-- The compiler and our packaging tools
    autobuilderTargets release ++	-- The autobuilder and its dependencies
    ghc6Targets release ++		-- Haskell targets
    otherTargets release		-- Non-haskell targets

-- Only use this for targets when we know that karmic is trumping sid.
-- Put an =version on the apt target so we notice when it changes.
aptSidOrKarmic release name version =
    "apt:" ++ (if karmic then "karmic" else "sid") ++ ":" ++ name ++ (if karmic then "=" ++ version else "")
    where karmic = isPrefixOf "karmic-" release

-- This module defines how we obtain and assemble the source code for
-- the packages we want to build.

ghc6CoreTargets release =
    [ Target { sourcePackageName = "ghc6"
             , sourceSpec = "deb-dir:(uri:http://www.haskell.org/ghc/dist/current/dist/ghc-6.13.20091231-src.tar.bz2:a5a9a4ac9fd2823ed0a1928a9a89498c):(darcs:http://src.seereason.com/ghc614-debian-sid)"
             , relaxInfo = ["ghc6"
                           ,"happy"
                           ,"alex"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"debhelper"
                           ,"quilt"] }
{-  , Target { sourcePackageName = "haskell-hscolour"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hscolour/1.15/hscolour-1.15.tar.gz:ae1f3d4b1b9911ca80735548b0a622d5):(darcs:http://src.seereason.com/hscolour-debian-nopatch)"
             , relaxInfo = ["hscolour"] } -}
    , Target { sourcePackageName = "haddock"
             , sourceSpec = "darcs:http://src.seereason.com/haddock-dummy"
             , relaxInfo = [] }
    , Target { sourcePackageName = "hscolour"
             , sourceSpec = "apt:sid:hscolour"
             , relaxInfo = ["hscolour"] }
{-  , Target { sourcePackageName = "haskell-happy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happy/1.18.2/happy-1.18.2.tar.gz:adb1679a1fa8cec74a6e621a4a277e98):(darcs:http://src.seereason.com/happy-debian)"
             , relaxInfo = ["happy"] } -}
    , Target { sourcePackageName = "happy"
             , sourceSpec = "apt:" ++ (if isPrefixOf "jaunty-" release then "karmic" else "sid") ++ ":happy"
             , relaxInfo = ["happy"] }
    , Target { sourcePackageName = "haskell-utf8-string"
             , sourceSpec = "quilt:(apt:sid:haskell-utf8-string):(darcs:http://src.seereason.com/haskell-utf8-string-quilt)"
             , relaxInfo = ["hscolour", "cpphs"] }
    , Target { sourcePackageName = "haskell-zlib"
             , sourceSpec = "apt:sid:haskell-zlib"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hpdf"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HPDF/1.4.2/HPDF-1.4.2.tar.gz:ba364b6e397413931b65a56e63b19bb4):(darcs:http://src.seereason.com/haskell-hpdf-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bzlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/haskell-bzlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskell-src"
             , sourceSpec = "quilt:(apt:sid:haskell-haskell-src):(darcs:http://src.seereason.com/haskell-haskell-src-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html"
             , sourceSpec = "apt:sid:haskell-html"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hunit"
             , sourceSpec = "apt:sid:haskell-hunit"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mtl"
             , sourceSpec = "apt:sid:haskell-mtl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-network"
             , sourceSpec = "quilt:(apt:sid:haskell-network):(darcs:http://src.seereason.com/haskell-network-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-network-bytestring"
             , sourceSpec="quilt:(apt:sid:haskell-network-bytestring):(darcs:http://src.seereason.com/haskell-network-bytestring-quilt)"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-parallel"
             , sourceSpec = "apt:sid:haskell-parallel"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parsec"
             , sourceSpec = "quilt:(apt:sid:haskell-parsec):(darcs:http://src.seereason.com/haskell-parsec-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parsec2"
             , sourceSpec = "apt:sid:haskell-parsec2"
             , relaxInfo = [] }
    -- Binary packages: libghc6-quickcheck2-dev, libghc6-quickcheck2-prof, libghc6-quickcheck2-doc
    , Target { sourcePackageName = "haskell-quickcheck"
             , sourceSpec = "quilt:(apt:sid:haskell-quickcheck):(darcs:http://src.seereason.com/haskell-quickcheck-quilt)"
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
    , Target { sourcePackageName = "haskell-regex-tdfa"
             , sourceSpec = "apt:sid:haskell-regex-tdfa"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stm"
             , sourceSpec = "apt:sid:haskell-stm"
             , relaxInfo = [] }
    -- Patch to add the bareAttr function and remove the custom show
    -- instance Specify a particular version of xhtml so we can
    -- perhaps eliminate this the next time the upstream package is
    -- revved.
    , Target { sourcePackageName = "haskell-xhtml"
             , sourceSpec = "quilt:(apt:sid:haskell-xhtml=3000.2.0.1-5):(darcs:http://src.seereason.com/haskell-xhtml-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "html-xml-utils"
           , sourceSpec = "apt:sid:html-xml-utils"
           , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-devscripts"
             -- Working version
             --, sourceSpec = "quilt:(apt:sid:haskell-devscripts):(darcs:http://src.seereason.com/haskell-devscripts-quilt)"
             , sourceSpec = "apt:sid:haskell-devscripts"
             -- Experimental version
             -- , sourceSpec = "quilt:(darcs:http://darcs.debian.org/pkg-haskell/haskell-devscripts):(darcs:http://src.seereason.com/haskell-devscripts-quilt)"
             , relaxInfo = ["hscolour"] }
    , Target { sourcePackageName = "haskell-debian"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-debian"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-debian-repo"
             , relaxInfo = [] }
    -- Known dependants in public:
    -- [haskell-happstack-extra, haskell-happstack-facebook, autobuilder, haskell-archive, haskell-debian,
    --  haskell-debian-repo, build-env, haskell-debian-mirror, haskell-help]
    , Target { sourcePackageName = "haskell-extra"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-extra"
             , relaxInfo = ["cabal-debian"] }
    -- Known dependants in public:
    -- [haskell-happstack-extra, haskell-happstack-facebook, autobuilder, haskell-archive, haskell-debian,
    --  haskell-debian-repo, build-env, haskell-debian-mirror, haskell-extra, haskell-help]
    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cpphs"
             , sourceSpec = "apt:sid:cpphs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-tagsoup"
             , sourceSpec = "apt:sid:haskell-tagsoup"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haxml"
             , sourceSpec = "apt:sid:haxml"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstackdotcom"
             , sourceSpec = "darcs:http://patch-tag.com/r/stepcut/happstackDotCom"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-irc"
             , sourceSpec = "apt:sid:haskell-irc"
             , relaxInfo = [] }
    ]

autobuilderTargets release =
    [ Target { sourcePackageName = "build-env"
             , sourceSpec = "darcs:http://src.seereason.com/build-env"
             , relaxInfo = [] }
    , Target { sourcePackageName = "autobuilder"
             , sourceSpec = "darcs:http://src.seereason.com/autobuilder"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cgi"
             , sourceSpec = "quilt:(apt:sid:haskell-cgi):(darcs:http://src.seereason.com/haskell-cgi-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mime"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-mime"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-magic"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/magic/1.0.8/magic-1.0.8.tar.gz:e81c493fe185431a5b70d4855ed4b87f):(darcs:http://src.seereason.com/magic-debian)"
             , relaxInfo = [] }
    ]

-- Note that some of the debian source packages have names that don't begin with haskell-:
--   pandoc, magic-haskell, hslogger
ghc6Targets release =
    [ Target { sourcePackageName = "haskell-http"
             , sourceSpec = "quilt:(apt:sid:haskell-http):(darcs:http://src.seereason.com/haskell-http-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.5/applicative-extras-0.1.5.tar.gz:b5a6288629133529c0951d5b71a21954):(darcs:http://src.seereason.com/applicative-extras-debian)"
             , relaxInfo = [] }
{-      
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "darcs:http://src.seereason.com/formlets"
             , relaxInfo = [] }
-}
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/formlets/0.7.1/formlets-0.7.1.tar.gz:9518af3a2b3e1df5181530db8780e774):(darcs:http://src.seereason.com/formlets-debian)"
             , relaxInfo = [] }
      
{-  , Target { sourcePackageName = "haskell-binary"
             , sourceSpec = "quilt:(apt:sid:haskell-binary):(darcs:http://src.seereason.com/haskell-binary-quilt)"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-maybet"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/MaybeT/0.1.2/MaybeT-0.1.2.tar.gz:9864a3f34151217004f8c968fda5b427):(darcs:http://src.seereason.com/MaybeT-debian)"
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
    -- Depends on pandoc
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html-entities"
             , sourceSpec = "darcs:http://src.seereason.com/html-entities"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pandoc"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pandoc/1.2/pandoc-1.2.tar.gz:402999cf17dd7072e4c8c7b6b6050ec3):(darcs:http://src.seereason.com/haskell-pandoc-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-facebook"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-facebook"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-json"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/json/0.4.3/json-0.4.3.tar.gz:1af33c67594f69048b69d4aeafeea03e):(darcs:http://src.seereason.com/json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/iconv"
             , relaxInfo = [] }

--  The Sid package has no profiling libraries, so dependent packages
--  won't build.  Use our debianization instead.
    , Target { sourcePackageName = "haskell-hslogger"
             , sourceSpec = "deb-dir:(uri:http://ftp.de.debian.org/debian/pool/main/h/hslogger/hslogger_1.0.8.orig.tar.gz:5ce575887d2b6076793f8736b077626a):(darcs:http://src.seereason.com/hslogger-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-harp"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/harp):(darcs:http://src.seereason.com/harp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-src-exts"
             , sourceSpec = "quilt:(apt:sid:haskell-src-exts):(darcs:http://src.seereason.com/haskell-src-exts-quilt)"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-hsx"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsx/0.6.1/hsx-0.6.1.tar.gz:e31dec6cdc587f6fe0e3f5117b418ac1):(darcs:http://src.seereason.com/hsx-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjavascript"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HJavaScript/0.4.6/HJavaScript-0.4.6.tar.gz:440ee4f0bcffe6e60ee0ce02dd3922cf):(darcs:http://src.seereason.com/hjavascript-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjscript"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HJScript/0.4.9/HJScript-0.4.9.tar.gz:4d11ae14faadd854e3e69a1fd6393b5a):(darcs:http://src.seereason.com/hjscript-debian)"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-hsp"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/hsp):(darcs:http://src.seereason.com/hsp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/happs-hsp-formlets"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hsx-xhtml):(darcs:http://src.seereason.com/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-frisby"
             , sourceSpec = "deb-dir:(cd:frisby:darcs:http://repetae.net/repos/frisby):(darcs:http://src.seereason.com/debian/frisby-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delconto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = "darcs:http://src.seereason.com/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = "darcs:http://src.seereason.com/archive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hstringtemplate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HStringTemplate/0.4.3/HStringTemplate-0.4.3.tar.gz:57139f6695f1c268ed38c34336191636):(darcs:http://src.seereason.com/HStringTemplate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-consumer"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-consumer"
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
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zip-archive/0.1.1.5/zip-archive-0.1.1.5.tar.gz:4bedb05a838eac253343df1f882366e3):(darcs:http://src.seereason.com/haskell-zip-archive-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-utility-ht"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/debian/haskell-utility-ht-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gnuplot"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gnuplot/0.3/gnuplot-0.3.tar.gz:4432a0e0b44ca7ceae8e6737cf0258fa):(darcs:http://src.seereason.com/debian/haskell-gnuplot-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class"
             , sourceSpec = "quilt:(apt:sid:haskell-syb-with-class):(darcs:http://src.seereason.com/haskell-syb-with-class-quilt)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-hsemail"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsemail/1.3/hsemail-1.3.tar.gz:e1b84af260aa36c017c190ce481e2c1a):(darcs:http://src.seereason.com/debian/haskell-hsemail-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-smtpclient"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/SMTPClient/1.0.1/SMTPClient-1.0.1.tar.gz:c6e02189636b608e27942dbb9af9732a):(darcs:http://src.seereason.com/debian/haskell-smtpclient-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-strict-concurrency"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/strict-concurrency/0.2.2/strict-concurrency-0.2.2.tar.gz:fe09148d0f2da0a343b492253aedea3e):(darcs:http://src.seereason.com/debian/haskell-strict-concurrency-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-unix-compat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/unix-compat/0.1.2.1/unix-compat-0.1.2.1.tar.gz:6ecfc3922fce2e96922af3a636b061f9):(darcs:http://src.seereason.com/debian/haskell-unix-compat-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-sendfile"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/sendfile/0.6.1/sendfile-0.6.1.tar.gz:35516b5bebe0566591dda3f78c5dcd9e):(darcs:http://src.seereason.com/debian/haskell-sendfile-debian)"
--           , sourceSpec = "deb-dir:(darcs:http://patch-tag.com/r/mae/sendfile/pullrepo):(darcs:http://src.seereason.com/debian/haskell-sendfile-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-text"
--           , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/text/0.5/text-0.5.tar.gz:f506b0866aa9b476a35e0960870cc4d1):(darcs:http://src.seereason.com/haskell-text-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/text/0.7.1.0/text-0.7.1.0.tar.gz:019036c9111b521654eab2b9dee7648d):(darcs:http://src.seereason.com/haskell-text-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-deepseq"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/deepseq/1.1.0.0/deepseq-1.1.0.0.tar.gz:41194f8633be8e30cacad88146dbf7c2):(darcs:http://src.seereason.com/haskell-deepseq-debian)"
             , relaxInfo = [] }
    -- Required by the darcs 2.3.0-3 in sid.
    , Target { sourcePackageName = "bash-completion"
             , sourceSpec = "apt:sid:bash-completion"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskeline"
             , sourceSpec = "apt:sid:haskell-haskeline"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mmap"
             , sourceSpec = "apt:sid:haskell-mmap"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hashed-storage"
             , sourceSpec = "apt:sid:haskell-hashed-storage"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-terminfo"
             , sourceSpec = aptSidOrKarmic release "haskell-terminfo" "0.3.0.2-2build1"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-permutation"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/permutation/0.4.1/permutation-0.4.1.tar.gz:a9e0b6231d7a085719188406f59ab1aa):(darcs:http://src.seereason.com/haskell-permutation)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-data-accessor"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor/0.2.1.2/data-accessor-0.2.1.2.tar.gz:8eabca77e2202dc0735b45ca7dea9175):(darcs:http://src.seereason.com/haskell-data-accessor-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-data-accessor-template"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor-template/0.2.1.2/data-accessor-template-0.2.1.2.tar.gz:0feb98757d889bbc9fbbe0379cdb3c05):(darcs:http://src.seereason.com/haskell-data-accessor-template-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-erf"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/erf/1.0.0.0/erf-1.0.0.0.tar.gz:1bdb56838565abfaa7d7ab6e2870ddaa):(darcs:http://src.seereason.com/haskell-erf-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-colour"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/colour/2.3.1/colour-2.3.1.tar.gz:5edced36d4c27393ae1ce1389eeb25ad):(darcs:http://src.seereason.com/haskell-colour-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-statistics"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/statistics/0.3.5/statistics-0.3.5.tar.gz:b351bee9514e26555f170676b3c66139):(darcs:http://src.seereason.com/haskell-statistics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-uvector"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.1.0/uvector-0.1.1.0.tar.gz:423e254dbbef0b57687f8adc737f7901):(darcs:http://src.seereason.com/haskell-uvector-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-uvector-algorithms"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/haskell-uvector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-transformers"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/transformers/0.1.4.0/transformers-0.1.4.0.tar.gz:6edd0f22594c477b05fd059fdac2c5a9):(darcs:http://src.seereason.com/haskell-transformers-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-utility-ht"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/debian/haskell-utility-ht-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-agi"
             , sourceSpec="darcs:http://src.seereason.com/haskell-agi"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-syb"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/syb/0.1.0.3/syb-0.1.0.3.tar.gz:5c97226fa413790c677b08049e2dc6b8):(darcs:http://src.seereason.com/debian/haskell-syb-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-logict"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/logict/0.4/logict-0.4.tar.gz:39eeb4aa1d7a67b1c4865f01ca417b7d):(darcs:http://src.seereason.com/debian/haskell-logict-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="tinymce"
             , sourceSpec="apt:sid:tinymce"
             , relaxInfo = [] }
    , Target { sourcePackageName="wordpress"
             , sourceSpec="apt:sid:wordpress"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-proplogic"
             , sourceSpec="deb-dir:(uri:http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz:e2fb3445dd16d435e81d7630d7f78c01):(darcs:http://src.seereason.com/haskell-proplogic-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-operational"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/operational/0.1.0.0/operational-0.1.0.0.tar.gz:9244bef3a6ac89a3792c9b8617df3c08):(darcs:http://src.seereason.com/operational-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "darcs"
             -- , sourceSpec = "quilt:(apt:sid:darcs):(darcs:http://src.seereason.com/darcs-quilt)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/darcs/2.4/darcs-2.4.tar.gz:169a6d245a33da97b2daa0eda60b28e5):(darcs:http://src.seereason.com/darcs-debian)"
             , relaxInfo = [] }
    ]

otherTargets release =
    [ -- This target fails during an arch only build, because it has no architecture dependent files.
      Target { sourcePackageName = "seereason-keyring"
             , sourceSpec = "darcs:http://src.seereason.com/seereason-keyring"
             , relaxInfo = [] }
    , Target { sourcePackageName = "jquery"
             , sourceSpec = "apt:sid:jquery"
             , relaxInfo = [] 
             }
    , Target { sourcePackageName = "jqueryui"
             , sourceSpec = "apt:sid:jqueryui"
             , relaxInfo = [] 
             }
    ]

failingTargets release =
    [ Target { sourcePackageName = "haskell-uniplate"
--           , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/uniplate/1.2.0.3/uniplate-1.2.0.3.tar.gz:e0e10700870f5b9756d4097e640164ca):(darcs:http://src.seereason.com/uniplate-debian)"
             , sourceSpec = "apt:sid:haskell-uniplate"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-benchpress"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/shellac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pcre-light"
             , sourceSpec = "apt:sid:haskell-pcre-light"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-configfile"
             , sourceSpec = "apt:sid:haskell-configfile"
             , relaxInfo = [] }
    -- Requires Cabal >= 1.7.3, ghc 6.10.3 comes with Cabal-1.6.0.3
    , Target { sourcePackageName = "haskell-cabal-install"
             , sourceSpec = "deb-dir:(darcs:http://darcs.haskell.org/cabal-install):(darcs:http://src.seereason.com/cabal-install-debian)"
             , relaxInfo = [] }
    -- We need debhelper >= 7.0.50 for darcs 2.3.0.
    -- However, one of its unit tests fails:
    -- #   Failed test 'unavailable jobserver'
    -- #   at t/buildsystems/buildsystem_tests line 540.
{-  , Target { sourcePackageName = "debhelper"
             , sourceSpec = "apt:sid:debhelper"
             , relaxInfo = [] } -}
    -- Required for darcs 2.3.0
    -- Fails because of missing dependency libssh2-1-dev
{-  , Target { sourcePackageName = "curl"
             , sourceSpec = "apt:sid:curl"
             , relaxInfo = [] } -}
{-  , Target { sourcePackageName = "tree-widget"
             , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
             , relaxInfo = [] } -}
{-  , Target { sourcePackageName = "gtk2hs"
             , sourceSpec = "apt:sid:gtk2hs"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-chart"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.11/Chart-0.11.tar.gz:b7f67defe06694eef580542947106fc0):(darcs:http://src.seereason.com/haskell-chart-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName="haskell-criterion"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/criterion/0.1.2/criterion-0.1.2.tar.gz:0e4d1c2f546ab650e03c610034c20226):(darcs:http://src.seereason.com/haskell-criterion-debian)"
             , relaxInfo = [] } -}
{-  , Target { sourcePackageName = "haskell-restarter"
             , sourceSpec = "darcs:http://src.seereason.com/Restarter"
             , relaxInfo = [] } -}
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
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/generic-formlets"
             , relaxInfo = [] }
{-  , Target { sourcePackageName = "haskell-happstack-examples"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/happstack-examples"
             , relaxInfo = [] } -}
  , Target { sourcePackageName = "happstack-blog"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/happstack-cms"
             , relaxInfo = [] }
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
    , Target { sourcePackageName = "haskell-seereason"
             , sourceSpec = "darcs:" ++ privateDarcsURI ++ "/haskell-seereason"
             , relaxInfo = [] }
    ]
    where privateDarcsURI = "ssh://upload@deb.seereason.com/srv/darcs"
