{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Hackage ( targets ) where

import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

targets _home =
    [ Target { sourcePackageName = "haskell-aes"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/AES/0.2.7/AES-0.2.7.tar.gz:c40edee8615226c78139dd5ca864ad9e):(darcs:http://src.seereason.com/haskell-aes-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-terminal"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/ansi-terminal/0.5.2/ansi-terminal-0.5.2.tar.gz:240b517c3775f1d5753c01ed52975ef1):(darcs:http://src.seereason.com/ansi-terminal-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-wl-pprint"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/ansi-wl-pprint/0.5.1/ansi-wl-pprint-0.5.1.tar.gz:d427a18a5a071b8dbcdff28633f4b800):(darcs:http://src.seereason.com/ansi-wl-pprint-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.6/applicative-extras-0.1.6.tar.gz:9eee19f8bc916fe7b28345eb599c28fa):(darcs:http://src.seereason.com/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-attempt"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/attempt/0.3.0/attempt-0.3.0.tar.gz:051be218858e24ffd53b4e435f024611):(darcs:http://src.seereason.com/haskell-attempt-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-authenticate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/authenticate/0.9.1.4/authenticate-0.9.1.4.tar.gz:9ffdf0f3f7c40c507abef8ab2d6df347):(darcs:" ++ repo ++ "/haskell-authenticate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bimap"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bimap/0.2.4/bimap-0.2.4.tar.gz:f6b79bff5741e709f1536df411aab53d):(darcs:http://src.seereason.com/haskell-bimap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitset"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bitset/1.0/bitset-1.0.tar.gz:466eb0fd8a92b16e705a219f0d01a54c):(darcs:http://src.seereason.com/haskell-bitset-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-blaze-from-html"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-from-html/0.3.1.0/blaze-from-html-0.3.1.0.tar.gz:8a20c2b8ead12ddfe65a8fb3cb2b7236):(darcs:http://src.seereason.com/haskell-blaze-from-html-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bytestring-trie"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bytestring-trie/0.2.2/bytestring-trie-0.2.2.tar.gz:87055596ec7c40270e3366f4ab5a0e55):(darcs:http://src.seereason.com/haskell-bytestring-trie-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delcont"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-convertible-text"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/convertible-text/0.3.0.6/convertible-text-0.3.0.6.tar.gz:8335ddd1155bcdb63be82e53c97b342d):(darcs:http://src.seereason.com/haskell-convertible-text-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object-json"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-object-json/0.3.1.4/data-object-json-0.3.1.4.tar.gz:8077d5a05b18e711d5a5bb4f3c2d1881):(darcs:http://src.seereason.com/haskell-data-object-json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-object/0.3.1.5/data-object-0.3.1.5.tar.gz:9580477eadd2e2bfdace7228c0a24cda):(darcs:http://src.seereason.com/haskell-data-object-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digestive-functors/0.1.0.0/digestive-functors-0.1.0.0.tar.gz:ca2d55dc58ef8b5a19990f944a0300d4):(darcs:http://src.seereason.com/haskell-digestive-functors-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors-happstack"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digestive-functors-happstack/0.1.0.0/digestive-functors-happstack-0.1.0.0.tar.gz:791d790db823894b49aa5bf3c1710f0e):(darcs:http://src.seereason.com/haskell-digestive-functors-happstack-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/formlets/0.7.3/formlets-0.7.3.tar.gz:456acbc7eb4922dd991e50611f917d77):(darcs:http://src.seereason.com/haskell-formlets-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-frisby"
             , sourceSpec = "deb-dir:(cd:frisby:darcs:http://src.seereason.com/frisby):(darcs:http://src.seereason.com/frisby-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-funsat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/funsat/0.6.1/funsat-0.6.1.tar.gz:6ec67ada1b478c85cbcd8e47f11b5643):(darcs:http://src.seereason.com/haskell-funsat-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gd/3000.4.0/gd-3000.4.0.tar.gz:7bc5bb68638b807d592aba433beb3fa5):(darcs:http://src.seereason.com/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gnuplot"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/gnuplot/0.3/gnuplot-0.3.tar.gz:4432a0e0b44ca7ceae8e6737cf0258fa):(darcs:http://src.seereason.com/haskell-gnuplot-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack"
             -- , sourceSpec = "deb-dir:(cd:happstack:darcs:" ++ happstackRepo ++ "):(darcs:" ++ repo ++ "/happstack-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack/6.0.0/happstack-6.0.0.tar.gz:dfc0fbb9b85b759fb1e61063373fa6d8):(darcs:http://src.seereason.com/happstack-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             -- , sourceSpec = "deb-dir:(cd:happstack-data:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-data-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz:be72c4c11d1317bf52c80782eac28a2d):(darcs:http://src.seereason.com/happstack-data-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-ixset"
             -- , sourceSpec = "deb-dir:(cd:happstack-ixset:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-ixset-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-ixset/6.0.0/happstack-ixset-6.0.0.tar.gz:4353295433c22c44524d520e209c6511):(darcs:http://src.seereason.com/happstack-ixset-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-server"
             -- , sourceSpec = "deb-dir:(cd:happstack-server:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-server-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-server/6.0.0/happstack-server-6.0.0.tar.gz:65b7f11f70b5c549b80b36a32436c8f9):(darcs:http://src.seereason.com/happstack-server-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-state"
             -- , sourceSpec = "deb-dir:(cd:happstack-state:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-state-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-state/6.0.0/happstack-state-6.0.0.tar.gz:c625f04118c4701e632eb9acc85148de):(darcs:http://src.seereason.com/happstack-state-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-util"
             -- , sourceSpec = "deb-dir:(cd:happstack-util:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-util-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-util/6.0.0/happstack-util-6.0.0.tar.gz:0839c54a7bb415d57e44a5d044562278):(darcs:http://src.seereason.com/happstack-util-debian)"
             , relaxInfo = [] }
    -- Depends on pandoc
    , Target { sourcePackageName = "haskell-safecopy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/safecopy/0.5.1/safecopy-0.5.1.tar.gz:8ad2cc63e1d2bbda9e3111c359b2ce9f):(darcs:http://src.seereason.com/haskell-safecopy-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-heap"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/heap/1.0.0/heap-1.0.0.tar.gz:7a650f3803da5c0ea5c865d0ef1c0857):(darcs:http://src.seereason.com/heap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hoauth"
             -- , sourceSpec = "apt:sid:haskell-hoauth" -- no available versions?
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hoauth/0.2.5/hoauth-0.2.5.tar.gz:566cdc96a21b077a9a1d39922c0e4332):(darcs:http://src.seereason.com/haskell-hoauth-debian)"
             , relaxInfo = [] }
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , Target { sourcePackageName = "haskell-hostname"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/hostname/1.0/hostname-1.0.tar.gz:9389c8634239e5d6096ef563e59a703f):(darcs:http://src.seereason.com/haskell-hostname-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hpdf"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HPDF/1.4.2/HPDF-1.4.2.tar.gz:ba364b6e397413931b65a56e63b19bb4):(darcs:http://src.seereason.com/haskell-hpdf-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsopenssl"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HsOpenSSL/0.8.0.2/HsOpenSSL-0.8.0.2.tar.gz:5ed5a3b42a73e5cab5ca9c45539d7a7a):(darcs:http://src.seereason.com/haskell-hsopenssl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-incremental-sat-solver"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/incremental-sat-solver/0.1.7/incremental-sat-solver-0.1.7.tar.gz:3d5f3d0bff3a92f4207631fbe9b32c36):(darcs:http://src.seereason.com/haskell-incremental-sat-solver-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-instant-generics"
             , sourceSpec = "proc:deb-dir:(uri:http://hackage.haskell.org/packages/archive/instant-generics/0.2.1/instant-generics-0.2.1.tar.gz:b6a37bfa18b054800ca8c5ce5dbe4e87):(darcs:http://src.seereason.com/haskell-instant-generics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-jsonb"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/JSONb/1.0.4/JSONb-1.0.4.tar.gz:071e186dcab3fe3ab262903895f5f670):(darcs:http://src.seereason.com/haskell-jsonb-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-monadlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/monadLib/3.6.1/monadLib-3.6.1.tar.gz:09ed56db9cd49af9f40789a0c7102759):(darcs:http://src.seereason.com/haskell-monadlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-murmur-hash"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/murmur-hash/0.1/murmur-hash-0.1.tar.gz:9bab434a87a7d611bc21e484977ce88f):(darcs:http://src.seereason.com/haskell-murmur-hash-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-nano-hmac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/nano-hmac/0.2.0/nano-hmac-0.2.0.tar.gz:96e93a3a51be4d659f07a241dc171ff4):(darcs:http://src.seereason.com/haskell-nano-hmac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-openid"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/openid/0.1.4.6/openid-0.1.4.6.tar.gz:1f121c2d0dc9508203db37e476586de8):(darcs:http://src.seereason.com/haskell-openid-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-operational"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/operational/0.2.0.1/operational-0.2.0.1.tar.gz:40b3fe5f20a9c49df1da55d86872064a):(darcs:http://src.seereason.com/operational-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parse-dimacs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/parse-dimacs/1.2/parse-dimacs-1.2.tar.gz:2a3bd8f6b0dd638567c172ef16e3e191):(darcs:http://src.seereason.com/parse-dimacs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pbkdf2"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/PBKDF2/0.3.1/PBKDF2-0.3.1.tar.gz:46a3109e272d296fefea4c1d2015173a):(darcs:http://src.seereason.com/pbkdf2-debian)"

             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-permutation"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/permutation/0.4.1/permutation-0.4.1.tar.gz:a9e0b6231d7a085719188406f59ab1aa):(darcs:http://src.seereason.com/haskell-permutation)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-psqueue"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/PSQueue/1.1/PSQueue-1.1.tar.gz:43649b94d88fe103233e6897a014a89f):(darcs:http://src.seereason.com/haskell-psqueue-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pwstore-purehaskell"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pwstore-purehaskell/2.1/pwstore-purehaskell-2.1.tar.gz:083cc7b79c4b1ad26819a08bbe120f81):(darcs:http://src.seereason.com/haskell-pwstore-purehaskell-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-sat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/sat/1.1.1/sat-1.1.1.tar.gz:5a974083ef008b32720b617fe5fb30a2):(darcs:http://src.seereason.com/haskell-sat-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework-hunit"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/test-framework-hunit/0.2.6/test-framework-hunit-0.2.6.tar.gz:7c012ca5426d743b3cf35e231d6f6072):(darcs:http://src.seereason.com/haskell-test-framework-hunit-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework-quickcheck"
            , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/test-framework-quickcheck/0.2.7/test-framework-quickcheck-0.2.7.tar.gz:278d06b64171ba36bf658ada61bcec44):(darcs:http://src.seereason.com/haskell-test-framework-quickcheck-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/test-framework/0.3.2/test-framework-0.3.2.tar.gz:9db832bd496d3f525e2fdf45b63cb0de):(darcs:http://src.seereason.com/haskell-test-framework-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-names"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/unicode-names/3.2.0.0/unicode-names-3.2.0.0.tar.gz:0441831bb24b7e891668df5db96395c5):(darcs:http://src.seereason.com/haskell-unicode-names-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-properties"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/unicode-properties/3.2.0.0/unicode-properties-3.2.0.0.tar.gz:efdb2a0021c328f23cc7026d31b23497):(darcs:http://src.seereason.com/haskell-unicode-properties-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-utf8-prelude"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utf8-prelude/0.1.6/utf8-prelude-0.1.6.tar.gz:582665f0f8fde8e8c8528487f325c10d):(darcs:http://src.seereason.com/utf8-prelude-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-encodings"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/web-encodings/0.3.0.6/web-encodings-0.3.0.6.tar.gz:fa8dcbc24b4c4aa8e481107017609d04):(darcs:http://src.seereason.com/haskell-web-encodings-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parseargs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/parseargs/0.1.3.2/parseargs-0.1.3.2.tar.gz:0b2693002b4dcc45b90005927818b0a5):(darcs:http://src.seereason.com/haskell-parseargs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-th-lift"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/th-lift/0.5.3/th-lift-0.5.3.tar.gz:eaf567442887bd93c374e78fede6bf50):(darcs:http://src.seereason.com/haskell-th-lift-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskell-src-meta"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/haskell-src-meta/0.4/haskell-src-meta-0.4.tar.gz:d4ea030e9d95ffb672737024b5df8e02):(darcs:http://src.seereason.com/haskell-haskell-src-meta-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-jmacro"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/jmacro/0.5/jmacro-0.5.tar.gz:103fb0eb730622e6e4d7e611734966a1):(darcs:http://src.seereason.com/haskell-jmacro-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitmap"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bitmap/0.0.1/bitmap-0.0.1.tar.gz:e602c579e81db27a35e7fdef3bb87312):(darcs:http://src.seereason.com/haskell-bitmap-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitmap-opengl"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bitmap-opengl/0.0.0/bitmap-opengl-0.0.0.tar.gz:a9d185a25595550b91bffb1bd75a9ff7):(darcs:http://src.seereason.com/haskell-bitmap-opengl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.2/stb-image-0.2.tar.gz:49091496ead58432fc6e0d17f15e13e7):(darcs:http://src.seereason.com/haskell-stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vacuum"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/vacuum/1.0.0/vacuum-1.0.0.tar.gz:c87c2e26662b7b95162d7a34023105ad):(darcs:http://src.seereason.com/haskell-vacuum-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vacuum-opengl"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/vacuum-opengl/0.0.3/vacuum-opengl-0.0.3.tar.gz:c92b412c56a1bd4b29c33525dd2425fe):(darcs:http://src.seereason.com/haskell-vacuum-opengl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rsa"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RSA/1.0.6.1/RSA-1.0.6.1.tar.gz:c240f2a7d0dcd0d29622cbf803783564):(darcs:" ++ repo ++ "/haskell-rsa-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-aeson"

             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/aeson/0.3.2.7/aeson-0.3.2.7.tar.gz:7567d0bf65908b81606b131160a42b1d):(darcs:" ++ repo ++ "/haskell-aeson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hashable"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hashable/1.1.2.0/hashable-1.1.2.0.tar.gz:87f2d33bba7cbe1541daa4ff87d15c04):(darcs:" ++ repo ++ "/haskell-hashable-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unordered-containers"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/unordered-containers/0.1.3.0/unordered-containers-0.1.3.0.tar.gz:4a8505db2d504d7471f8e5cf7ebb9655):(darcs:" ++ repo ++ "/haskell-unordered-containers-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-blaze-textual"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-textual/0.1.0.0/blaze-textual-0.1.0.0.tar.gz:6d0461e201b243d5a613f90f3a203dd3):(darcs:" ++ repo ++ "/haskell-blaze-textual-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http-enumerator"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/http-enumerator/0.6.5.4/http-enumerator-0.6.5.4.tar.gz:3f91c23021f4e22c1f8c0883c71bba64):(darcs:" ++ repo ++ "/haskell-http-enumerator-debian)"
             , relaxInfo = [] }
    ]