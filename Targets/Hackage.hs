{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Hackage ( targets ) where

import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

targets _home =
    [ Target { sourcePackageName = "haskell-aes"
             , sourceSpec = "deb-dir:(hackage:AES):(darcs:http://src.seereason.com/haskell-aes-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-terminal"
             , sourceSpec = "deb-dir:(hackage:ansi-terminal):(darcs:http://src.seereason.com/ansi-terminal-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-wl-pprint"
             , sourceSpec = "deb-dir:(hackage:ansi-wl-pprint):(darcs:http://src.seereason.com/ansi-wl-pprint-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec = "deb-dir:(hackage:applicative-extras):(darcs:http://src.seereason.com/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-attempt"
             , sourceSpec = "deb-dir:(hackage:attempt=0.3.0):(darcs:http://src.seereason.com/haskell-attempt-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-authenticate"
             , sourceSpec = "deb-dir:(hackage:authenticate):(darcs:" ++ repo ++ "/haskell-authenticate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bimap"
             , sourceSpec = "deb-dir:(hackage:bimap):(darcs:http://src.seereason.com/haskell-bimap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitset"
             , sourceSpec = "deb-dir:(hackage:bitset):(darcs:http://src.seereason.com/haskell-bitset-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-blaze-from-html"
            , sourceSpec = "deb-dir:(hackage:blaze-from-html):(darcs:http://src.seereason.com/haskell-blaze-from-html-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bytestring-trie"
             , sourceSpec = "deb-dir:(hackage:bytestring-trie):(darcs:http://src.seereason.com/haskell-bytestring-trie-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delcont"
             , sourceSpec = "deb-dir:(hackage:CC-delcont):(darcs:http://src.seereason.com/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-convertible-text"
             , sourceSpec = "deb-dir:(hackage:convertible-text):(darcs:http://src.seereason.com/haskell-convertible-text-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object-json"
             , sourceSpec = "deb-dir:(hackage:data-object-json):(darcs:http://src.seereason.com/haskell-data-object-json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object"
             , sourceSpec = "deb-dir:(hackage:data-object):(darcs:http://src.seereason.com/haskell-data-object-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors"
             , sourceSpec = "deb-dir:(hackage:digestive-functors=0.1.0.0):(darcs:http://src.seereason.com/haskell-digestive-functors-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors-happstack"
             , sourceSpec = "deb-dir:(hackage:digestive-functors-happstack):(darcs:http://src.seereason.com/haskell-digestive-functors-happstack-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec = "deb-dir:(hackage:formlets):(darcs:http://src.seereason.com/haskell-formlets-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-funsat"
             , sourceSpec = "deb-dir:(hackage:funsat):(darcs:http://src.seereason.com/haskell-funsat-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gd"
             , sourceSpec = "deb-dir:(hackage:gd):(darcs:http://src.seereason.com/haskell-gd-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-gnuplot"
             , sourceSpec = "deb-dir:(hackage:gnuplot):(darcs:http://src.seereason.com/haskell-gnuplot-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack"
             -- , sourceSpec = "deb-dir:(cd:happstack:darcs:" ++ happstackRepo ++ "):(darcs:" ++ repo ++ "/happstack-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack):(darcs:" ++ repo ++ "/happstack-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             -- , sourceSpec = "deb-dir:(cd:happstack-data:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-data-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack-data):(darcs:http://src.seereason.com/happstack-data-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-ixset"
             -- , sourceSpec = "deb-dir:(cd:happstack-ixset:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-ixset-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack-ixset):(darcs:http://src.seereason.com/happstack-ixset-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-server"
             -- , sourceSpec = "deb-dir:(cd:happstack-server:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-server-debian)"
             -- , sourceSpec = "deb-dir:(hackage:happstack-server):(darcs:http://src.seereason.com/happstack-server-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack-server):(darcs:" ++ repo ++ "/happstack-server-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-state"
             -- , sourceSpec = "deb-dir:(cd:happstack-state:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-state-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack-state):(darcs:http://src.seereason.com/happstack-state-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-util"
             -- , sourceSpec = "deb-dir:(cd:happstack-util:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-util-debian)"
             , sourceSpec = "deb-dir:(hackage:happstack-util):(darcs:http://src.seereason.com/happstack-util-debian)"
             , relaxInfo = [] }
    -- Depends on pandoc
    , Target { sourcePackageName = "haskell-safecopy"
             , sourceSpec = "deb-dir:(hackage:safecopy):(darcs:http://src.seereason.com/haskell-safecopy-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-heap"
             , sourceSpec="deb-dir:(hackage:heap):(darcs:http://src.seereason.com/heap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hoauth"
             -- , sourceSpec = "apt:sid:haskell-hoauth" -- no available versions?
             , sourceSpec = "deb-dir:(hackage:hoauth):(darcs:http://src.seereason.com/haskell-hoauth-debian)"
             , relaxInfo = [] }
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , Target { sourcePackageName = "haskell-hostname"
             , sourceSpec="deb-dir:(hackage:hostname):(darcs:http://src.seereason.com/haskell-hostname-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hpdf"
             , sourceSpec = "deb-dir:(hackage:HPDF):(darcs:http://src.seereason.com/haskell-hpdf-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsopenssl"
             , sourceSpec = "deb-dir:(hackage:HsOpenSSL):(darcs:http://src.seereason.com/haskell-hsopenssl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(hackage:i18n):(darcs:http://src.seereason.com/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-incremental-sat-solver"
             , sourceSpec = "deb-dir:(hackage:incremental-sat-solver):(darcs:http://src.seereason.com/haskell-incremental-sat-solver-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-instant-generics"
             , sourceSpec = "proc:deb-dir:(hackage:instant-generics):(darcs:http://src.seereason.com/haskell-instant-generics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-jsonb"
             , sourceSpec = "deb-dir:(hackage:JSONb):(darcs:http://src.seereason.com/haskell-jsonb-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-monadlib"
             , sourceSpec = "deb-dir:(hackage:monadLib):(darcs:http://src.seereason.com/haskell-monadlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-murmur-hash"
             , sourceSpec = "deb-dir:(hackage:murmur-hash):(darcs:http://src.seereason.com/haskell-murmur-hash-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-nano-hmac"
             , sourceSpec = "deb-dir:(hackage:nano-hmac):(darcs:http://src.seereason.com/haskell-nano-hmac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-openid"
             , sourceSpec = "deb-dir:(hackage:openid):(darcs:http://src.seereason.com/haskell-openid-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-operational"
             , sourceSpec = "deb-dir:(hackage:operational):(darcs:http://src.seereason.com/operational-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parse-dimacs"
             , sourceSpec = "deb-dir:(hackage:parse-dimacs):(darcs:http://src.seereason.com/parse-dimacs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pbkdf2"
             , sourceSpec = "deb-dir:(hackage:PBKDF2):(darcs:http://src.seereason.com/pbkdf2-debian)"

             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-permutation"
             , sourceSpec = "deb-dir:(hackage:permutation):(darcs:http://src.seereason.com/haskell-permutation)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-psqueue"
             , sourceSpec = "deb-dir:(hackage:PSQueue):(darcs:http://src.seereason.com/haskell-psqueue-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pwstore-purehaskell"
             , sourceSpec = "deb-dir:(hackage:pwstore-purehaskell):(darcs:http://src.seereason.com/haskell-pwstore-purehaskell-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(hackage:RJson):(darcs:http://src.seereason.com/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-sat"
             , sourceSpec = "deb-dir:(hackage:sat):(darcs:http://src.seereason.com/haskell-sat-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework-hunit"
             , sourceSpec="deb-dir:(hackage:test-framework-hunit):(darcs:http://src.seereason.com/haskell-test-framework-hunit-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework-quickcheck"
            , sourceSpec="deb-dir:(hackage:test-framework-quickcheck):(darcs:http://src.seereason.com/haskell-test-framework-quickcheck-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-test-framework"
             , sourceSpec="deb-dir:(hackage:test-framework):(darcs:http://src.seereason.com/haskell-test-framework-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-names"
             , sourceSpec="deb-dir:(hackage:unicode-names):(darcs:http://src.seereason.com/haskell-unicode-names-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-properties"
             , sourceSpec="deb-dir:(hackage:unicode-properties):(darcs:http://src.seereason.com/haskell-unicode-properties-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-utf8-prelude"
             , sourceSpec = "deb-dir:(hackage:utf8-prelude):(darcs:http://src.seereason.com/utf8-prelude-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-encodings"
             , sourceSpec = "deb-dir:(hackage:web-encodings):(darcs:http://src.seereason.com/haskell-web-encodings-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parseargs"
             , sourceSpec = "deb-dir:(hackage:parseargs):(darcs:http://src.seereason.com/haskell-parseargs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-th-lift"
             , sourceSpec = "deb-dir:(hackage:th-lift):(darcs:http://src.seereason.com/haskell-th-lift-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskell-src-meta"
             , sourceSpec = "deb-dir:(hackage:haskell-src-meta=0.4):(darcs:http://src.seereason.com/haskell-haskell-src-meta-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-jmacro"
             , sourceSpec = "deb-dir:(hackage:jmacro):(darcs:http://src.seereason.com/haskell-jmacro-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitmap"
             , sourceSpec = "deb-dir:(hackage:bitmap):(darcs:http://src.seereason.com/haskell-bitmap-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitmap-opengl"
             , sourceSpec = "deb-dir:(hackage:bitmap-opengl):(darcs:http://src.seereason.com/haskell-bitmap-opengl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(hackage:stb-image):(darcs:http://src.seereason.com/haskell-stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vacuum"
             , sourceSpec = "deb-dir:(hackage:vacuum):(darcs:http://src.seereason.com/haskell-vacuum-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vacuum-opengl"
             , sourceSpec = "deb-dir:(hackage:vacuum-opengl):(darcs:http://src.seereason.com/haskell-vacuum-opengl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rsa"
             , sourceSpec = "deb-dir:(hackage:RSA):(darcs:" ++ repo ++ "/haskell-rsa-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-aeson"

             , sourceSpec = "deb-dir:(hackage:aeson=0.3.2.8):(darcs:" ++ repo ++ "/haskell-aeson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hashable"
             , sourceSpec = "deb-dir:(hackage:hashable):(darcs:" ++ repo ++ "/haskell-hashable-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unordered-containers"
             , sourceSpec = "deb-dir:(hackage:unordered-containers):(darcs:" ++ repo ++ "/haskell-unordered-containers-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-blaze-textual"
             , sourceSpec = "deb-dir:(hackage:blaze-textual=0.1.0.0):(darcs:" ++ repo ++ "/haskell-blaze-textual-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http-enumerator"
             , sourceSpec = "deb-dir:(hackage:http-enumerator=0.6.5.4):(darcs:" ++ repo ++ "/haskell-http-enumerator-debian)"
             , relaxInfo = [] }
    ]