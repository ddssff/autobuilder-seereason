-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Targets 
    ( publicTargets
    , privateTargets
    ) where

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Debian.AutoBuilder.ParamClass (Target(..))

repo = "http://src.seereason.com"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs"
localRepo home = "file://" ++ home ++ "/darcs"

sid = "sid"

data GHCVersion = GHC6 | GHC7

-- |If you change this to GHC7 you may want to create a special
-- @~/.autobuilder@ to receive the results.  Do not upload the build
-- result to the server yet!
compiler = GHC7

-- |The targets with ghc7 specific fixes use this repo.  Once these
-- fixes are resolved, or the older compiler is retired, move these
-- back into the common repo.
ghcRepo =
    case compiler of
      GHC6 -> repo
      GHC7 -> repo {- ++ "/ghc7" -}
ghcPrivateRepo =
    case compiler of
      GHC6 -> privateRepo
      -- GHC7 -> "upload@src.seereason.com:/srv/darcs/ghc7"
      GHC7 -> privateRepo

happstackRepo = "http://patch-tag.com/r/mae/happstack"
--happstackRepo = repo ++ "/happstack"
localHappstackRepo home = localRepo home ++ "/happstack"

publicTargets home release =
    [ Target { sourcePackageName = "autobuilder"
             , sourceSpec = "darcs:http://src.seereason.com/autobuilder"
             , relaxInfo = [] }
    , Target { sourcePackageName = "bash-completion"
             , sourceSpec = "apt:" ++ sid ++ ":bash-completion"
             , relaxInfo = [] }
    , Target { sourcePackageName = "geneweb"
             , sourceSpec = "apt:sid:geneweb"
             , relaxInfo = [] }
    , Target { sourcePackageName = "darcs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/darcs/2.5.2/darcs-2.5.2.tar.gz:491b6ca01dec245a16112ad2c7e07dc1):(darcs:http://src.seereason.com/darcs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "ghc"
             , sourceSpec = "apt:sid:ghc"
             , relaxInfo = ["ghc"
                           ,"happy"
                           ,"alex"
                           ,"xsltproc"
                           ,"haskell-devscripts"
                           ,"debhelper"
                           ,"quilt"]
             }
{-    , Target { sourcePackageName = "gmp"
             , sourceSpec = "quilt:(apt:sid:gmp):(darcs:" ++ repo ++ "/gmp-quilt)"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haddock"
             , sourceSpec = "darcs:http://src.seereason.com/haddock-dummy"
             , relaxInfo = [] }
    , Target { sourcePackageName = "happy"
             , sourceSpec = "apt:" ++ sid ++ ":happy"
             , relaxInfo = ["happy"] }
    , Target { sourcePackageName = "haskell-aes"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/AES/0.2.7/AES-0.2.7.tar.gz:c40edee8615226c78139dd5ca864ad9e):(darcs:http://src.seereason.com/haskell-aes-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "agda"
             , sourceSpec = "apt:sid:agda"
             , relaxInfo = [] }
    , Target { sourcePackageName = "agda-bin"
             , sourceSpec = "apt:sid:agda-bin"
             , relaxInfo = [] }
    , Target { sourcePackageName = "agda-stdlib"
             , sourceSpec = "apt:sid:agda-stdlib"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-agi"
             , sourceSpec="darcs:http://src.seereason.com/haskell-agi"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-terminal"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/ansi-terminal/0.5.2/ansi-terminal-0.5.2.tar.gz:240b517c3775f1d5753c01ed52975ef1):(darcs:http://src.seereason.com/ansi-terminal-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ansi-wl-pprint"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/ansi-wl-pprint/0.5.1/ansi-wl-pprint-0.5.1.tar.gz:d427a18a5a071b8dbcdff28633f4b800):(darcs:http://src.seereason.com/ansi-wl-pprint-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-applicative-extras"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/applicative-extras/0.1.6/applicative-extras-0.1.6.tar.gz:9eee19f8bc916fe7b28345eb599c28fa):(darcs:http://src.seereason.com/applicative-extras-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = "darcs:http://src.seereason.com/archive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-asn1-data"
             -- , sourceSpec = "apt:sid:haskell-asn1-data" -- no available versions?
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/asn1-data/0.2.2/asn1-data-0.2.2.tar.gz:dfc412f4b1cff907e924dd65ce70c399):(darcs:http://src.seereason.com/haskell-asn1-data-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-attempt"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/attempt/0.3.0/attempt-0.3.0.tar.gz:051be218858e24ffd53b4e435f024611):(darcs:http://src.seereason.com/haskell-attempt-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-attoparsec-enumerator"
             , sourceSpec = "apt:sid:haskell-attoparsec-enumerator"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/attoparsec-enumerator/0.2/attoparsec-enumerator-0.2.tar.gz:ec79f184f49109cc7ec882dfbb9b836e):(darcs:http://src.seereason.com/haskell-attoparsec-enumerator-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-attoparsec"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/attoparsec/0.8.1.1/attoparsec-0.8.1.1.tar.gz:8948e39002acb823fae3af9cde6983e8):(darcs:http://src.seereason.com/haskell-attoparsec-debian)"
             , sourceSpec = "apt:sid:haskell-attoparsec"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-authenticate"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/authenticate/0.8.0/authenticate-0.8.0.tar.gz:42d0ae309ee3acb86bb4b05d4c34854e):(darcs:http://src.seereason.com/haskell-authenticate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-base64-bytestring"
             , sourceSpec = "apt:sid:haskell-base64-bytestring"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/base64-bytestring/0.1.0.1/base64-bytestring-0.1.0.1.tar.gz:f0f9cb5a29a718bcca49e2c284378d62):(darcs:http://src.seereason.com/haskell-base64-bytestring-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bimap"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bimap/0.2.4/bimap-0.2.4.tar.gz:f6b79bff5741e709f1536df411aab53d):(darcs:http://src.seereason.com/haskell-bimap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-binary"
             , sourceSpec = "quilt:(apt:sid:haskell-binary):(darcs:http://src.seereason.com/haskell-binary-quilt)"
             -- , sourceSpec = "apt:sid:haskell-binary"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bitset"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bitset/1.0/bitset-1.0.tar.gz:466eb0fd8a92b16e705a219f0d01a54c):(darcs:http://src.seereason.com/haskell-bitset-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-blaze-builder"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-builder/0.2.1.4/blaze-builder-0.2.1.4.tar.gz:fefd5b1a72104c835034287ca4591460):(darcs:http://src.seereason.com/haskell-blaze-builder-debian)"
             , sourceSpec = "apt:sid:haskell-blaze-builder"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-blaze-html"
            , sourceSpec = "apt:sid:haskell-blaze-html"
            -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-html/0.4.1.0/blaze-html-0.4.1.0.tar.gz:6aa538867ee7ede93f0377259766137e):(darcs:http://src.seereason.com/haskell-blaze-html-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-blaze-builder-enumerator"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-builder-enumerator/0.2.0.1/blaze-builder-enumerator-0.2.0.1.tar.gz:3d21f0bda0606d7639ade71dc5bcc69c):(darcs:http://src.seereason.com/haskell-blaze-builder-enumerator-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-blaze-from-html"
            , sourceSpec =
                "deb-dir:(uri:http://hackage.haskell.org/packages/archive/blaze-from-html/0.3.1.0/blaze-from-html-0.3.1.0.tar.gz:8a20c2b8ead12ddfe65a8fb3cb2b7236):(darcs:http://src.seereason.com/haskell-blaze-from-html-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bytestring-nums"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bytestring-nums/0.3.2/bytestring-nums-0.3.2.tar.gz:f54377b9915a9074d8dc1a07f4f17298):(darcs:http://src.seereason.com/haskell-bytestring-nums-debian)"
             , sourceSpec = "apt:sid:haskell-bytestring-nums"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bytestring-trie"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bytestring-trie/0.2.2/bytestring-trie-0.2.2.tar.gz:87055596ec7c40270e3366f4ab5a0e55):(darcs:http://src.seereason.com/haskell-bytestring-trie-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-bzlib"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/bzlib/0.5.0.0/bzlib-0.5.0.0.tar.gz:ab594aaf9998ed602f8b23dd25199e19):(darcs:http://src.seereason.com/haskell-bzlib-debian)"
             , sourceSpec = "quilt:(apt:sid:haskell-bzlib):(darcs:http://src.seereason.com/haskell-bzlib-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cc-delconto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/CC-delcont/0.2/CC-delcont-0.2.tar.gz:e52149fca9bf76330a7c159917152790):(darcs:http://src.seereason.com/CC-delcont-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cereal"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cereal/0.3.3.0/cereal-0.3.3.0.tar.gz:70d24b61194880df3ec798ad89a21a7b):(darcs:http://src.seereason.com/haskell-cereal-debian)"
             -- , sourceSpec = "apt:sid:haskell-cereal" -- too old
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-certificate"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/certificate/0.3.2/certificate-0.3.2.tar.gz:17a2c881188033b1f1e57a852e250daf):(darcs:http://src.seereason.com/haskell-certificate-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cgi"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz:4092efaf00ac329b9771879f57a95323):(darcs:http://src.seereason.com/haskell-cgi-debian)"
             -- , sourceSpec = "apt:sid:haskell-cgi"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-colour"
             -- , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/colour/2.3.1/colour-2.3.1.tar.gz:5edced36d4c27393ae1ce1389eeb25ad):(darcs:http://src.seereason.com/haskell-colour-debian)"
             , sourceSpec="apt:sid:haskell-colour"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-consumer"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-consumer"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-convertible-text"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/convertible-text/0.3.0.6/convertible-text-0.3.0.6.tar.gz:8335ddd1155bcdb63be82e53c97b342d):(darcs:http://src.seereason.com/haskell-convertible-text-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cpphs"
             , sourceSpec = "apt:sid:cpphs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-crypto"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Crypto/4.2.3/Crypto-4.2.3.tar.gz:28b8556f1ea5b85e0d2a8d0d030c0f69):(darcs:http://src.seereason.com/crypto-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-crypto-api"
             , sourceSpec = "apt:sid:haskell-crypto-api"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/crypto-api/0.2/crypto-api-0.2.tar.gz:c7b7aa4459549ebc9d0936a7efdde87f):(darcs:http://src.seereason.com/haskell-crypto-api-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cryptocipher"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cryptocipher/0.2/cryptocipher-0.2.tar.gz:1b2353ae52c16463d841c8c76fea2b47):(darcs:http://src.seereason.com/haskell-cryptocipher-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-cryptohash"
             , sourceSpec = "apt:sid:haskell-cryptohash"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/cryptohash/0.6.1/cryptohash-0.6.1.tar.gz:40139c278620007fe7c7840050050fa3):(darcs:http://src.seereason.com/haskell-cryptohash-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-curl"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/curl/1.3.5/curl-1.3.5.tar.gz:0b08065ca25cead1e2b374958dc69818):(darcs:http://src.seereason.com/haskell-curl-debian)"
             , sourceSpec = "apt:sid:haskell-curl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-accessor"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor/0.2.1.2/data-accessor-0.2.1.2.tar.gz:8eabca77e2202dc0735b45ca7dea9175):(darcs:" ++ ghcRepo ++ "/haskell-data-accessor-debian)"
             , sourceSpec = "apt:sid:haskell-data-accessor"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-accessor-template"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-accessor-template/0.2.1.2/data-accessor-template-0.2.1.2.tar.gz:0feb98757d889bbc9fbbe0379cdb3c05):(darcs:http://src.seereason.com/haskell-data-accessor-template-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-default"
             , sourceSpec = "apt:sid:haskell-data-default"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-default/0.2/data-default-0.2.tar.gz:cfbbaaa4fa49a0c4c1b52c5d51258ae7):(darcs:http://src.seereason.com/haskell-data-default-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-dataenc"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-dataenc"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object-json"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-object-json/0.3.1.4/data-object-json-0.3.1.4.tar.gz:8077d5a05b18e711d5a5bb4f3c2d1881):(darcs:http://src.seereason.com/haskell-data-object-json-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-data-object"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/data-object/0.3.1.5/data-object-0.3.1.5.tar.gz:9580477eadd2e2bfdace7228c0a24cda):(darcs:http://src.seereason.com/haskell-data-object-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = "darcs:http://src.seereason.com/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-debian-repo"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian"
             , sourceSpec = "apt:sid:haskell-debian"
             -- , sourceSpec = "darcs:http://src.seereason.com/haskell-debian"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-deepseq"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-deepseq"
             , relaxInfo = [] }
    -- Required by the darcs 2.3.0-3 in sid.
    , Target { sourcePackageName = "haskell-devscripts"
             , sourceSpec = "apt:sid:haskell-devscripts"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-devscripts):(darcs:http://src.seereason.com/haskell-devscripts-quilt)"
             -- Experimental version
             --, sourceSpec = "quilt:(darcs:http://darcs.debian.org/pkg-haskell/haskell-devscripts):(darcs:http://src.seereason.com/haskell-devscripts-quilt)"
             , relaxInfo = ["hscolour"] }
    , Target { sourcePackageName = "haskell-digest"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-digest"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digest/0.0.0.5/digest-0.0.0.5.tar.gz:ba60cc9d1ad6d0795ad84390976699d1):(darcs:http://src.seereason.com/debian/haskell-digest-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/digestive-functors-hsp"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digestive-functors/0.1.0.0/digestive-functors-0.1.0.0.tar.gz:ca2d55dc58ef8b5a19990f944a0300d4):(darcs:http://src.seereason.com/haskell-digestive-functors-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors-happstack"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/digestive-functors-happstack/0.1.0.0/digestive-functors-happstack-0.1.0.0.tar.gz:791d790db823894b49aa5bf3c1710f0e):(darcs:http://src.seereason.com/haskell-digestive-functors-happstack-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-dlist"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/dlist/0.5/dlist-0.5.tar.gz:54f6c18ff689b7ef54e3229dc4a94107):(darcs:http://src.seereason.com/haskell-dlist-debian)"
             , sourceSpec = "apt:sid:haskell-dlist"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-edison-api"
             , sourceSpec="apt:sid:haskell-edison-api"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-edison-core"
             , sourceSpec="apt:sid:haskell-edison-core"
             -- , sourceSpec="quilt:(apt:" ++ sid ++ ":haskell-edison-core):(darcs:http://src.seereason.com/edison-core-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-enumerator"
             , sourceSpec = "apt:sid:haskell-enumerator"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/enumerator/0.4.6/enumerator-0.4.6.tar.gz:660f11852f47c8a93518c0163a9c6397):(darcs:http://src.seereason.com/haskell-enumerator-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-erf"
             -- , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/erf/1.0.0.0/erf-1.0.0.0.tar.gz:1bdb56838565abfaa7d7ab6e2870ddaa):(darcs:http://src.seereason.com/haskell-erf-debian)"
             , sourceSpec = "apt:sid:haskell-erf"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-extra"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-extra"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-failure"
             , sourceSpec = "apt:sid:haskell-failure"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/failure/0.1.0/failure-0.1.0.tar.gz:7c14ad9271ec9f8f87b4fdbf63291c44):(darcs:http://src.seereason.com/haskell-failure-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-feed"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/feed/0.3.8/feed-0.3.8.tar.gz:0df089dfc2e2b0726ee5d66286d98f5f):(darcs:http://src.seereason.com/debian/haskell-feed-debian)"
             , sourceSpec = "apt:sid:haskell-feed"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-fgl"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-fgl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = "darcs:http://src.seereason.com/formlets-hsp"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-formlets"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/formlets/0.7.3/formlets-0.7.3.tar.gz:456acbc7eb4922dd991e50611f917d77):(darcs:http://src.seereason.com/haskell-formlets-debian)"
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
    , Target { sourcePackageName = "haskell-happstack-authenticate"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-authenticate"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-data"
             -- , sourceSpec = "deb-dir:(cd:happstack-data:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-data-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz:be72c4c11d1317bf52c80782eac28a2d):(darcs:http://src.seereason.com/happstack-data-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstackdotcom"
             , sourceSpec = "darcs:http://patch-tag.com/r/stepcut/happstackDotCom"
             -- , sourceSpec = "darcs:" ++ localRepo home ++ "/happstackDotCom"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstackdotcom-doc"
             , sourceSpec = "darcs:http://src.seereason.com/happstackDotCom-doc"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-facebook"
             , sourceSpec = "darcs:http://src.seereason.com/happstack-facebook"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-hsp"
             , sourceSpec = "deb-dir:(cd:happstack-hsp:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-hsp-debian)"
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
    , Target { sourcePackageName = "haskell-harp"
             -- , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/harp):(darcs:http://src.seereason.com/harp-debian)"
             , sourceSpec = "apt:sid:haskell-harp"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-safecopy"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/safecopy/0.5.1/safecopy-0.5.1.tar.gz:8ad2cc63e1d2bbda9e3111c359b2ce9f):(darcs:http://src.seereason.com/haskell-safecopy-debian)"
             , relaxInfo = []
             }
    , Target{ sourcePackageName = "haskell-acid-state"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/acid-state/0.4.1/acid-state-0.4.1.tar.gz:29609cac51c1c4bf8d5e460afd980e87):(darcs:http://src.seereason.com/haskell-acid-state-debian)"
            , relaxInfo = []
            }
    , Target { sourcePackageName = "haskell-hashed-storage"
             , sourceSpec = "apt:sid:haskell-hashed-storage"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-hashed-storage):(darcs:http://src.seereason.com/hashed-storage-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskeline" -- Required by darcs
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-haskeline):(darcs:" ++ repo ++ "/haskell-haskeline-quilt)"
             , sourceSpec = "apt:sid:haskell-haskeline"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-haskell-src"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-haskell-src"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-heap"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/heap/1.0.0/heap-1.0.0.tar.gz:7a650f3803da5c0ea5c865d0ef1c0857):(darcs:http://src.seereason.com/heap-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             -- , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjavascript"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HJavaScript/0.4.6/HJavaScript-0.4.6.tar.gz:440ee4f0bcffe6e60ee0ce02dd3922cf):(darcs:http://src.seereason.com/hjavascript-debian)"
             , sourceSpec = "apt:sid:haskell-hjavascript"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hjscript"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HJScript/0.5.0/HJScript-0.5.0.tar.gz:3eb1e67900e41c9e93528ae15ca27028):(darcs:http://src.seereason.com/hjscript-debian)"
             , sourceSpec = "apt:sid:haskell-hjscript"
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
    , Target { sourcePackageName = "haskell-hsemail"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsemail/1.6/hsemail-1.6.tar.gz:fa56a870e06435ccea8da0ecebdbf689):(darcs:http://src.seereason.com/debian/haskell-hsemail-debian)"
             , sourceSpec = "apt:sid:haskell-hsemail"
             , relaxInfo = [] }

    , Target { sourcePackageName = "hslogger"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hslogger/1.0.10/hslogger-1.0.10.tar.gz:f65a5326d28f9cdad6887a32525d70dc):(darcs:http://src.seereason.com/hslogger-debian)"
             , sourceSpec = "apt:sid:hslogger"

             -- No profiling libraries in sid version
             -- , sourcePackageName = "hslogger"
             -- , sourceSpec = "apt:" ++ sid ++ ":hslogger"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsopenssl"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HsOpenSSL/0.8.0.2/HsOpenSSL-0.8.0.2.tar.gz:5ed5a3b42a73e5cab5ca9c45539d7a7a):(darcs:http://src.seereason.com/haskell-hsopenssl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsopenssl"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HsOpenSSL/0.8.0.2/HsOpenSSL-0.8.0.2.tar.gz:5ed5a3b42a73e5cab5ca9c45539d7a7a):(darcs:http://src.seereason.com/haskell-hsopenssl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsp"
             -- , sourceSpec = "apt:sid:haskell-hsp"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsp/0.6.0/hsp-0.6.0.tar.gz:c80d48e6706a4d1d4608f63549069c36):(darcs:http://src.seereason.com/hsp-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hstringtemplate"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/HStringTemplate/0.4.3/HStringTemplate-0.4.3.tar.gz:57139f6695f1c268ed38c34336191636):(darcs:http://src.seereason.com/HStringTemplate-debian)"
             , sourceSpec = "apt:sid:haskell-hstringtemplate"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx"
             -- , sourceSpec = "deb-dir:(darcs:http://code.haskell.org/HSP/hsx):(darcs:" ++ ghcRepo ++ "/hsx-debian)"
             -- , sourceSpec = "deb-dir:(darcs:" ++ ghcRepo ++ "/hsx):(darcs:" ++ ghcRepo ++ "/hsx-debian)"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/hsx/0.9.0/hsx-0.9.0.tar.gz:d82f4ae3fcc08b4acdb001f7b189c13a):(darcs:http://src.seereason.com/hsx-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html-entities"
             , sourceSpec = "darcs:http://src.seereason.com/html-entities"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-html"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-html"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http-enumerator"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/http-enumerator/0.3.1/http-enumerator-0.3.1.tar.gz:bb3f0cb522923965bcc43f0db6c08ae3):(darcs:http://src.seereason.com/haskell-http-enumerator-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-http"
             , sourceSpec = "apt:sid:haskell-http"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hunit"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-hunit"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-i18n"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/i18n/0.3/i18n-0.3.tar.gz:e59445b4ad743ab77c61a281cf942bbf):(darcs:http://src.seereason.com/i18n-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/iconv"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-incremental-sat-solver"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/incremental-sat-solver/0.1.7/incremental-sat-solver-0.1.7.tar.gz:3d5f3d0bff3a92f4207631fbe9b32c36):(darcs:http://src.seereason.com/haskell-incremental-sat-solver-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-instant-generics"
             , sourceSpec = "proc:deb-dir:(uri:http://hackage.haskell.org/packages/archive/instant-generics/0.2.1/instant-generics-0.2.1.tar.gz:b6a37bfa18b054800ca8c5ce5dbe4e87):(darcs:http://src.seereason.com/haskell-instant-generics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-irc"
             , sourceSpec = "apt:sid:haskell-irc"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-jsonb"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/JSONb/1.0.4/JSONb-1.0.4.tar.gz:071e186dcab3fe3ab262903895f5f670):(darcs:http://src.seereason.com/haskell-jsonb-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-json"
             , sourceSpec = "apt:sid:haskell-json"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/json/0.4.3/json-0.4.3.tar.gz:1af33c67594f69048b69d4aeafeea03e):(darcs:http://src.seereason.com/json-debian)"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-json=0.4.4-1):(darcs:" ++ repo ++ "/haskell-json-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-largeword"
             , sourceSpec = "apt:sid:haskell-largeword"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-logic"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-logic"
             , relaxInfo = [] }
    , Target { sourcePackageName = "magic-haskell"
             , sourceSpec = "apt:sid:magic-haskell"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/magic/1.0.8/magic-1.0.8.tar.gz:e81c493fe185431a5b70d4855ed4b87f):(darcs:http://src.seereason.com/magic-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-maybet"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/MaybeT/0.1.2/MaybeT-0.1.2.tar.gz:9864a3f34151217004f8c968fda5b427):(darcs:http://src.seereason.com/MaybeT-debian)"
             , sourceSpec = "apt:sid:haskell-maybet"
             , relaxInfo = [] }               
    , Target { sourcePackageName = "haskell-mime"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-mime"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mmap0.4"
             , sourceSpec = "apt:sid:haskell-mmap0.4"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mmap"
             , sourceSpec = "apt:sid:haskell-mmap"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-monadcatchio-mtl"
             , sourceSpec = "apt:sid:haskell-monadcatchio-mtl"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/MonadCatchIO-mtl/0.3.0.1/MonadCatchIO-mtl-0.3.0.1.tar.gz:53f56fece758b15eff950f9c9796fbdf):(darcs:http://src.seereason.com/haskell-monadcatchio-mtl-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-monadlib"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/monadLib/3.6.1/monadLib-3.6.1.tar.gz:09ed56db9cd49af9f40789a0c7102759):(darcs:http://src.seereason.com/haskell-monadlib-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mtl"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/mtl/2.0.0.0/mtl-2.0.0.0.tar.gz:068e6714badedd839808ce8d7dd80abe):(darcs:http://src.seereason.com/haskell-mtl-debian)"
             , sourceSpec = "apt:sid:haskell-mtl"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-murmur-hash"
             , sourceSpec =
                  "deb-dir:(uri:http://hackage.haskell.org/packages/archive/murmur-hash/0.1/murmur-hash-0.1.tar.gz:9bab434a87a7d611bc21e484977ce88f):(darcs:http://src.seereason.com/haskell-murmur-hash-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mwc-random"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/mwc-random/0.8.0.2/mwc-random-0.8.0.2.tar.gz:5d95ec60ac4dd4097a4ce23f0e29254a):(darcs:http://src.seereason.com/haskell-mwc-random-debian)"
             , sourceSpec = "apt:sid:haskell-mwc-random"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-nano-hmac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/nano-hmac/0.2.0/nano-hmac-0.2.0.tar.gz:96e93a3a51be4d659f07a241dc171ff4):(darcs:http://src.seereason.com/haskell-nano-hmac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-network"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-network"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-openid"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/openid/0.1.4.6/openid-0.1.4.6.tar.gz:1f121c2d0dc9508203db37e476586de8):(darcs:http://src.seereason.com/haskell-openid-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-operational"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/operational/0.2.0.1/operational-0.2.0.1.tar.gz:40b3fe5f20a9c49df1da55d86872064a):(darcs:http://src.seereason.com/operational-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pandoc"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pandoc/1.5.1.1/pandoc-1.5.1.1.tar.gz:bfccc042ae0cf0901bbca1f87748f969):(darcs:http://src.seereason.com/haskell-pandoc-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parallel"
             , sourceSpec = 
                 case compiler of
                   GHC6 -> "apt:" ++ sid ++ ":haskell-parallel"
                   GHC7 -> "apt:" ++ sid ++ ":haskell-parallel"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parsec2"
             , sourceSpec = "apt:sid:haskell-parsec2"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-parsec2):(darcs:http://src.seereason.com/haskell-parsec2-quilt)"
             , relaxInfo = [] }
    -- Binary packages: libghc6-quickcheck2-dev, libghc6-quickcheck2-prof, libghc6-quickcheck2-doc
    , Target { sourcePackageName = "haskell-parsec"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-parsec"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parse-dimacs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/parse-dimacs/1.2/parse-dimacs-1.2.tar.gz:2a3bd8f6b0dd638567c172ef16e3e191):(darcs:http://src.seereason.com/parse-dimacs-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pbkdf2"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/PBKDF2/0.3.1/PBKDF2-0.3.1.tar.gz:46a3109e272d296fefea4c1d2015173a):(darcs:http://src.seereason.com/pbkdf2-debian)"

             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pcre-light"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-pcre-light"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-permutation"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/permutation/0.4.1/permutation-0.4.1.tar.gz:a9e0b6231d7a085719188406f59ab1aa):(darcs:http://src.seereason.com/haskell-permutation)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-polyparse"
             , sourceSpec = "apt:sid:haskell-polyparse"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-primitive"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/primitive/0.3.1/primitive-0.3.1.tar.gz:9f3a9ecd184a1e1ec5980c66f63e6187):(darcs:http://src.seereason.com/haskell-primitive-debian)"
             , sourceSpec = "apt:sid:haskell-primitive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-proplogic"
             , sourceSpec="deb-dir:(uri:http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz:e2fb3445dd16d435e81d7630d7f78c01):(darcs:http://src.seereason.com/haskell-proplogic-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-psqueue"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/PSQueue/1.1/PSQueue-1.1.tar.gz:43649b94d88fe103233e6897a014a89f):(darcs:http://src.seereason.com/haskell-psqueue-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pwstore-purehaskell"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/pwstore-purehaskell/2.1/pwstore-purehaskell-2.1.tar.gz:083cc7b79c4b1ad26819a08bbe120f81):(darcs:http://src.seereason.com/haskell-pwstore-purehaskell-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-quickcheck"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-quickcheck"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-quickcheck1"
             , sourceSpec = "apt:sid:haskell-quickcheck1"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-quickcheck1):(darcs:http://src.seereason.com/haskell-quickcheck1-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-base"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-regex-base"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-compat"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-regex-compat"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-posix"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-regex-posix"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-regex-tdfa"
             , sourceSpec = "apt:sid:haskell-regex-tdfa"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-regex-tdfa):(darcs:http://src.seereason.com/haskell-regex-tdfa-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-revision"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-revision"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-rjson"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/RJson/0.3.5/RJson-0.3.5.tar.gz:e69c34b295e067c169a15fc5327a9dd9):(darcs:http://src.seereason.com/RJson-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-safe"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-safe=0.2-2):(darcs:" ++ repo ++ "/haskell-safe-quilt)"
             , sourceSpec = "apt:sid:haskell-safe"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-sat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/sat/1.1.1/sat-1.1.1.tar.gz:5a974083ef008b32720b617fe5fb30a2):(darcs:http://src.seereason.com/haskell-sat-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-semigroups"
             , sourceSpec = "apt:sid:haskell-semigroups"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-sendfile"
             -- , sourceSpec = "apt:sid:haskell-sendfile"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/sendfile/0.7.2/sendfile-0.7.2.tar.gz:7e59a3c47ead59ad19c72ee1fcef9fa6):(darcs:http://src.seereason.com/haskell-sendfile-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-sha"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/SHA/1.4.1.3/SHA-1.4.1.3.tar.gz:6adbe05bfaf4416c1a7e1ac5e999811e):(darcs:" ++ repo ++ "/haskell-sha-debian)"
             , sourceSpec = "apt:sid:haskell-sha"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-smtpclient"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/SMTPClient/1.0.2/SMTPClient-1.0.2.tar.gz:59b474179c301a08af45c99f85fca224):(darcs:http://src.seereason.com/haskell-smtpclient-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-src-exts"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-src-exts"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-stb-image"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/stb-image/0.1.1/stb-image-0.1.1.tar.gz:9e8ac1305c60e13d04359744976e402a):(darcs:http://src.seereason.com/stb-image-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-stm"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-stm"
             , relaxInfo = [] }
    -- Patch to add the bareAttr function and remove the custom show
    -- instance Specify a particular version of xhtml so we can
    -- perhaps eliminate this the next time the upstream package is
    -- revved.
    , Target { sourcePackageName = "haskell-strict-concurrency"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/strict-concurrency/0.2.4.1/strict-concurrency-0.2.4.1.tar.gz:1fea3949811b465b79d8790dd06401e9):(darcs:http://src.seereason.com/haskell-strict-concurrency-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-syb"

             -- , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/syb/0.2.1/syb-0.2.1.tar.gz:f3c0ada9efc0cbb5e1ff219497a0628b):(darcs:http://src.seereason.com/haskell-syb-debian)"
             , sourceSpec="apt:sid:haskell-syb"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-syb-with-class):(darcs:http://src.seereason.com/haskell-syb-with-class-quilt)"
             , sourceSpec = "apt:sid:haskell-syb-with-class"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-syb-with-class-instances-text"
             -- , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/syb-with-class-instances-text):(darcs:http://src.seereason.com/syb-with-class-instances-text-debian)"
             , sourceSpec = "apt:sid:haskell-syb-with-class-instances-text"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-tagged"
             , sourceSpec = "apt:sid:haskell-tagged"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/tagged/0.1.1/tagged-0.1.1.tar.gz:ed9ddfd0d12dfaf136788da8e32c08f8):(darcs:http://src.seereason.com/haskell-tagged-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-tagsoup"
            -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/tagsoup/0.11.1/tagsoup-0.11.1.tar.gz:5469453026dff80cd515ba163c41ecfa):(darcs:http://src.seereason.com/haskell-tagsoup-debian)"
            , sourceSpec = "apt:sid:haskell-tagsoup"
            , relaxInfo = [] }
{-
    , Target { sourcePackageName = "haskell-tagsoup"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-tagsoup"
             , relaxInfo = []
             }
-}
    , Target { sourcePackageName = "haskell-tar"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/tar/0.3.1.0/tar-0.3.1.0.tar.gz:10b54856495f9ef0a42aa270085c7d5e):(darcs:http://src.seereason.com/haskell-tar-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-terminfo"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-terminfo"
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
    , Target { sourcePackageName = "haskell-texmath"
             , sourceSpec = "apt:sid:haskell-texmath"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-texmath):(darcs:http://src.seereason.com/haskell-texmath-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-text"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/text/0.11.0.5/text-0.11.0.5.tar.gz:516a6813a6b9061fabdc9f0c3bbb1592):(darcs:http://src.seereason.com/haskell-text-debian)"
             , sourceSpec = "apt:sid:haskell-text"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-tls"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/tls/0.3/tls-0.3.tar.gz:066b7615916243ad4b834e362dce8542):(darcs:http://src.seereason.com/haskell-tls-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-transformers"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/transformers/0.2.2.0/transformers-0.2.2.0.tar.gz:3470ac66116900cd1ba84d3744474e49):(darcs:" ++ repo ++ "/haskell-transformers-debian)"
             , sourceSpec = "apt:sid:haskell-transformers"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-names"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/unicode-names/3.2.0.0/unicode-names-3.2.0.0.tar.gz:0441831bb24b7e891668df5db96395c5):(darcs:http://src.seereason.com/haskell-unicode-names-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unicode-properties"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/unicode-properties/3.2.0.0/unicode-properties-3.2.0.0.tar.gz:efdb2a0021c328f23cc7026d31b23497):(darcs:http://src.seereason.com/haskell-unicode-properties-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uniplate"
             , sourceSpec="quilt:(apt:sid:haskell-uniplate):(darcs:http://src.seereason.com/haskell-uniplate-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unix-compat"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/unix-compat/0.1.2.1/unix-compat-0.1.2.1.tar.gz:6ecfc3922fce2e96922af3a636b061f9):(darcs:http://src.seereason.com/haskell-unix-compat-debian)"
             , relaxInfo = [] }

    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-utf8-prelude"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utf8-prelude/0.1.6/utf8-prelude-0.1.6.tar.gz:582665f0f8fde8e8c8528487f325c10d):(darcs:http://src.seereason.com/utf8-prelude-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-utf8-string"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utf8-string/0.3.6/utf8-string-0.3.6.tar.gz:be8c5ef52a0824babdc89d60c1e9b600):(darcs:http://src.seereason.com/utf8-string-debian)"
             , sourceSpec = "apt:sid:haskell-utf8-string"
             , relaxInfo = ["hscolour", "cpphs"] }
    , Target { sourcePackageName = "haskell-utility-ht"
             , sourceSpec = "apt:sid:haskell-utility-ht"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/utility-ht/0.0.5.1/utility-ht-0.0.5.1.tar.gz:98dcb042f404378d9071fc6344703386):(darcs:http://src.seereason.com/haskell-utility-ht-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vector"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/vector/0.7.0.1/vector-0.7.0.1.tar.gz:b0cfeab898384d960f0ad7393e5f955c):(darcs:http://src.seereason.com/haskell-vector-debian)"
             , sourceSpec = "apt:sid:haskell-vector"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-wai"
             -- , sourceSpec = "apt:sid:haskell-wai" - sid version is 0.2.2.1-1, too old
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/wai/0.3.1/wai-0.3.1.tar.gz:5a777cf08713a55818955ec4d0748622):(darcs:http://src.seereason.com/haskell-wai-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-encodings"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/web-encodings/0.3.0.6/web-encodings-0.3.0.6.tar.gz:fa8dcbc24b4c4aa8e481107017609d04):(darcs:http://src.seereason.com/haskell-web-encodings-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-parseargs"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/parseargs/0.1.3.2/parseargs-0.1.3.2.tar.gz:0b2693002b4dcc45b90005927818b0a5):(darcs:http://src.seereason.com/haskell-parseargs-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-th-lift"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/th-lift/0.5.3/th-lift-0.5.3.tar.gz:eaf567442887bd93c374e78fede6bf50):(darcs:http://src.seereason.com/haskell-th-lift-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-haskell-src-meta"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/haskell-src-meta/0.4/haskell-src-meta-0.4.tar.gz:d4ea030e9d95ffb672737024b5df8e02):(darcs:http://src.seereason.com/haskell-haskell-src-meta-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-jmacro"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/jmacro/0.5/jmacro-0.5.tar.gz:103fb0eb730622e6e4d7e611734966a1):(darcs:http://src.seereason.com/haskell-jmacro-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-web-routes-happstack"
             , sourceSpec = "cd:web-routes-happstack:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-mtl"
             , sourceSpec = "cd:web-routes-mtl:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-web-routes"
             , sourceSpec = "cd:web-routes:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-hsp"
             , sourceSpec = "cd:web-routes-hsp:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-th"
             , sourceSpec = "cd:web-routes-th:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-xhtml"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-xhtml"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-xml"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-xml"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/xml/1.3.4/xml-1.3.4.tar.gz:841c3a36a0bfb2e46f88448ecc227cad):(darcs:http://src.seereason.com/debian/haskell-xml-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-xss-sanitize"
            , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/xss-sanitize/0.2.6/xss-sanitize-0.2.6.tar.gz:ebd120843e10b31c71009b022fab233b):(darcs:http://src.seereason.com/haskell-xss-sanitize-debian)"
            , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zip-archive"
             , sourceSpec = "apt:sid:haskell-zip-archive"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-zip-archive):(darcs:http://src.seereason.com/haskell-zip-archive-quilt)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zlib-bindings"
             , sourceSpec = "apt:sid:haskell-zlib-bindings"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/zlib-bindings/0.0.0/zlib-bindings-0.0.0.tar.gz:ced9be9c31f54ad848f58babf1ca2190):(darcs:http://src.seereason.com/haskell-zlib-bindings-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-zlib"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-zlib"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haxml"
             , sourceSpec = "apt:sid:haxml"
             , relaxInfo = [] }
    , Target { sourcePackageName = "hscolour"
             -- , sourceSpec = "quilt:(apt:" ++ sid ++ ":hscolour):(darcs:http://src.seereason.com/hscolour-quilt)"
             , sourceSpec = "apt:sid:hscolour"
             , relaxInfo = ["hscolour"] }
    , Target { sourcePackageName = "html-xml-utils"
           , sourceSpec = "apt:" ++ sid ++ ":html-xml-utils"
           , relaxInfo = [] }
    -- This target fails during an arch only build, because it has no architecture dependent files.
    , Target { sourcePackageName = "seereason-keyring"
             , sourceSpec = "darcs:http://src.seereason.com/seereason-keyring"
             , relaxInfo = [] }
    , Target { sourcePackageName = "tinymce"
             , sourceSpec="apt:" ++ sid ++ ":tinymce"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    , Target { sourcePackageName = "wordpress"
             , sourceSpec="apt:" ++ sid ++ ":wordpress"
             , relaxInfo = [] }
    ] ++
    case compiler of
      GHC6 -> [ Target { sourcePackageName = "haskell-time-extras"
                       , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/time-extras/1.1.4/time-extras-1.1.4.tar.gz:2c7cfb8e661c74d9c13e0ca6a425876f):(darcs:http://src.seereason.com/haskell-time-extras-debian)"
                       , relaxInfo = [] } ]
      GHC7 -> []

--    , Target { sourcePackageName = "eclipse-clp"
--             , sourceSpec = "deb-dir:(uri:http://eclipseclp.org/Distribution/6.0_160/src/eclipse_src.tgz:75d074bf0ee66948e6afd3b69e51e81e):(darcs:http://src.seereason.com/eclipse-clp-debian)"
--             , relaxInfo = [] }
--  , Target { sourcePackageName = "haskell-binary"
--             , sourceSpec = "quilt:(apt:" ++ sid ++ ":haskell-binary):(darcs:http://src.seereason.com/haskell-binary-quilt)"
--             , relaxInfo = [] } -}
--    , Target { sourcePackageName = "haskell-formlets"
--             , sourceSpec = "darcs:http://src.seereason.com/formlets"
--             , relaxInfo = [] }
--    , Target { sourcePackageName="haskell-logict"
--             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/logict/0.4/logict-0.4.tar.gz:39eeb4aa1d7a67b1c4865f01ca417b7d):(darcs:http://src.seereason.com/debian/haskell-logict-debian)"
--             , relaxInfo = [] }
--    , Target { sourcePackageName = "haskell-special-functors"
--      , sourceSpec =
--      "deb-dir:(uri:http://hackage.haskell.org/packages/archive/special-functors/1.0/special-functors-1.0.tar.gz:4547f0a1b4146d3621bcc95b11148939):(darcs:http://src.seereason.com/haskell-special-functors-debian)"
--      , relaxInfo = [] }
--    , Target { sourcePackageName = "tptp"
--             , sourceSpec = "deb-dir:(uri:http://www.cs.miami.edu/~tptp/TPTP/Distribution/TPTP-v4.1.0.tgz:3cffa92b1def9b8b9865f65d0b775b86):(darcs:http://src.seereason.com/tptp-debian)"
--             , relaxInfo = [] }

privateTargets home =
    [ Target { sourcePackageName = "haskell-filecache"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-filecache"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-document"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-document"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-appraisal"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/artvaluereport"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-mailinglist"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/mailingList"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-generic-formlets3"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/generic-formlets3"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ontology"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-ontology"
             , relaxInfo = [] }
{-  , Target { sourcePackageName = "haskell-happstack-examples"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/happstack-examples"
             , relaxInfo = [] } -}

--    , Target { sourcePackageName = "happstack-blog"
--             , sourceSpec = "darcs:" ++ privateRepo ++ "/happstack-cms"
--             , relaxInfo = [] }
--    , Target { sourcePackageName = "happstack-imagegallery"
--             , sourceSpec = "darcs:" ++ privateRepo ++ "/imagegallery"
--             , relaxInfo = [] }
{-  -- Uses newSession, which was removed from happstack
    , Target { sourcePackageName = "haskell-algebrazam"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/AlgebraZam"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-algebrazam-quiz"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/algebrazam-quiz"
             , relaxInfo = [] } -}
{-  -- Compile error at the moment, but this package is not a current priority.
    , Target { sourcePackageName = "haskell-senioritymatters"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/SeniorityMatters"
             , relaxInfo = [] } -}
    , Target { sourcePackageName = "haskell-seereason"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/seereason"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-creativeprompts"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/creativeprompts"
             , relaxInfo = [] }
    , Target { sourcePackageName = "prefeteria"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/prefeteria"
             , relaxInfo = [] }
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , Target { sourcePackageName = "seereason-darcs-backups"
             , sourceSpec = "darcs:http://src.seereason.com/autobuilder-config"
             , relaxInfo = [] }
    ]

failingTargets release =
    [ Target { sourcePackageName = "haskell-uvector-algorithms"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/haskell-uvector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uvector"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.1.0/uvector-0.1.1.0.tar.gz:423e254dbbef0b57687f8adc737f7901):(darcs:http://src.seereason.com/haskell-uvector-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-criterion"
             -- , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/criterion/0.5.0.5/criterion-0.5.0.5.tar.gz:dddc5f8dc9dbabbd36b82a8a531b178c):(darcs:http://src.seereason.com/haskell-criterion-debian)"
             , sourceSpec = "apt:sid:haskell-criterion"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-statistics"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/statistics/0.3.5/statistics-0.3.5.tar.gz:b351bee9514e26555f170676b3c66139):(darcs:http://src.seereason.com/haskell-statistics-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-derive"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/derive/2.3.0.2/derive-2.3.0.2.tar.gz:7f8ad00e17c1cea5ad103b1481dfc250):(darcs:" ++ repo ++ "/haskell-derive-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-geni"
             , sourceSpec="deb-dir:(darcs:http://code.haskell.org/GenI):(darcs:http://src.seereason.com/haskell-geni-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-chart"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.14/Chart-0.14.tar.gz:a7189cd1483d50e2de7f2b20bb3f97d8):(darcs:http://src.seereason.com/haskell-chart-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-guarded-rewriting"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/guarded-rewriting/0.1/guarded-rewriting-0.1.tar.gz:0d4a284236a8a3241d93e8aec014198a):(darcs:http://src.seereason.com/haskell-guarded-rewriting-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-logic-tptp"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/logic-TPTP/0.2.0.1/logic-TPTP-0.2.0.1.tar.gz:5aae329f353bc3aafe2d47162108a02a):(darcs:http://src.seereason.com/logic-TPTP-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/hsx-xhtml):(darcs:http://src.seereason.com/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-vector-algorithms"
             , sourceSpec =
                 "deb-dir:(uri:http://hackage.haskell.org/packages/archive/vector-algorithms/0.3.4/vector-algorithms-0.3.4.tar.gz:1457802a2e0babf239c31b45d09d6b40):(darcs:http://src.seereason.com/haskell-vector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-split"
             , sourceSpec="apt:" ++ sid ++ ":haskell-split"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-benchpress"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/shellac-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-pcre-light"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-pcre-light"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-configfile"
             , sourceSpec = "apt:" ++ sid ++ ":haskell-configfile"
             , relaxInfo = [] }
    -- We need debhelper >= 7.0.50 for darcs 2.3.0.
    -- However, one of its unit tests fails:
    -- #   Failed test 'unavailable jobserver'
    -- #   at t/buildsystems/buildsystem_tests line 540.
--  , Target { sourcePackageName = "debhelper"
--           , sourceSpec = "apt:" ++ sid ++ ":debhelper"
--           , relaxInfo = [] } -}
--  -- Required for darcs 2.3.0
--  -- Fails because of missing dependency libssh2-1-dev
--  , Target { sourcePackageName = "curl"
--           , sourceSpec = "apt:" ++ sid ++ ":curl"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "tree-widget"
--           , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "gtk2hs"
--           , sourceSpec = "apt:" ++ sid ++ ":gtk2hs"
--           , relaxInfo = [] }
--  , Target { sourcePackageName="haskell-chart"
--           , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.11/Chart-0.11.tar.gz:b7f67defe06694eef580542947106fc0):(darcs:http://src.seereason.com/haskell-chart-debian)"
--           , relaxInfo = [] }
--  , Target { sourcePackageName="haskell-criterion"
--           , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/criterion/0.1.2/criterion-0.1.2.tar.gz:0e4d1c2f546ab650e03c610034c20226):(darcs:http://src.seereason.com/haskell-criterion-debian)"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "haskell-restarter"
--           , sourceSpec = "darcs:http://src.seereason.com/Restarter"
--           , relaxInfo = [] } -}
--    , Target { sourcePackageName = "jquery"
--             , sourceSpec = "apt:" ++ sid ++ ":jquery"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "yui-compressor"
--             , sourceSpec = "apt:" ++ sid ++ ":yui-compressor"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "rhino"
--             , sourceSpec = "apt:" ++ sid ++ ":rhino"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "maven-repo-helper"
--             , sourceSpec = "apt:" ++ sid ++ ":maven-repo-helper"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "libstax-java"
--             , sourceSpec = "proc:apt:" ++ sid ++ ":libstax-java"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "jqueryui"
--             , sourceSpec = "apt:" ++ sid ++ ":jqueryui"
--             , relaxInfo = [] 
--             }
    ]
