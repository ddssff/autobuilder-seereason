{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Targets.Hackage ( hackage, Flag(..), targets ) where

import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

debianize :: String -> Target
debianize name =
    let debName = map toLower name in
    Target { sourcePackageName = "haskell-" ++ debName
           , sourceSpec = "debianize:" ++ name
           , relaxInfo = [] }

hackage :: String -> String -> [Flag] -> Target
hackage "lucid-seereason" name flags =
     let debName = map toLower name in
     Target { sourcePackageName = "haskell-" ++ debName
            , sourceSpec = proc ++ "deb-dir:(hackage:" ++ name ++ v ++ "):(darcs:" ++ r ++ "/" ++ pre ++ name' ++ suff ++ ")"
            , relaxInfo = [] }
     where
       pre = if elem NP flags then "" else "haskell-"
       suff = if elem NS flags then "" else "-debian"
       name' = if elem UC flags then name else map toLower name
       proc = if elem P flags then "proc:" else ""
       v = foldl f "" flags
           where f _ (Pin ver) = "=" ++ ver
                 f s _ = s
       r = foldl f repo flags
           where f _ (Local home) = localRepo home
                 f s _ = s

-- |Build a hackage Target (a target that pulls the source code from
-- hackage.haskell.org) from a the cabal package and some flags
-- describing common variations on the mapping from one to another.
hackage "natty-seereason" name flags =
    let debName = map toLower name in
    Target { sourcePackageName = "haskell-" ++ debName
           , sourceSpec = proc ++ "debianize:" ++ name ++ v
           , relaxInfo = [] }
    where
      proc = if elem P flags then "proc:" else ""
      v = foldl f "" flags
          where f _ (Pin ver) = "=" ++ ver
                f s _ = s

hackage release _ _ = error $ "Target.Hackage.hackage - unexpected release: " ++ release

data Flag
    = Pin String   -- ^ Pin version number instead of using the most recent.  These arise when
                   -- a new hackage version appears but we aren't ready to migrate.
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is generally the _home parameter to targets.
    deriving Eq

-- |If these work in natty they would almost certainly work in lucid.
releaseTargets _home release@"natty-seereason" =
    [ -- Targets that are taken from Sid in lucid
      hackage release "text" []
    -- Doesn't build with our debianization due to funky bootstrapping
    -- files in dist/build/happy/happy-tmp.
    -- , hackage release "happy" []
    , hackage release "network" []
    , hackage release "QuickCheck" []
    , hackage release "syb" []
    , hackage release "syb-with-class" []
    , hackage release "gtk2hs-buildtools" []
    , hackage release "HTTP" []
    , hackage release "haskell-src-exts" []
    , debianize "random"
    , debianize "semigroups"
    , debianize "tagged"
    , debianize "polyparse"
    , debianize "HaXml"
    , debianize "haskeline"
    , debianize "hsx"
    , debianize "glib"
    -- , hackage release "mtl" []
    -- , hackage release "deepseq" []
    -- , hackage release "transformers" []
    ]
releaseTargets _home "lucid-seereason" =
    []
releaseTargets _home release =
    error $ "Unexpected release: " ++ release

-- | By default, these targets expect to find a debianization package
-- named "haskell-<packagename>-debian" in the repo exported from
-- Targets.Common.  To pull the debianization from a local repo add
-- the flag Local _home.
-- 
-- We usually create the debianization using this command:
--     cabal-debian --debianize --maintainer '...'
targets _home release =
    releaseTargets _home release ++
    [ hackage release "AES" []
    , hackage release "monads-tf" []
    , debianize "ansi-terminal"
    , debianize "ansi-wl-pprint"
    , hackage release "applicative-extras" [NP]
    , hackage release "attempt" []
    , debianize "authenticate"
    , hackage release "bimap" []
    , hackage release "bitset" []
    , hackage release "blaze-from-html" []
    , hackage release "bytestring-trie" []
    , hackage release "CC-delcont" [NP, UC]
    , hackage release "convertible-text" []
    , hackage release "data-object-json" []
    , hackage release "data-object" []
    , hackage release "digestive-functors" []
    , hackage release "digestive-functors-happstack" []
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , hackage release "double-conversion" []
    , hackage release "formlets" []
    , hackage release "funsat" []
    , hackage release "gd" []
    , hackage release "gnuplot" []
    , hackage release "happstack" [NP]
    -- Switch to the hackage target for happstack-data once a new upstream appears in hackage.
    , Target { sourcePackageName = "haskell-happstack-data"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz:be72c4c11d1317bf52c80782eac28a2d):(darcs:http://src.seereason.com/happstack-data-debian)"
             , relaxInfo = [] }
    -- , hackage release "happstack-data" [NP]
    , hackage release "happstack-ixset" [NP]
    , hackage release "ixset" [NP]
    , hackage release "happstack-server" [NP]
    , hackage release "happstack-state" [NP]
    , hackage release "happstack-util" [NP]
    -- Depends on pandoc
    , hackage release "safecopy" []
    , hackage release "heap" [NP]
    , hackage release "hoauth" []
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "hostname"
    , hackage release "HPDF" []
    , hackage release "HsOpenSSL" []
    , hackage release "i18n" [NP]
    , hackage release "incremental-sat-solver" []
    , hackage release "instant-generics" [P, Pin "0.3.2"]
    , hackage release "JSONb" []
    , hackage release "monadLib" []
    , hackage release "murmur-hash" [Pin "0.1.0.2"]
    , hackage release "nano-hmac" []
    , hackage release "openid" []
    , hackage release "operational" [NP]
    , hackage release "parse-dimacs" [NP]
    , hackage release "PBKDF2" [NP]
    , hackage release "permutation" [NS]
    , hackage release "PSQueue" []
    , hackage release "pwstore-purehaskell" []
    , hackage release "RJson" [NP, UC]
    , hackage release "sat" []
    , hackage release "test-framework-hunit" []
    , hackage release "test-framework-quickcheck" []
    , hackage release "test-framework" [Pin "0.4.0"]
    , hackage release "unicode-names" []
    , hackage release "unicode-properties" []
    , hackage release "utf8-prelude" [NP]
    , hackage release "web-encodings" []
    , hackage release "parseargs" []
    , hackage release "th-lift" [Pin "0.5.3"]
    , hackage release "haskell-src-meta" [Pin "0.4.0.1"]
    , hackage release "jmacro" [Pin "0.5.1"]
    , hackage release "bitmap" []
    , hackage release "bitmap-opengl" []
    , hackage release "stb-image" []
    , hackage release "vacuum" []
    , hackage release "vacuum-opengl" []
    , hackage release "RSA" []
    , hackage release "aeson" []
    , debianize "hashable"
    , debianize "unordered-containers"
    , hackage release "blaze-textual" []
    , hackage release "http-enumerator" [Pin "0.6.5.5"]
    , hackage release "xml-enumerator" []
    , hackage release "xml-types" []
    , hackage release "attoparsec-text-enumerator" []
    , hackage release "tagsoup" []
    -- This hackage target for pandoc is in the Sid module because
    -- some pandoc dependencies are there.  We would like to build the
    -- sid version of pandoc, but it requires a version of cdbs that
    -- probably can't be built under lucid.
    -- , hackage release "pandoc" [Pin "1.5.1.1"]
    ]
{-
  -- Testing generated targets against originals:
    ++ check [test (hackage release "pandoc" [Pin "1.5.1.1"])
              (Target { sourcePackageName = "haskell-pandoc"
                      , sourceSpec = "deb-dir:(hackage:pandoc=1.5.1.1):(darcs:http://src.seereason.com/haskell-pandoc-debian)"
                      , relaxInfo = [] })
             ]
    where
      check targets =
          case partitionEithers targets of
            ([], targets) -> targets
            (errs, _) -> error (intercalate "\n" (map (\ (orig, gen) -> "orig=" ++ show orig ++ "\n gen=" ++ show gen) errs))

      test :: Target -> Target -> Either (Target, Target) Target
      test gen target = if gen == target then Right target else Left (target, gen)
-}
