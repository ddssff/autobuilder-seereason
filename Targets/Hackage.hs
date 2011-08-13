{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Targets.Hackage ( hackage, Flag(..), targets ) where

import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

-- |Build a hackage Target (a target that pulls the source code from
-- hackage.haskell.org) from a the cabal package and some flags
-- describing common variations on the mapping from one to another.
hackage :: String -> [Flag] -> Target
hackage name flags =
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

data Flag
    = Pin String   -- ^ Pin version number instead of using the most recent.  These arise when
                   -- a new hackage version appears but we aren't ready to migrate.
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is generally the _home parameter to targets.
    deriving Eq

-- | By default, these targets expect to find a debianization package
-- named "haskell-<packagename>-debian" in the repo exported from
-- Targets.Common.  To pull the debianization from a local repo add
-- the flag Local _home.
-- 
-- We usually create the debianization using this command:
--     cabal-debian --debianize --maintainer '...'
targets _home =
    [ hackage "AES" []
    , hackage "ansi-terminal" [NP]
    , hackage "ansi-wl-pprint" [NP]
    , hackage "applicative-extras" [NP]
    , hackage "attempt" []
    , hackage "authenticate" []
    , hackage "bimap" []
    , hackage "bitset" []
    , hackage "blaze-from-html" []
    , hackage "bytestring-trie" []
    , hackage "CC-delcont" [NP, UC]
    , hackage "convertible-text" []
    , hackage "data-object-json" []
    , hackage "data-object" []
    , hackage "digestive-functors" []
    , hackage "digestive-functors-happstack" []
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , hackage "double-conversion" []
    , hackage "formlets" []
    , hackage "funsat" []
    , hackage "gd" []
    , hackage "gnuplot" []
    , hackage "happstack" [NP, Pin "6.0.1"]
    , hackage "happstack-data" [NP]
    , hackage "happstack-ixset" [NP]
    , hackage "ixset" [NP]
    , hackage "happstack-server" [NP, Pin "6.1.6"]
    , hackage "happstack-state" [NP, Pin "6.0.0"]
    , hackage "happstack-util" [NP]
    -- Depends on pandoc
    , hackage "safecopy" []
    , hackage "heap" [NP]
    , hackage "hoauth" []
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , hackage "hostname" []
    , hackage "HPDF" []
    , hackage "HsOpenSSL" []
    , hackage "i18n" [NP]
    , hackage "incremental-sat-solver" []
    , hackage "instant-generics" [P, Pin "0.3.2"]
    , hackage "JSONb" []
    , hackage "monadLib" []
    , hackage "murmur-hash" [Pin "0.1.0.2"]
    , hackage "nano-hmac" []
    , hackage "openid" []
    , hackage "operational" [NP]
    , hackage "parse-dimacs" [NP]
    , hackage "PBKDF2" [NP]
    , hackage "permutation" [NS]
    , hackage "PSQueue" []
    , hackage "pwstore-purehaskell" []
    , hackage "RJson" [NP, UC]
    , hackage "sat" []
    , hackage "test-framework-hunit" []
    , hackage "test-framework-quickcheck" []
    , hackage "test-framework" [Pin "0.4.0"]
    , hackage "unicode-names" []
    , hackage "unicode-properties" []
    , hackage "utf8-prelude" [NP]
    , hackage "web-encodings" []
    , hackage "parseargs" []
    , hackage "th-lift" [Pin "0.5.3"]
    , hackage "haskell-src-meta" []
    , hackage "jmacro" [Pin "0.5.1"]
    , hackage "bitmap" []
    , hackage "bitmap-opengl" []
    , hackage "stb-image" []
    , hackage "vacuum" []
    , hackage "vacuum-opengl" []
    , hackage "RSA" []
    , hackage "aeson" []
    , hackage "hashable" []
    , hackage "unordered-containers" [Pin "0.1.4.0"]
    , hackage "blaze-textual" []
    , hackage "http-enumerator" [Pin "0.6.5.5"]
    , hackage "xml-enumerator" []
    , hackage "xml-types" []
    , hackage "attoparsec-text-enumerator" []
    , hackage "tagsoup" []
    -- This hackage target for pandoc is in the Sid module because
    -- some pandoc dependencies are there.  We would like to build the
    -- sid version of pandoc, but it requires a version of cdbs that
    -- probably can't be built under lucid.
    -- , hackage "pandoc" [Pin "1.5.1.1"]
    ]
{-
  -- Testing generated targets against originals:
    ++ check [test (hackage "pandoc" [Pin "1.5.1.1"])
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
