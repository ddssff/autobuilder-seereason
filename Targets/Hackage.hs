{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Targets.Hackage ( hackage, Flag(..), targets ) where

import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

data Flag
    = Pin String   -- ^ Pin version number
    | UC           -- ^ Use hackage name as debian repo name without converting to lower case
    | NP           -- ^ Do not put haskell- prefix on debian repo name
    | NS           -- ^ Do not put suffix -debian on debian repo name
    | P            -- ^ Make it a proc: target
    | Local String -- ^ Use a local repo, Argument is _home
    deriving Eq

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

targets _home =
    [ hackage "AES" []
    , hackage "ansi-terminal" [NP]
    , hackage "ansi-wl-pprint" [NP]
    , hackage "applicative-extras" [NP]
    , hackage "attempt" [Pin "0.3.0"]
    , hackage "authenticate" []
    , hackage "bimap" []
    , hackage "bitset" []
    , hackage "blaze-from-html" []
    , hackage "bytestring-trie" []
    , hackage "CC-delcont" [NP, UC]
    , hackage "convertible-text" []
    , hackage "data-object-json" []
    , hackage "data-object" []
    , hackage "digestive-functors" [Pin "0.1.0.0"]
    , hackage "digestive-functors-happstack" []
    , hackage "formlets" []
    , hackage "funsat" []
    , hackage "gd" []
    , hackage "gnuplot" []
    , hackage "happstack" [NP, Pin "6.0.0"]
    , hackage "happstack-data" [NP]
    , hackage "happstack-ixset" [NP]
    , hackage "happstack-server" [NP, Pin "6.1.5"]
    , hackage "happstack-state" [NP]
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
    , hackage "instant-generics" [P]
    , hackage "JSONb" []
    , hackage "monadLib" []
    , hackage "murmur-hash" []
    , hackage "nano-hmac" []
    , hackage "openid" []
    , hackage "operational" [NP, Pin "0.2.0.1"]
    , hackage "parse-dimacs" [NP]
    , hackage "PBKDF2" [NP]
    , hackage "permutation" [NS]
    , hackage "PSQueue" []
    , hackage "pwstore-purehaskell" []
    , hackage "RJson" [NP, UC]
    , hackage "sat" []
    , hackage "test-framework-hunit" []
    , hackage "test-framework-quickcheck" []
    , hackage "test-framework" []
    , hackage "unicode-names" []
    , hackage "unicode-properties" []
    , hackage "utf8-prelude" [NP]
    , hackage "web-encodings" []
    , hackage "parseargs" []
    , hackage "th-lift" []
    , hackage "haskell-src-meta" [Pin "0.4"]
    , hackage "jmacro" []
    , hackage "bitmap" []
    , hackage "bitmap-opengl" []
    , hackage "stb-image" []
    , hackage "vacuum" []
    , hackage "vacuum-opengl" []
    , hackage "RSA" []
    , hackage "aeson" [Pin "0.3.2.8"]
    , hackage "hashable" [Pin "1.1.2.0"]
    , hackage "unordered-containers" []
    , hackage "blaze-textual" [Pin "0.1.0.0"]
    , hackage "http-enumerator" [Pin "0.6.5.4"]
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
