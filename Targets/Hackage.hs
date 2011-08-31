{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Targets.Hackage ( hackage, Flag(..), targets ) where

import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec
import Targets.Common

debianize :: String -> [P.PackageFlag] -> P.Package
debianize s flags =
    P.Package { P.name = "haskell-" ++ debianName s
              , P.spec = Debianize s Nothing
              , P.flags = flags}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "quickcheck2"
      debianName "parsec" = "parsec3"
      debianName _ = map toLower s

hackage :: String -> String -> [Flag] -> P.Package
hackage "lucid-seereason" name fs =
     P.Package { P.name = "haskell-" ++ map toLower name
               , P.spec = proc $ DebDir (Hackage name v) (Darcs (r ++ "/" ++ pre ++ name' ++ suff) Nothing)
               , P.flags = [] }
     where
       pre = if elem NP fs then "" else "haskell-"
       suff = if elem NS fs then "" else "-debian"
       name' = if elem UC fs then name else map toLower name
       proc x = if elem P fs then Proc x else x
       v = foldl f Nothing fs
           where f Nothing (Pin ver) = Just ver
                 f s _ = s
       r = foldl f repo fs
           where f _ (Local home) = localRepo home
                 f s _ = s

-- |Build a hackage Target (a target that pulls the source code from
-- hackage.haskell.org) from a the cabal package and some flags
-- describing common variations on the mapping from one to another.
hackage "natty-seereason" name flags =
    let debName = map toLower name in
    P.Package { P.name = "haskell-" ++ debName
              , P.spec = proc $ Debianize name v
              , P.flags = [] }
    where
      proc spec = if elem P flags then Proc spec else spec
      v = foldl f Nothing flags
          where f Nothing (Pin ver) = Just ver
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
    , debianize "QuickCheck" [{-P.DebName "quickcheck2"-}]
    , hackage release "syb" []
    , hackage release "syb-with-class" []
    , hackage release "gtk2hs-buildtools" []
    , debianize "HTTP" [P.Epoch 1]
    , hackage release "haskell-src-exts" []
    , debianize "random" []
    , debianize "semigroups" []
    , debianize "tagged" []
    , debianize "polyparse" []
    , debianize "HaXml" [P.Epoch 1]
    , debianize "haskeline" []
    , debianize "hsx" []
    , debianize "uniplate" []
    , debianize "HsOpenSSL" [P.ExtraDep "libcrypto++-dev"]
    , debianize "nano-hmac" [P.ExtraDep "libcrypto++-dev"]

{-
    , P.Package { P.name = "haskell-json"
                , P.spec = Quilt (Debianize "json" Nothing) (Darcs "http://src.seereason.com/haskell-json-quilt" Nothing)
                , P.flags = [P.DebVersion "0.4.4-1.1"] }
-}
    , debianize "blaze-html" []
    , debianize "Crypto" []
    , debianize "double-conversion" []
    , debianize "MissingH" []
    , debianize "attoparsec" []
    , debianize "dataenc" []
    , debianize "fgl" []
    , debianize "case-insensitive" []
    , debianize "haskell-src" [P.ExtraDep "happy"]
    , debianize "hsp" [P.ExtraDep "trhsx"]
    , debianize "base-unicode-symbols" []
    , debianize "cprng-aes" []
    , debianize "SMTPClient" []
    -- This target tells cabal-debian to add the dependency on the C
    -- package, but we need a version after 0.12.0, which doesn't have
    -- the "Ambiguous module name `Prelude'" error.  until then stick
    -- with Sid version.
    -- , debianize "glib" [P.ExtraDep "libglib2.0-dev"]

    -- , hackage release "mtl" []
    -- , hackage release "deepseq" []
    -- , hackage release "transformers" []
    ]
releaseTargets _home release@"lucid-seereason" =
    [ hackage release "HsOpenSSL" []
    , hackage release "nano-hmac" [] ]
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
    , debianize "acid-state" []
    , hackage release "monads-tf" []
    , debianize "ansi-terminal" []
    , debianize "ansi-wl-pprint" []
    , hackage release "applicative-extras" [NP]
    , hackage release "attempt" []
    , debianize "authenticate" []
    , hackage release "bimap" []
    , hackage release "bitset" []
    , hackage release "blaze-from-html" []
    , hackage release "bytestring-trie" []
    , hackage release "CC-delcont" [NP, UC]
    , hackage release "convertible-text" []
    , hackage release "data-object-json" []
    , debianize "data-object" []
    , hackage release "digestive-functors" []
    , hackage release "digestive-functors-happstack" []
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , hackage release "double-conversion" []
    , hackage release "formlets" []
    , hackage release "funsat" []
    , hackage release "gd" []
    , hackage release "gnuplot" []
    , hackage release "happstack" [NP]
    , debianize "happstack-plugins" []
    , debianize "plugins" []
    , debianize "hinotify" []
    , debianize "http-types" []
    -- Switch to the hackage target for happstack-data once a new upstream appears in hackage.
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz" "be72c4c11d1317bf52c80782eac28a2d") (Darcs "http://src.seereason.com/happstack-data-debian" Nothing)
                , P.flags = [] }
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
    , debianize "hostname" []
    , hackage release "HPDF" []
    , hackage release "i18n" [NP]
    , hackage release "incremental-sat-solver" []
    , debianize "instant-generics" []
    , hackage release "JSONb" []
    , hackage release "monadLib" []
    , hackage release "murmur-hash" [Pin "0.1.0.2"]
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
    , debianize "hashable" []
    , debianize "unordered-containers" []
    , hackage release "blaze-textual" []
    -- , hackage release "http-enumerator" [Pin "0.6.5.5"]
    , debianize "http-enumerator" []
    , debianize "aeson-native" []
    , debianize "blaze-textual-native" []
    , hackage release "xml-enumerator" []
    , hackage release "xml-types" []
    , hackage release "attoparsec-text-enumerator" []
    , hackage release "tagsoup" []
    , debianize "logic-TPTP" [P.ExtraDep "alex", P.ExtraDep "happy"]
    , debianize "monad-control" []
    -- Version 2.9.2 specifies ghc < 7.2 and base == 4.3.*
    -- , debianize "haddock" []
    , debianize "data-accessor-template" []
    , debianize "process" []
    , debianize "split" []
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
