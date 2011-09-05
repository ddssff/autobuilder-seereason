{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
module Targets.Hackage ( hackage, Flag(..), targets ) where

import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (intercalate)
import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec
import Targets.Common

-- | Build a target that pulls the source from hackage and then
-- generates a debianization using cabal-debian.
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
hackage _ name fs =
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
{-
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
-}

hackage release _ _ = error $ "Target.Hackage.hackage - unexpected release: " ++ release

-- |Transitional - hackage for lucid, debianize for natty
hackdeb "natty-seereason" name flags = debianize name []
hackdeb release@"lucid-seereason" name flags = hackage release name flags

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
      debianize "text" []
    -- Doesn't build with our debianization due to funky bootstrapping
    -- files in dist/build/happy/happy-tmp.
    -- , hackage release "happy" []
    , debianize "network" []
    , debianize "QuickCheck" [{-P.DebName "quickcheck2"-}]
    , debianize "syb" []
    , debianize "syb-with-class" []
    , debianize "gtk2hs-buildtools" []
    , debianize "HTTP" [P.Epoch 1]
    , debianize "random" []
    , debianize "semigroups" []
    , debianize "tagged" []
    , debianize "polyparse" []
    , debianize "HaXml" [P.Epoch 1]
    , debianize "haskeline" []
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
    , debianize "base-unicode-symbols" []
    , debianize "cprng-aes" []
    , debianize "SMTPClient" []
    , debianize "data-accessor-template" []
    , debianize "hashed-storage" []
    -- This target tells cabal-debian to add the dependency on the C
    -- package, but we need a version after 0.12.0, which doesn't have
    -- the "Ambiguous module name `Prelude'" error.  until then stick
    -- with Sid version.
    -- , debianize "glib" [P.ExtraDep "libglib2.0-dev"]

    -- , hackage release "mtl" []
    -- , hackage release "deepseq" []
    -- , hackage release "transformers" []

    , debianize "murmur-hash" []
    , debianize "test-framework" []
    , debianize "th-lift" []
    , debianize "haskell-src-meta" []
    , debianize "jmacro" []
    -- , debianize "http-enumerator" []
    , hackage release "happstack-state" [NP]
    , debianize "hsp" [P.ExtraDep "trhsx"]
    ]

releaseTargets _home release@"lucid-seereason" =
    [ hackage release "HsOpenSSL" []
    , hackage release "nano-hmac" []
    , hackage release "murmur-hash" [Pin "0.1.0.2"]
    , hackage release "test-framework" [Pin "0.4.0"]
    , hackage release "th-lift" [Pin "0.5.3"]
    , hackage release "haskell-src-meta" [Pin "0.4.0.1"]
    , hackage release "jmacro" [Pin "0.5.1"]
    -- , hackage release "http-enumerator" [Pin "0.6.5.5"]
    , hackage release "happstack-state" [NP]
    -- This pandoc debianize target has a dependency on an older version of HTTP
    -- , debianize "pandoc" []
    , hackage release "pandoc" []
    , hackage release "hsp" [NP]
    ]
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
    [ debianize "acid-state" []
    , debianize "ansi-terminal" []
    , debianize "ansi-wl-pprint" []
    , debianize "authenticate" []
    , debianize "data-object" []
    , debianize "happstack-plugins" []
    , debianize "plugins" []
    , debianize "hinotify" []
    , debianize "http-types" []
    , debianize "hostname" []
    , debianize "instant-generics" []
    , debianize "monad-par" []
    , debianize "operational" []
    , debianize "hashable" []
    , debianize "unordered-containers" []
    , debianize "http-enumerator" []
    , debianize "aeson-native" []
    , debianize "blaze-textual-native" []
    , debianize "logic-TPTP" [P.ExtraDep "alex", P.ExtraDep "happy"]
    , debianize "monad-control" []
    -- Version 2.9.2 specifies ghc < 7.2 and base == 4.3.*
    -- , debianize "haddock" []
    -- This is bundled with the compiler
    -- , debianize "process" []
    , debianize "split" []
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , hackdeb release "hsx" [NP]
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" []
    , let t = debianize "RSA" [] in t {P.spec = Quilt (P.spec t) (Darcs (repo ++ "/haskell-rsa-quilt") Nothing)}

    , hackdeb release "AES" []
    , hackdeb release "monads-tf" []
    , hackdeb release "applicative-extras" [NP]
    , hackdeb release "attempt" []
    , hackdeb release "bimap" []
    , hackdeb release "bitset" []
    , hackdeb release "blaze-from-html" []
    , hackdeb release "bytestring-trie" []
    , hackdeb release "CC-delcont" [NP, UC]
    , hackdeb release "convertible-text" []
    , hackdeb release "data-object-json" []
    , hackdeb release "digestive-functors" []
    , hackdeb release "digestive-functors-happstack" []
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , hackdeb release "double-conversion" []
    , hackdeb release "funsat" []
    , hackdeb release "gd" []
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , hackdeb release "gnuplot" []
    , hackdeb release "happstack" [NP]
    -- Switch to the hackage target for happstack-data once a new upstream appears in hackage.
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/happstack-data/6.0.0/happstack-data-6.0.0.tar.gz" "be72c4c11d1317bf52c80782eac28a2d") (Darcs "http://src.seereason.com/happstack-data-debian" Nothing)
                , P.flags = [] }
    -- , hackdeb release "happstack-data" [NP]
    , hackdeb release "happstack-ixset" [NP]
    , hackdeb release "ixset" [NP]
    , hackdeb release "happstack-server" [NP]
    , hackdeb release "happstack-util" [NP]
    -- Depends on pandoc
    , hackdeb release "safecopy" []
    , hackdeb release "heap" [NP]
    , hackdeb release "hoauth" []
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , hackdeb release "HPDF" []
    , hackdeb release "i18n" [NP]
    , hackdeb release "JSONb" []
    , hackdeb release "monadLib" []
    , hackdeb release "openid" []
    -- , hackdeb release "operational" [NP]
    , hackdeb release "parse-dimacs" [NP]
    , hackdeb release "PBKDF2" [NP]
    , hackdeb release "permutation" [NS]
    , hackdeb release "PSQueue" []
    , hackdeb release "pwstore-purehaskell" []
    , hackdeb release "RJson" [NP, UC]
    , hackdeb release "sat" []
    , hackdeb release "test-framework-hunit" []
    , hackdeb release "test-framework-quickcheck" []
    , hackdeb release "unicode-names" []
    , hackdeb release "unicode-properties" []
    , hackdeb release "utf8-prelude" [NP]
    , hackdeb release "web-encodings" []
    , hackdeb release "parseargs" []
    , hackdeb release "bitmap" []
    , hackdeb release "bitmap-opengl" []
    , hackdeb release "stb-image" []
    , hackdeb release "vacuum" []
    , hackdeb release "vacuum-opengl" []
    , hackdeb release "aeson" []
    , hackdeb release "blaze-textual" []
    , debianize "xml-enumerator" []
    , hackdeb release "xml-types" []
    , hackdeb release "attoparsec-text-enumerator" []
    , hackdeb release "tagsoup" []
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , hackdeb release "haskell-src-exts" [NP]
    , debianize "haskell-src-exts" []
    , hackage release "formlets" []
    , hackage release "incremental-sat-solver" []
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" [P.DebVersion "0.9-2~hackage1"]

{-  -- Algebra cohort
    , debianize "adjunctions" []
    , debianize "algebra" []
    , debianize "bifunctors" []
    , debianize "categories" []
    , debianize "comonad" []
    , debianize "comonads-fd" []
    , debianize "comonad-transformers" []
    , debianize "contravariant" []
    , debianize "data-lens" []
    , debianize "distributive" []
    , debianize "free" []
    , debianize "keys" []
    , debianize "representable-functors" []
    , debianize "representable-tries" []
    , debianize "semigroupoids" []
    , debianize "void" []
-}
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
