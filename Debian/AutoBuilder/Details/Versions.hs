{-# OPTIONS -Wall #-}
{-# LANGUAGE CPP #-}
module Debian.AutoBuilder.Details.Versions
    ( seereasonDefaults
    ) where

import Control.Lens ((%=))
import Data.Set as Set (insert)
import Debian.Debianize (CabalT, DebBase(DebBase), debInfo, missingDependencies, mapCabal, remapCabal, splitCabal, debianDefaults)
import Debian.Relation (BinPkgName(BinPkgName))
#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Version (Version, mkVersion)
import Distribution.Package (PackageName, mkPackageName)
#else
import Data.Version (Version(Version))
import Distribution.Package (PackageName(PackageName))

mkVersion ns = Version ns []
mkPackageName = PackageName
#endif

seereasonDefaults :: Monad m => CabalT m ()
seereasonDefaults =
    do debianDefaults

       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-happstack-authenticate-9-doc")
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-fail-doc") -- No modules, no doc
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-nats-doc") -- No modules, no doc
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-bytestring-builder-doc") -- No modules, no doc

       mapCabal (mkPackageName "clckwrks") (DebBase "clckwrks")
       splitCabal (mkPackageName "clckwrks") (DebBase "clckwrks-13") (mkVersion [0, 14])
       splitCabal (mkPackageName "clckwrks") (DebBase "clckwrks-14") (mkVersion [0, 15])

       -- We want a different name for *newer* versions of transformers, the normal
       -- name is built into ghc.  (Disabled, because ghc-7.10 includes version 0.4.2.)
       -- mapCabal (PackageName "transformers") (DebBase "transformers-4")
       -- splitCabal (PackageName "transformers") (DebBase "transformers") (Version [0, 4] [])

       mapCabal (mkPackageName "blaze-html") (DebBase "blaze-html")
       splitCabal (mkPackageName "blaze-html") (DebBase "blaze-html-5") (mkVersion [0, 6])

       mapCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (mkPackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (mkVersion [2])

       mapCabal (mkPackageName "http-types") (DebBase "http-types")
       splitCabal (mkPackageName "http-types") (DebBase "http-types-7") (mkVersion [0, 8])

       -- Remap to build debs for Cabal that do not conflict with the
       -- virtual package provided by ghc.
       remapCabal (mkPackageName "Cabal") (DebBase "cabal2")
       -- But only use the package name if the dependency requires Cabal >= 2
       splitCabal (mkPackageName "Cabal") (DebBase "cabal") (mkVersion [2])

       mapCabal (mkPackageName "web-plugins") (DebBase "web-plugins")
       splitCabal (mkPackageName "web-plugins") (DebBase "web-plugins-1") (mkVersion [0, 2])

       mapCabal (mkPackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (mkPackageName "QuickCheck") (DebBase "quickcheck") (mkVersion [2])

       -- mapCabal (PackageName "binary") (DebBase "binary-08")
       -- splitCabal (PackageName "binary") (DebBase "binary") (Version [0,8] [])

       mapCabal (mkPackageName "case-insensitive") (DebBase "case-insensitive")
       splitCabal (mkPackageName "case-insensitive") (DebBase "case-insensitive-0") (mkVersion [1])
