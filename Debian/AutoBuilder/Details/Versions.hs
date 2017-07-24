{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Details.Versions
    ( seereasonDefaults
    ) where

import Control.Lens ((%=))
import Data.Set as Set (insert)
import Data.Version (Version(Version))
import Debian.Debianize (CabalT, DebBase(DebBase), debInfo, missingDependencies, mapCabal, remapCabal, splitCabal, debianDefaults)
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName))

seereasonDefaults :: Monad m => CabalT m ()
seereasonDefaults =
    do debianDefaults

       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-happstack-authenticate-9-doc")
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-fail-doc") -- No modules, no doc
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-nats-doc") -- No modules, no doc
       (debInfo . missingDependencies) %= Set.insert (BinPkgName "libghc-bytestring-builder-doc") -- No modules, no doc

       mapCabal (PackageName "clckwrks") (DebBase "clckwrks")
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-13") (Version [0, 14] [])
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-14") (Version [0, 15] [])

       -- We want a different name for *newer* versions of transformers, the normal
       -- name is built into ghc.  (Disabled, because ghc-7.10 includes version 0.4.2.)
       -- mapCabal (PackageName "transformers") (DebBase "transformers-4")
       -- splitCabal (PackageName "transformers") (DebBase "transformers") (Version [0, 4] [])

       mapCabal (PackageName "blaze-html") (DebBase "blaze-html")
       splitCabal (PackageName "blaze-html") (DebBase "blaze-html-5") (Version [0, 6] [])

       mapCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (Version [2] [])

       mapCabal (PackageName "http-types") (DebBase "http-types")
       splitCabal (PackageName "http-types") (DebBase "http-types-7") (Version [0, 8] [])

       -- Remap to build debs for Cabal that do not conflict with the
       -- virtual package provided by ghc.
       -- remapCabal (PackageName "Cabal") (DebBase "cabal1228")
       -- But only use the package name if the dependency requires Cabal >= 1.22.8
       -- splitCabal (PackageName "Cabal") (DebBase "cabal") (Version [1, 22, 8] [])

       mapCabal (PackageName "web-plugins") (DebBase "web-plugins")
       splitCabal (PackageName "web-plugins") (DebBase "web-plugins-1") (Version [0, 2] [])

       mapCabal (PackageName "QuickCheck") (DebBase "quickcheck2")
       splitCabal (PackageName "QuickCheck") (DebBase "quickcheck") (Version [2] [])

       mapCabal (PackageName "case-insensitive") (DebBase "case-insensitive")
       splitCabal (PackageName "case-insensitive") (DebBase "case-insensitive-0") (Version [1] [])
