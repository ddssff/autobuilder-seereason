{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Details.Atoms
    ( seereasonDefaultAtoms
    ) where

import Data.Map as Map (alter)
import Data.Version (Version(Version))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Types.Atoms as T (missingDependencies, debianNameMap)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude ((+=), (%=))
import Debian.Debianize.VersionSplits (DebBase(DebBase))
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName))

seereasonDefaultAtoms :: Monad m => DebT m ()
seereasonDefaultAtoms =
    do debianDefaultAtoms

       missingDependencies += BinPkgName "libghc-happstack-authenticate-9-doc"

       mapCabal (PackageName "clckwrks") (DebBase "clckwrks")
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-13") (Version [0, 14] [])
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-14") (Version [0, 15] [])

       mapCabal (PackageName "blaze-html") (DebBase "blaze-html")
       splitCabal (PackageName "blaze-html") (DebBase "blaze-html-5") (Version [0, 6] [])

       mapCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate-9") (Version [0, 10] [])

       mapCabal (PackageName "http-types") (DebBase "http-types")
       splitCabal (PackageName "http-types") (DebBase "http-types-7") (Version [0, 8] [])

       mapCabal (PackageName "web-plugins") (DebBase "web-plugins")
       splitCabal (PackageName "web-plugins") (DebBase "web-plugins-1") (Version [0, 2] [])

       mapCabal (PackageName "case-insensitive") (DebBase "case-insensitive")
       splitCabal (PackageName "case-insensitive") (DebBase "case-insensitive-0") (Version [1] [])

       -- mapCabal (PackageName "cabal-install") (DebBase "cabal-install-ghcjs")
       remapCabal (PackageName "Cabal") (DebBase "cabal-ghcjs")

       -- Haven't worked out how to do documentation packages for ghcjs libraries
       missingDependencies += BinPkgName "libghcjs-ghcjs-dom-doc"
       missingDependencies += BinPkgName "libghcjs-blaze-builder-doc"
       missingDependencies += BinPkgName "libghcjs-blaze-markup-doc"
       missingDependencies += BinPkgName "libghcjs-blaze-html-doc"

-- | Belongs in cabal-debian
unmapCabal :: Monad m => PackageName -> DebT m ()
unmapCabal pname = debianNameMap %= Map.alter (const Nothing) pname

remapCabal :: Monad m => PackageName -> DebBase -> DebT m ()
remapCabal pname dname = unmapCabal pname >> mapCabal pname dname
