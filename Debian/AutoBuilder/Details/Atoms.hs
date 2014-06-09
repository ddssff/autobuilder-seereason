{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Details.Atoms
    ( seereasonDefaultAtoms
    ) where

import Data.Version (Version(Version))
import Debian.Debianize.DebianName (mapCabal, splitCabal)
import Debian.Debianize.Details (debianDefaultAtoms)
import Debian.Debianize.Types.Atoms as T (missingDependencies)
import Debian.Debianize.Monad (DebT)
import Debian.Debianize.Prelude ((+=))
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName))

seereasonDefaultAtoms :: Monad m => DebT m ()
seereasonDefaultAtoms =
    do debianDefaultAtoms

       missingDependencies += BinPkgName "libghc-happstack-authenticate-9-doc"
       missingDependencies += BinPkgName "libghc-cabal-ghcjs-doc"

       mapCabal (PackageName "clckwrks") "clckwrks"
       splitCabal (PackageName "clckwrks") "clckwrks-13" (Version [0, 14] [])
       splitCabal (PackageName "clckwrks") "clckwrks-14" (Version [0, 15] [])

       mapCabal (PackageName "blaze-html") "blaze-html"
       splitCabal (PackageName "blaze-html") "blaze-html-5" (Version [0, 6] [])

       mapCabal (PackageName "happstack-authenticate") "happstack-authenticate"
       splitCabal (PackageName "happstack-authenticate") "happstack-authenticate-9" (Version [0, 10] [])

       mapCabal (PackageName "http-types") "http-types"
       splitCabal (PackageName "http-types") "http-types-7" (Version [0, 8] [])

       mapCabal (PackageName "web-plugins") "web-plugins"
       splitCabal (PackageName "web-plugins") "web-plugins-1" (Version [0, 2] [])

       mapCabal (PackageName "case-insensitive") "case-insensitive"
       splitCabal (PackageName "case-insensitive") "case-insensitive-0" (Version [1] [])

       -- We build special versions of cabal and cabal-install to support the ghcjs compiler
       mapCabal (PackageName "Cabal") "cabal-ghcjs"
       mapCabal (PackageName "cabal-install") "cabal-install-ghcjs"
