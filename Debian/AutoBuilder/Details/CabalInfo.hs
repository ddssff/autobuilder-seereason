{-# OPTIONS -Wall #-}
module Debian.AutoBuilder.Details.CabalInfo
    ( seereasonDefaults
    ) where

import Data.Version (Version(Version))
import Debian.Debianize (CabalT, (+=), DebBase(DebBase), debInfo, missingDependencies, mapCabal, splitCabal, debianDefaults)
import Debian.Relation (BinPkgName(BinPkgName))
import Distribution.Package (PackageName(PackageName))

seereasonDefaults :: Monad m => CabalT m ()
seereasonDefaults =
    do debianDefaults

       (debInfo . missingDependencies) += BinPkgName "libghc-happstack-authenticate-9-doc"

       mapCabal (PackageName "clckwrks") (DebBase "clckwrks")
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-13") (Version [0, 14] [])
       splitCabal (PackageName "clckwrks") (DebBase "clckwrks-14") (Version [0, 15] [])

       mapCabal (PackageName "blaze-html") (DebBase "blaze-html")
       splitCabal (PackageName "blaze-html") (DebBase "blaze-html-5") (Version [0, 6] [])

       mapCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate")
       splitCabal (PackageName "happstack-authenticate") (DebBase "happstack-authenticate-0") (Version [2] [])

       mapCabal (PackageName "http-types") (DebBase "http-types")
       splitCabal (PackageName "http-types") (DebBase "http-types-7") (Version [0, 8] [])

       mapCabal (PackageName "web-plugins") (DebBase "web-plugins")
       splitCabal (PackageName "web-plugins") (DebBase "web-plugins-1") (Version [0, 2] [])

       mapCabal (PackageName "case-insensitive") (DebBase "case-insensitive")
       splitCabal (PackageName "case-insensitive") (DebBase "case-insensitive-0") (Version [1] [])
