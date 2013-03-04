-- | Information specific to the SeeReason repository.
module Targets.SeeReason
    ( knownEpochMappings
    , defaultAtoms
    ) where

import Data.Lens.Lazy (modL, setL)
import Data.Map as Map (Map, alter, fromList)
import Data.Monoid (mempty)
import Data.Set as Set (insert)
import Debian.Relation (BinPkgName(BinPkgName))
import Data.Version (Version(Version))
import Debian.Debianize (Atoms, debianNameMap, missingDependencies, epochMap)
import Debian.Debianize.Types.VersionSplits (VersionSplits, makePackage, insertSplit)
import Debian.Debianize.VersionSplits (mapCabal, splitCabal)
import Distribution.Package (PackageName(PackageName))

-- | We should always call this, just as we should always apply
-- knownVersionSplits.
knownEpochMappings :: Map PackageName Int
knownEpochMappings = Map.fromList [(PackageName "HaXml", 1), (PackageName "HTTP", 1)]

defaultAtoms = configure mempty

configure :: Atoms -> Atoms
configure =
    modL missingDependencies (Set.insert (BinPkgName "libghc-happstack-authenticate-9-doc")) .
    -- All packages that want to use the older packages in clckwrks14
    -- need to apply extraSplits to their debianization.
    setL epochMap knownEpochMappings .

    splitCabal (PackageName "parsec") "parsec2" (Version [3] []) .
    mapCabal (PackageName "parsec") "parsec3" .
    splitCabal (PackageName "QuickCheck") "quickcheck1" (Version [2] []) .
    mapCabal (PackageName "QuickCheck") "quickcheck2" .
    mapCabal (PackageName "gtk2hs-buildtools") "gtk2hs-buildtools" .

    splitCabal (PackageName "clckwrks") "clckwrks-13" (Version [0, 14] []) .
    splitCabal (PackageName "clckwrks") "clckwrks-14" (Version [0, 15] []) .
    mapCabal (PackageName "clckwrks") "clckwrks" .
    splitCabal (PackageName "blaze-html") "blaze-html-5" (Version [0, 6] []) .
    mapCabal (PackageName "blaze-html") "blaze-html" .
    splitCabal (PackageName "happstack-authenticate") "happstack-authenticate-9" (Version [0, 10] []) .
    mapCabal (PackageName "happstack-authenticate") "happstack-authenticate" .
    splitCabal (PackageName "http-types") "http-types-7" (Version [0, 8] []) .
    mapCabal (PackageName "http-types") "http-types" .
    splitCabal (PackageName "web-plugins") "web-plugins-1" (Version [0, 2] []) .
    mapCabal (PackageName "web-plugins") "web-plugins" .
    splitCabal (PackageName "case-insensitive") "case-insensitive-0" (Version [1] []) .
    mapCabal (PackageName "case-insensitive") "case-insensitive"
{-
    extraSplit "clckwrks" (Version [0, 15] []) .
    extraSplit "clckwrks" (Version [0, 14] []) .
    extraSplit "blaze-html" (Version [0, 6] []) .
    extraSplit "happstack-authenticate" (Version [0, 10] []) .
    extraSplit "http-types" (Version [0, 8] []) .
    extraSplit "case-insensitive" (Version [1] [])
-}
    where

{-
extraSplit :: PackageName -> String -> Version -> String -> Atoms -> Atoms
extraSplit pname ltname ver gename atoms =
    modL debianNameMap (Map.alter (Just . f) pname)
    where
      f Nothing = mapDebianName pname gename $ atoms

extraSplit :: String -> Version -> VersionSplit -> VersionSplit
extraSplit ltname ver split =
    modL debianNameMap (Map.alter (Just . f) pname)
    where
      f Nothing = insertSplit ltname ver (VersionSplits {oldestPackage = pname, splits = []})
      f (Just sp) = insertSplit ver ltname sp
-}
