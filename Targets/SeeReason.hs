-- | Information specific to the SeeReason repository.
module Targets.SeeReason
    ( configure
    ) where

import Data.Lens.Lazy (modL)
import Data.Map as Map (alter)
import Data.Version (Version(Version))
import Debian.Debianize (Atoms, debianNameMap, VersionSplits(..), insertSplit)
import Distribution.Package (PackageName(PackageName))

configure :: Atoms -> Atoms
configure =
    -- All packages that want to use the older packages in clckwrks14
    -- need to apply extraSplits to their debianization.
    extraSplit "clckwrks" (Version [0, 15] []) .
    extraSplit "clckwrks" (Version [0, 14] []) .
    extraSplit "blaze-html" (Version [0, 6] []) .
    extraSplit "happstack-authenticate" (Version [0, 10] []) .
    extraSplit "http-types" (Version [0, 8] []) .
    extraSplit "case-insensitive" (Version [1] [])
    where
      extraSplit base ver =
          modL debianNameMap (Map.alter (Just . f) (PackageName base))
          where
            f Nothing = insertSplit base ver (VersionSplits {oldestPackage = base, splits = []})
            f (Just sp) = insertSplit base ver sp
