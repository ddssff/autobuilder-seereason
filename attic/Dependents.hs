{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds #-}
module Targets.Dependents
    ( dependents
    ) where

import Data.List (sort, nub)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

-- | Map from package name to the names of other packages that need to
-- be rebuilt.  We don't know this for sure until we update the source
-- of the packages, but usually the dependencies change very slowly.
dependents =
    Map.fromList
           [ ("haskell-happstack-scaffolding", ["haskell-seereason-base"])
           , ("haskell-happstack-authenticate", ["haskell-seereason-base", "haskell-happstack-scaffolding"])
           , ("haskell-debian", ["autobuilder", "haskell-cabal-debian", "haskell-debian-repo", "haskell-archive", "haskell-debian-mirror"])
           , ("haskell-json", ["haskell-happstack-extra", "haskell-happstack-facebook", "haskell-happstack-scaffolding", "haskell-pandoc", "haskell-seereason-base", "haskell-happstack-jmacro", "haskell-citeproc-hs", "haskell-hsx-jmacro", "haskell-jmacro"])
           , ("haskell-happstack-extra", ["haskell-happstack-facebook"])
           , ("haskell-pandoc", ["haskell-happstack-facebook", "haskell-happstack-extra"])
           , ("haskell-citeproc-hs", ["haskell-happstack-facebook", "haskell-happstack-extra", "haskell-pandoc"])
           , ("haskell-hsx-jmacro", ["haskell-seereason-base", "haskell-happstack-scaffolding"])
           , ("haskell-jmacro", ["haskell-seereason-base", "haskell-happstack-scaffolding", "haskell-happstack-jmacro", "haskell-hsx-jmacro"])
           , ("haskell-happstack-hsp", ["haskell-seereason-base", "haskell-happstack-scaffolding", "haskell-happstack-authenticate", "haskell-happstack-facebook", "haskell-happstack-extra", "haskell-happstackdotcom"])
           , ("haskell-happstack", ["haskell-happstack-facebook", "haskell-happstackdotcom"])
           , ("haskell-extra", ["haskell-archive", "haskell-debian-mirror", "haskell-help", "haskell-debian-repo"])
           , ("haskell-help", ["haskell-archive", "haskell-debian-mirror"])
           , ("haskell-debian-mirror", ["haskell-archive"])
           , ("haskell-debian", ["autobuilder", "haskell-debian-repo", "haskell-archive", "haskell-debian-mirror", "haskell-cabal-debian"])
           , ("haskell-debian-repo", ["autobuilder"])
           ]

andFriends :: [String] -> [String]
andFriends xs =
    if length xs' > length xs then andFriends xs' else xs
    where xs' = nub (sort (concat (xs : catMaybes (map (`Map.lookup` dependents) xs))))
