-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Targets 
    ( public
    , private
    ) where

import Data.List (isPrefixOf)
import Data.Monoid (mappend)
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.PackageFlag as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.RetrieveMethod (RetrieveMethod(Debianize, Hackage))
import qualified Targets.Public as Public
import qualified Targets.Private as Private

{-
data OSVendor
    = Ubuntu | Debian

data OSRelease
    = Dapper
    | Feisty
    | Edgy
    | Hardy
    | Intrepid
    | Jaunty
    | Karmic
    | Lucid
    | Maverick
    | Natty
    | Oneiric
    | Woody
    | Sarge
    | Etch
    | Lenny
    | Squeeze
    | Wheezy
    | Sid
    | Experimental

releaseVendor :: OSRelease -> OSVendor
releaseVendor Dapper = Ubuntu
releasevendor Feisty = Ubuntu
releasevendor Edgy = Ubuntu
releasevendor Hardy = Ubuntu
releasevendor Intrepid = Ubuntu
releasevendor Jaunty = Ubuntu
releasevendor Karmic = Ubuntu
releasevendor Lucid = Ubuntu
releasevendor Maverick = Ubuntu
releasevendor Natty = Ubuntu
releasevendor Oneiric = Ubuntu
releaseVendor Woody = Debian
releaseVendor Sarge = Debian
releaseVendor Etch = Debian
releaseVendor Lenny = Debian
releaseVendor Squeeze = Debian
releaseVendor Wheezy = Debian
releaseVendor Sid = Debian
releaseVendor Experimental = Debian

developmentRelease :: OSVendor -> Release
developmentRelease Ubuntu = Oneiric
developmentRelease Debian = Sid

ubuntuReleases = [Oneiric, Natty, Maverick, Lucid, Karmic, Jaunty, Intrepid, Hardy, Edgy, Feisty, Dapper]
-}

-- |Each of theses lists can be built on their own as a group,
-- and any sequence of groups can be built together as long as
-- no intermediate group is omitted.  Comment out the ones you
-- don't wish to build.
public :: String -> String -> P.Packages
public home release =
    applyEpochMap $ applyDepMap $ Public.targets home release
    -- Dangerous when uncommented - build private targets into public, do not upload!!
    --          ++ private home

private :: String -> P.Packages
private home =
    applyEpochMap $ applyDepMap $ mappend (Private.libraries home) (Private.applications home)

-- | Supply some special cases to map cabal library names to debian.
-- The prefix "lib" and the suffix "-dev" will be added later by
-- cabal-debian.
applyDepMap :: P.Packages -> P.Packages
applyDepMap P.NoPackage = P.NoPackage
applyDepMap (P.Packages s) = P.Packages (Set.map applyDepMap s)
applyDepMap x@(P.Package {}) =
    x {P.spec = case P.spec x of
                  Debianize s flags -> Debianize s (flags ++ mappings)
                  Hackage s flags -> Hackage s (flags ++ mappings)
                  _ -> P.spec x }
    where
      mappings = [P.MapDep "cryptopp" "crypto++"]

applyEpochMap :: P.Packages -> P.Packages
applyEpochMap P.NoPackage = P.NoPackage
applyEpochMap (P.Packages s) = P.Packages (Set.map applyEpochMap s)
applyEpochMap x@(P.Package {}) =
    x {P.spec = case P.spec x of
                  Debianize s flags -> Debianize s (flags ++ mappings)
                  Hackage s flags -> Hackage s (flags ++ mappings)
                  _ -> P.spec x }
    where
      mappings =
          [ P.Epoch "HTTP" 1
          , P.Epoch "HaXml" 1 ]