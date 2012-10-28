{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Targets 
    ( public
    , private
    ) where

import Data.Data (Data, toConstr)
import Data.Typeable (Typeable)
import Data.List (isPrefixOf, partition)
import Data.Monoid (mappend)
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Types.CacheRec as P
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Debian.Relation (PkgName(..), BinPkgName(..))
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

deriving instance Typeable PackageFlag
deriving instance Typeable BinPkgName
deriving instance Typeable PkgName
deriving instance Data PackageFlag
deriving instance Data BinPkgName
deriving instance Data PkgName

-- |Each of theses lists can be built on their own as a group,
-- and any sequence of groups can be built together as long as
-- no intermediate group is omitted.  Comment out the ones you
-- don't wish to build.
public :: String -> String -> P.Packages
public home release =
    fixFlags $ applyEpochMap $ applyDepMap $ Public.targets home release
    -- Dangerous when uncommented - build private targets into public, do not upload!!
    --          ++ private home

private :: String -> P.Packages
private home =
    fixFlags $ applyEpochMap $ applyDepMap $ mappend (Private.libraries home) (Private.applications home)

-- | Supply some special cases to map cabal library names to debian.
-- The prefix "lib" and the suffix "-dev" will be added later by
-- cabal-debian.
applyDepMap :: P.Packages -> P.Packages
applyDepMap P.NoPackage = P.NoPackage
applyDepMap (P.Packages n s) = P.Packages n (map applyDepMap s)
applyDepMap x@(P.Package {}) =
    x {P.flags = P.flags x ++ mappings}
    where
      mappings = [P.CabalDebian ["--map-dep", "cryptopp=libcrypto++-dev"],
                  P.CabalDebian ["--map-dep", "crypt=libc6-dev"],
                  P.CabalDebian ["--map-dep", "GL=libgl1-mesa-dev"],
                  P.CabalDebian ["--map-dep", "GLU=libglu1-mesa-dev"],
                  P.CabalDebian ["--map-dep", "glut=freeglut3-dev"]]
      deb = BinPkgName . PkgName

applyEpochMap :: P.Packages -> P.Packages
applyEpochMap P.NoPackage = P.NoPackage
applyEpochMap (P.Packages n s) = P.Packages n (map applyEpochMap s)
applyEpochMap x@(P.Package {}) =
    x {P.flags = P.flags x ++ mappings}
    where
      mappings = [ P.CabalDebian ["--epoch", "HTTP=1"], P.CabalDebian ["--epoch", "HaXml=1"] ]

fixFlags :: P.Packages -> P.Packages 
fixFlags = ensureFlag (P.CabalDebian ["--revision", ""]) . ensureFlag (P.CabalDebian ["--maintainer", "SeeReason Autobuilder <partners@seereason.com>"])

-- | If the package contains no flag with the same constructor as def add it to the flag list.
ensureFlag :: P.PackageFlag -> P.Packages -> P.Packages
ensureFlag _ P.NoPackage = P.NoPackage
ensureFlag def p@(P.Packages {Debian.AutoBuilder.Types.Packages.packages = ps}) = p {Debian.AutoBuilder.Types.Packages.packages = map (ensureFlag def) ps}
ensureFlag def p@(P.Package {flags = fs}) =
  case partition (\ x -> toConstr x == toConstr def) fs of
    ([], _) -> p {P.flags = def : fs}
    _ -> p
