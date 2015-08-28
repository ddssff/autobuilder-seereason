{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Debian.AutoBuilder.Details.Targets
    ( public
    , private
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.State (evalState)
import Data.Monoid (mappend)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Debian.Relation (Relation(..), BinPkgName(..))
import Debian.Relation.String (parseRelations)
import Debian.Releases (BaseRelease(..), baseRelease, Release)
import qualified Debian.Repo.Fingerprint as P
-- import Debian.AutoBuilder.Details.GHC (ghc)
import qualified Debian.AutoBuilder.Details.Public as Public
import qualified Debian.AutoBuilder.Details.Private as Private

-- |Each of theses lists can be built on their own as a group,
-- and any sequence of groups can be built together as long as
-- no intermediate group is omitted.  Comment out the ones you
-- don't wish to build.
public :: String -> Release -> P.Packages
public home release =
    proc' $
    {- relaxCabalDebian $ fixFlags $ -} applyEpochMap $ applyDepMap release $ evalState (Public.targets) (targetState release home)
    -- Dangerous when uncommented - build private targets into public, do not upload!!
    --          ++ private home

private :: String -> Release -> P.Packages
private home release =
    proc' $
    {- relaxCabalDebian $ fixFlags $ -} applyEpochMap $ applyDepMap release $ evalState (mappend <$> Private.libraries <*> Private.applications) (targetState release home)

proc' :: P.Packages -> P.Packages
proc' p@(Named {}) = p {packages = proc' (packages p)}
proc' p@(Packages {}) = p {list = map proc' (list p)}
proc' a@(APackage (Package {_spec = P.Proc _})) = a
proc' (APackage p) = APackage (p {_spec = P.Proc (_spec p)})
proc' p = p

-- | This prevents every package that uses cabal-debian for
-- debianization from rebuilding every time the library is revved.
-- Presumably the current build is working ok, right?
relaxCabalDebian :: P.Packages -> P.Packages
relaxCabalDebian (P.Named n s) = P.Named n (relaxCabalDebian s)
relaxCabalDebian (P.Packages s) = P.Packages (map relaxCabalDebian s)
relaxCabalDebian (P.APackage p)
    | isDebianizeSpec (P._spec p) =
        P.APackage (p {P._flags = P._flags p ++ map P.RelaxDep ["libghc-cabal-debian-dev", "libghc-cabal-debian-prof", "libghc-cabal-debian-doc"]})
relaxCabalDebian x = x

-- FIXME - make this generic.  Not sure if the assumption that
-- Debianize is outermost is valid.
isDebianizeSpec  :: P.RetrieveMethod -> Bool
isDebianizeSpec (P.Debianize'' _ _) = True
isDebianizeSpec _ = False

-- | Add MapDep and DevelDep flags Supply some special cases to map cabal library names to debian.
applyDepMap :: Release -> P.Packages -> P.Packages
applyDepMap _ P.NoPackage = P.NoPackage
applyDepMap release (P.Named n s) = P.Named n (applyDepMap release s)
applyDepMap release (P.Packages s) = P.Packages (map (applyDepMap release) s)
applyDepMap release (P.APackage x) =
    APackage (x {P._flags = P._flags x ++ mappings})
    where
      mappings = [P.MapDep "cryptopp" (deb "libcrypto++-dev"),
                  P.MapDep "crypto" (deb "libcrypto++-dev"),
                  P.MapDep "crypt" (deb "libc6-dev"),
                  P.MapDep "GL" (deb "libgl1-mesa-dev"),
                  P.MapDep "GLU" (deb "libglu1-mesa-dev"),
                  P.MapDep "glut" (deb "freeglut3-dev"),
                  P.MapDep "pcre" (deb "libpcre3-dev"),
                  P.MapDep "m" (deb "libc6-dev"),
                  P.MapDep "X11" (deb "libx11-dev"),
                  P.MapDep "Xi" (deb "libxi-dev"),
                  P.MapDep "Xxf86vm" (deb "libxxf86vm-dev"),
                  P.MapDep "pthread" (deb "libc6-dev"),
                  P.MapDep "Xrandr" (rel ("libxrandr-dev" ++
                                          case baseRelease release of
                                            Wheezy -> " (= 2:1.3.2-2+deb7u1)"
                                            Precise -> " (= 2:1.3.2-2ubuntu0.2)"
                                            _ -> "")),
                  -- the libxrandr-dev-lts-quantal package installs
                  -- /usr/lib/x86_64-linux-gnu/libXrandr_ltsq.so.
                  -- P.MapDep "Xrandr_ltsq" (deb "libxrandr-dev-lts-quantal"),
                  P.MapDep "freetype" (deb "libfreetype6-dev"),
                  P.MapDep "icuuc" (deb "libicu-dev"),
                  P.MapDep "icui18n" (deb "libicu-dev"),
                  P.MapDep "icudata" (deb "libicu-dev")
                 ]
      deb s = [[Rel (BinPkgName s) Nothing Nothing]]
      rel s = either (error $ "Parse error in debian relations: " ++ show s) id (parseRelations s)

applyEpochMap :: P.Packages -> P.Packages
applyEpochMap P.NoPackage = P.NoPackage
applyEpochMap (P.Named n s) = P.Named n (applyEpochMap s)
applyEpochMap (P.Packages s) = P.Packages (map applyEpochMap s)
applyEpochMap (P.APackage x) =
    P.APackage (x {P._flags = P._flags x ++ mappings})
    where
      mappings = [ P.Epoch "HTTP" 1, P.Epoch "HaXml" 1 ]

{-
fixFlags :: P.Packages -> P.Packages 
fixFlags = ensureFlag (P.Revision "") . ensureFlag (P.Maintainer "SeeReason Autobuilder <partners@seereason.com>")

-- | If the package contains no flag with the same constructor as def add it to the flag list.
ensureFlag :: P.PackageFlag -> P.Packages -> P.Packages
ensureFlag _ P.NoPackage = P.NoPackage
ensureFlag def p@(P.Packages {Debian.AutoBuilder.Types.Packages.list = ps}) = p {Debian.AutoBuilder.Types.Packages.list = map (ensureFlag def) ps}
ensureFlag def p@(P.Package {flags = fs}) =
  case partition (\ x -> toConstr x == toConstr def) fs of
    ([], _) -> p {P.flags = def : fs}
    _ -> p
-}
