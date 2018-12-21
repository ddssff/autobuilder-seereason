{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Debian.AutoBuilder.Details.Targets
    ( public
    , private
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Lens ((%=), (%~), use, view)
import Control.Monad.State (execState, execStateT, get, modify)
import Data.List as List (map)
import Data.Map as Map (insert, map)
import Data.Monoid (mappend)
import qualified Debian.AutoBuilder.Types.Packages as P hiding (TSt)
import Debian.AutoBuilder.Types.Packages hiding (TSt)
import Debian.Debianize as D (CabalInfo, debInfo, execMap)
import Debian.Relation (Relation(..), BinPkgName(..))
import Debian.Relation.String (parseRelations)
import Debian.Release (ReleaseName(..))
import Debian.Releases (BaseRelease(..), baseRelease, Distro(..), ReleaseTree(..))
import qualified Debian.Repo.Fingerprint as P
import Debian.AutoBuilder.Details.Common (seeReason7, seeReason8, TSt)
import qualified Debian.AutoBuilder.Details.Artful as Artful
import qualified Debian.AutoBuilder.Details.Xenial as Xenial
import qualified Debian.AutoBuilder.Details.Private as Private
import Debian.AutoBuilder.Types.ParamRec (ParamRec)
import Debian.Repo.Internal.Apt (MonadApt)
import Debian.Repo.Slice (NamedSliceList, SourcesChangedAction)
import Debian.Repo.State.AptImage (withAptImage)

-- |Each of theses lists can be built on their own as a group,
-- and any sequence of groups can be built together as long as
-- no intermediate group is omitted.  Comment out the ones you
-- don't wish to build.
public :: Monad m => ParamRec -> TSt m ()
public params = do
  rel <- use release
  let targets =
          case rel of
            -- ExtendedRelease (Release Xenial) SeeReason8 -> Xenial.buildTargets8
            ExtendedRelease (Foundation (BaseRelease _ (ReleaseName "bionic"))) distro | distro == seeReason8 -> Xenial.buildTargets84
            _ -> error $ "Unexpected release: " ++ show rel
  -- Dangerous when uncommented - build private targets into public, do not upload!!
  -- private params >>
  targets >> applyEpochMap >> applyExecMap >> use P.release >>= applyDepMap >> proc'

private :: Monad m => ParamRec -> TSt m ()
private params =
    Private.buildTargets params >> applyEpochMap >> applyExecMap >> use P.release >>= applyDepMap >> proc'

proc' :: Monad m => TSt m ()
proc' =
    packageMap %= Map.map f
    where
      f p@(Package {_spec = P.Proc _}) = p
      f p = p {_spec = P.Proc (_spec p)}

-- | This prevents every package that uses cabal-debian for
-- debianization from rebuilding every time the library is revved.
-- Presumably the current build is working ok, right?
relaxCabalDebian :: Monad m => TSt m ()
relaxCabalDebian =
    packageMap %= Map.map f
    where f p | isDebianizeSpec (P._spec p) =
                  p {_flags = _flags p ++ (List.map P.RelaxDep ["libghc-cabal-debian-dev",
                                                                "libghc-cabal-debian-prof",
                                                                "libghc-cabal-debian-doc"])}
          f p = p

-- FIXME - make this generic.  Not sure if the assumption that
-- Debianize is outermost is valid.
isDebianizeSpec  :: P.RetrieveMethod -> Bool
isDebianizeSpec (P.Debianize'' _ _) = True
isDebianizeSpec _ = False

-- | Add MapDep and DevelDep flags Supply some special cases to map cabal library names to debian.
applyDepMap :: Monad m => ReleaseTree -> TSt m ()
applyDepMap release =
    packageMap %= Map.map f
    where
      f x = x {P._flags = P._flags x ++ mappings}
      mappings = [P.MapDep "cairo" (deb "libcairo2-dev"),
                  P.MapDep "cryptopp" (deb "libcrypto++-dev"),
                  P.MapDep "crypto" (deb "libcrypto++-dev"),
                  P.MapDep "crypt" (deb "libc6-dev"),
                  P.MapDep "GL" (deb "libgl1-mesa-dev"),
                  P.MapDep "GLU" (deb "libglu1-mesa-dev"),
                  P.MapDep "glut" (deb "freeglut3-dev"),
                  P.MapDep "pcre" (deb "libpcre3-dev"),
                  P.MapDep "libpcre" (deb "libpcre3-dev"),
                  P.MapDep "m" (deb "libc6-dev"),
                  P.MapDep "X11" (deb "libx11-dev"),
                  P.MapDep "Xi" (deb "libxi-dev"),
                  P.MapDep "Xxf86vm" (deb "libxxf86vm-dev"),
                  P.MapDep "pthread" (deb "libc6-dev"),
                  P.MapDep "Xrandr" (rel ("libxrandr-dev" ++
                                          case baseRelease release of
                                            BaseRelease _ (ReleaseName "wheezy") -> " (= 2:1.3.2-2+deb7u1)"
                                            BaseRelease _ (ReleaseName "precise") -> " (= 2:1.3.2-2ubuntu0.2)"
                                            _ -> "")),
                  -- the libxrandr-dev-lts-quantal package installs
                  -- /usr/lib/x86_64-linux-gnu/libXrandr_ltsq.so.
                  -- P.MapDep "Xrandr_ltsq" (deb "libxrandr-dev-lts-quantal"),
                  P.MapDep "freetype" (deb "libfreetype6-dev"),
                  P.MapDep "icuuc" (deb "libicu-dev"),
                  P.MapDep "icui18n" (deb "libicu-dev"),
                  P.MapDep "icudata" (deb "libicu-dev"),
                  P.MapDep "gtk2hsC2hs" (deb "gtk2hs-buildtools"),
                  P.MapDep "gtk2hsHookGenerator" (deb "gtk2hs-buildtools"),
                  P.MapDep "gtk2hsTypeGen" (deb "gtk2hs-buildtools")
                 ]
      deb s = [[Rel (BinPkgName s) Nothing Nothing]]
      rel s = either (error $ "Parse error in debian relations: " ++ show s) id (parseRelations s)

applyEpochMap :: Monad m => TSt m ()
applyEpochMap =
    packageMap %= (Map.map f)
    where
      f :: P.Package -> Package
      f x = x {P._flags = P._flags x ++ [ P.Epoch "HTTP" 1, P.Epoch "HaXml" 1 ]}

applyExecMap :: Monad m => TSt m ()
applyExecMap =
    packageMap %= Map.map f
    where
      f :: P.Package -> Package
      f x = x {P._post = List.map (uncurry g) myExecMap ++ P._post x}
      g :: String -> [[Relation]] -> CabalInfo -> CabalInfo
      g p d = (debInfo . execMap) %~ (Map.insert p d)

myExecMap =
    -- mapM_
      -- (\(p, dep) -> modify ((debInfo . execMap) %~ (Map.insert p dep)))
      [("cpphs",  [[Rel (BinPkgName "cpphs") Nothing Nothing]]),
       ("ghc",    [[Rel (BinPkgName "ghc") Nothing Nothing]]),
       ("happy",  [[Rel (BinPkgName "happy") Nothing Nothing]]),
       ("alex",   [[Rel (BinPkgName "alex") Nothing Nothing]]),
       ("hsx2hs", [[Rel (BinPkgName "hsx2hs") Nothing Nothing]]),
       ("cpphs",  [[Rel (BinPkgName "cpphs") Nothing Nothing]]),
       ("hsc2hs",  [[Rel (BinPkgName "ghc") Nothing Nothing]]),
       ("gtk2hsC2hs",  [[Rel (BinPkgName "gtk2hs-buildtools") Nothing Nothing]]),
       ("gtk2hsHookGenerator",  [[Rel (BinPkgName "gtk2hs-buildtools") Nothing Nothing]]),
       ("gtk2hsTypeGen", [[Rel (BinPkgName "gtk2hs-buildtools") Nothing Nothing]]),
       ("mysql_config", [[Rel (BinPkgName "libmysqlclient-dev") Nothing Nothing]]),
       ("pcre", [[Rel (BinPkgName "libpcre3-dev") Nothing Nothing]])
      ]

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
