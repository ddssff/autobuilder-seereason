{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, RecordWildCards, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import Control.Lens (Lens', over, use, view, (%=), set)
import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Map as Map (insertWith)
import Data.Maybe (fromMaybe)
import Data.Set as Set (singleton, union)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (deletePackage, flag, GroupName, Package(Package), newId, pid, spec, TSt)
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeBaseName)

import Control.Monad.State (get)
import Debian.AutoBuilder.Types.Packages as P (release, PackageFlag, hackage, debianize, git)
import Debian.Debianize as D
    (CabalInfo, CabalM, execCabalM, debInfo, compilerFlavor, binaryDebDescription, flags, relations, conflicts, replaces)
import Debian.Relation (BinPkgName(..), Relation(Rel), Relations)
import Debian.Releases (baseRelease, BaseRelease(..), Release(..))
import Distribution.Compiler (CompilerFlavor(GHCJS))
import System.FilePath((</>))

data Build = Production | Testing
build = Production
-- build = Testing

-- repo = "http://src.seereason.com" :: String
localRepo :: String -> String
localRepo home = "file://" ++ home ++ "/darcs"

privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String

happstackRepo = "http://hub.darcs.net/stepcut/happstack" :: String
--happstackRepo = repo ++ "/happstack"

asciiToString :: B.ByteString -> String
asciiToString = map (chr . fromIntegral) . B.unpack

named :: GroupName -> [P.Package] -> TSt ()
named s ps = mapM_ (\p -> P.groups %= Map.insertWith Set.union s (singleton (view pid p))) ps

-- | Suitable flags for ghcjs library packages.  Won't work
-- for anything complicated (like happstack-ghcjs-client.)
ghcjs_flags :: P.Package -> TSt P.Package
ghcjs_flags p = do
  P.clonePackage id p >>=
   putSrcPkgName (makeSrcPkgName (view P.spec p)) >>=
             flag (P.CabalDebian ["--ghcjs"]) >>=
             -- flag (P.CabalDebian ["--source-package-name=" <> makeSrcPkgName (P.spec p)])
             flag (P.BuildDep "libghc-cabal-dev") >>= -- Not libghcjs-cabal-dev?
             flag (P.BuildDep "ghcjs") >>=
             flag (P.BuildDep "haskell-devscripts (>= 0.8.21.3)")

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage "haskell-src-exts") = "ghcjs-src-exts"
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize'' p s) = fromMaybe (makeSrcPkgName p) s
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ map toLower (takeBaseName url)
makeSrcPkgName (Cd dir _) = "ghcjs-" ++ map toLower (takeBaseName dir)
makeSrcPkgName (Darcs path) = "ghcjs-" ++ map toLower (takeBaseName path)
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: String -> Package -> TSt Package
putSrcPkgName name p = P.modifyPackage (over spec (\x -> putSrcPkgName' x name)) p

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize'' cabal _) name = Debianize'' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
-- More - we need a traversal - is it Typeable yet?
putSrcPkgName' p _ = p

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str

skip :: Reason -> P.Package -> TSt ()
skip _ = deletePackage

newtype Reason = Reason String

broken :: P.Package -> TSt ()
broken = deletePackage

zero :: P.Package
zero = Package (toEnum 0) Zero mempty []

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"
-- seereason :: String
-- seereason = "http://src.seereason.com"

ghcFlags :: P.Package -> TSt P.Package
ghcFlags p = relax "ghc" p >>=
             relax "happy" >>=
             relax "alex" >>=
             relax "xsltproc" >>=
             relax "debhelper" >>=
             relax "quilt" >>=
             relax "python-minimal" >>=
             relax "libgmp-dev"

rel :: Release -> a -> a -> a
rel r precise quantal =
    case baseRelease r of
      Quantal -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Package -> P.PackageFlag -> P.Package
ghc74flag p _ = p

sflag :: PackageFlag -> Package -> TSt Package
sflag fl p = (baseRelease . view release <$> get) >>= \ r -> (case r of Squeeze -> flag; _ -> noflag) fl p
pflag :: PackageFlag -> Package -> TSt Package
pflag fl p = (baseRelease . view release <$> get) >>= \ r -> (case r of Precise -> flag; _ -> noflag) fl p
tflag :: PackageFlag -> Package -> TSt Package
tflag fl p = (baseRelease . view release <$> get) >>= \ r -> (case r of Trusty -> flag; _ -> noflag) fl p
qflag :: PackageFlag -> Package -> TSt Package
qflag fl p = (baseRelease . view release <$> get) >>= \ r -> (case r of Quantal -> flag; _ -> noflag) fl p
wflag :: PackageFlag -> Package -> TSt Package
wflag fl p = (baseRelease . view release <$> get) >>= \ r -> (case r of Wheezy -> flag; _ -> noflag) fl p
wskip :: P.Package -> TSt (Maybe P.Package)
wskip p = do
  r <- (baseRelease . view release <$> get)
  case r of Wheezy -> deletePackage p >> return Nothing
            _ -> return (Just p)
wonly p = do
  r <- (baseRelease . view release <$> get)
  case r of Wheezy -> return (Just p)
            _ -> deletePackage p >> return Nothing

noflag :: PackageFlag -> Package -> TSt Package
noflag _ p = return p

relax :: String -> P.Package -> TSt P.Package
relax x p = P.modifyPackage (over P.flags (++ [P.RelaxDep x])) p

gitrepo x = git ("https://github.com/clckwrks" </> x ++ ".git") []
-- repo = "http://hub.darcs.net/stepcut/clckwrks-dev"
-- repo = "http://src.seereason.com/mirrors/clckwrks-dev"

-- | When the newer is installed apt will uninstall older.  However,
-- because newer doesn't provid
-- one another.  The one in the dependency list will be installed and
-- the other uninstalled.  For example, regex-tdfa-rc is a fork of
-- regex-tfda - they can't both be installed at the same time, but you
-- can't build packages that expect regex-tdfa using regex-tdfa-rc.
substitute :: String -> String -> CabalInfo -> CabalInfo
substitute newer older = execCabalM $ do
  prefix <- (\ hc -> case hc of GHCJS -> "libghcjs-"; _ -> "libghc-") <$> use (debInfo . D.flags . compilerFlavor)
  addDeps newer older prefix (\ b -> debInfo . binaryDebDescription b . relations . conflicts)
  addDeps newer older prefix (\ b -> debInfo . binaryDebDescription b . relations . replaces)

-- | Create a flag that tells cabal debian the package @newer@ may
-- substitute for @older@, so that when it is installed the @older@
-- package is uninstalled.  For example, process-listlike has the same
-- API as process-extras, most packages can build with either one.  For
-- this to work the packages must have the same cabal name, which is
-- not usually the case.
--
-- Note that apt will not install older if newer is already installed,
-- because newer looks to apt like it will do the job (due to provides.)
-- The 'replacement' function is probably more suitable in most cases.
replacement :: String -> String -> CabalInfo -> CabalInfo
replacement newer older = execCabalM $ do
  prefix <- (\ hc -> case hc of GHCJS -> "libghcjs-"; _ -> "libghc-") <$> use (debInfo . D.flags . compilerFlavor)
  addDeps newer older prefix (\ b -> debInfo . binaryDebDescription b . relations . conflicts)
  -- If we include the Provides: relationship then we will never
  -- switch between these packages - it will look to apt like whatever
  -- is installed will do the job just fine.  That is not useful
  -- behavior.  This is only useful for a package that has been retired,
  -- has the same cabal package name, but different debian names.
  -- addDeps newer older prefix (\ b -> debInfo . binaryDebDescription b . relations . provides)
  addDeps newer older prefix (\ b -> debInfo . binaryDebDescription b . relations . replaces)

addDeps :: String -> String -> String -> (BinPkgName -> Lens' CabalInfo Relations) -> CabalM ()
addDeps newer older pre lns = do
  addDeps' "-dev"
  addDeps' "-prof"
  addDeps' "-doc"
    where
      addDeps' :: String -> CabalM ()
      addDeps' suf =
          let lns' :: Lens' CabalInfo Relations
              lns' = lns (BinPkgName (pre ++ newer ++ suf)) in
          do lns' %= (++ [[Rel (BinPkgName (pre ++ older ++ suf)) Nothing Nothing]])
             lns' %= (++ [[Rel (BinPkgName (pre ++ older ++ suf)) Nothing Nothing]])
             lns' %= (++ [[Rel (BinPkgName (pre ++ older ++ suf)) Nothing Nothing]])
{-
    ["--conflicts", deps, "--provides", deps, "--replaces", deps]
    where
      deps = intercalate "," [dev name ++ ":" ++ dev orig,
                              prof name ++ ":" ++ prof orig,
                              doc name ++ ":" ++ prof orig]
      dev x = prefix ++ x ++ "-dev"
      prof x = prefix ++ x ++ "-prof"
      doc x = prefix ++ x ++ "-doc"
-}

hack name = hackage name >>= debianize

git' r c = git r c >>= debianize
