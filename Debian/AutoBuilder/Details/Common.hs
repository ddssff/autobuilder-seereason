{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, RecordWildCards, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import Control.Lens (at, Lens', over, use, view, (%=))
import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Set as Set (delete, insert)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (deletePackage, flag, PackageId, spec, TSt)
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeBaseName)

import Control.Monad.State (get)
import Debian.AutoBuilder.Types.Packages as P (release, PackageFlag, hackage, debianize, git)
import Debian.Debianize as D
    (CabalInfo, CabalM, execCabalM, debInfo, binaryDebDescription, compilerFlavor, flags, relations, conflicts, replaces)
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

-- | Turn a GHC package into a GHCJS package.
ghcjs_only :: P.PackageId -> TSt P.PackageId
ghcjs_only i = do
  _ <- P.modifyPackage (over P.groups (Set.delete "ghc-libs")) i
  _ <- P.modifyPackage (over P.groups (Set.insert "ghcjs-libs")) i
  Just p <- use (P.packageMap . at i)
  _ <- putSrcPkgName (makeSrcPkgName (view P.spec p)) i
  _ <- flag (P.CabalDebian ["--ghcjs"]) i
  _ <- flag (P.BuildDep "libghc-cabal-dev") i
  _ <- flag (P.BuildDep "ghcjs") i
  -- flag (P.BuildDep "haskell-devscripts (>= 0.8.21.3)") i
  flag P.NoDoc i -- sometimes the ghcjs haddock is super slow

-- | Clone a GHC package and turn the clone into a GHCJS package.
ghcjs_flags :: P.PackageId -> TSt P.PackageId
ghcjs_flags i = do
  j <- P.clonePackage id i
  _ <- ghcjs_only j
  _ <- P.modifyPackage (over P.groups (Set.delete "ghcjs-libs")) i
  _ <- P.modifyPackage (over P.groups (Set.insert "ghc-libs")) i
  return j

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage "haskell-src-exts") = "ghcjs-src-exts"
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize'' p s) = fromMaybe (makeSrcPkgName p) s
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ map toLower (takeBaseName url)
makeSrcPkgName (Cd dir _) = "ghcjs-" ++ map toLower (takeBaseName dir)
makeSrcPkgName (Darcs path) = "ghcjs-" ++ map toLower (takeBaseName path)
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: String -> P.PackageId -> TSt P.PackageId
putSrcPkgName name i = P.modifyPackage (over spec (\x -> putSrcPkgName' x name)) i

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize'' cabal _) name = Debianize'' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
putSrcPkgName' p _ = p

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str

skip :: Reason -> P.PackageId -> TSt ()
skip _ = deletePackage

newtype Reason = Reason String

broken :: P.PackageId -> TSt ()
broken = deletePackage

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"

ghcFlags :: P.PackageId -> TSt P.PackageId
ghcFlags i = relax "ghc" i >>=
             relax "happy" >>=
             relax "alex" >>=
             relax "xsltproc" >>=
             relax "debhelper" >>=
             relax "quilt" >>=
             relax "python-minimal" >>=
             relax "libgmp-dev" >>=
             relax "haskell-devscripts"

rel :: Release -> a -> a -> a
rel r precise quantal =
    case baseRelease r of
      Quantal -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Package -> P.PackageFlag -> P.Package
ghc74flag p _ = p

sflag :: PackageFlag -> PackageId -> TSt PackageId
sflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Squeeze -> flag; _ -> noflag) fl i
pflag :: PackageFlag -> PackageId -> TSt PackageId
pflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Precise -> flag; _ -> noflag) fl i
tflag :: PackageFlag -> PackageId -> TSt PackageId
tflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Trusty -> flag; _ -> noflag) fl i
qflag :: PackageFlag -> PackageId -> TSt PackageId
qflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Quantal -> flag; _ -> noflag) fl i
wflag :: PackageFlag -> PackageId -> TSt PackageId
wflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Wheezy -> flag; _ -> noflag) fl i
wskip :: P.PackageId -> TSt (Maybe P.PackageId)
wskip i = do
  r <- (baseRelease . view release <$> get)
  case r of Wheezy -> deletePackage i >> return Nothing
            _ -> return (Just i)
wonly i = do
  r <- (baseRelease . view release <$> get)
  case r of Wheezy -> return (Just i)
            _ -> deletePackage i >> return Nothing

noflag :: PackageFlag -> PackageId -> TSt PackageId
noflag _ i = return i

relax :: String -> P.PackageId -> TSt P.PackageId
relax x i = P.modifyPackage (over P.flags (++ [P.RelaxDep x])) i

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

hack v name = hackage v name >>= debianize []

git' r c = git r c >>= debianize []
