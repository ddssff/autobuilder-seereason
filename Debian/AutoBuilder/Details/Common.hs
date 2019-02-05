-- | Targets that are build for both dists (Trusty and Artful.)

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Debian.AutoBuilder.Details.Common
    ( aflag
    , broken
    , broken2
    , ghcjs_only
    , ghcjs_also
    , git'
    , gitrepo
    , gitrepo2
    , hack
    , noTests
    , noDoc
    , noProf
    , pflag
    , privateRepo
    , putSrcPkgName
    , qflag
    , Reason(Reason)
    , replacement
    --, seeReason8
    --, seeReason86
    , sflag
    , skip0
    , skip
    , skip2
    , substitute
    , tflag
    , TSt
    , wflag
    , wskip
    ) where

import Control.Lens (at, Lens', over, use, view, (%=))
--import qualified Data.ByteString as B
import Data.Char (toLower)
--import Data.FileEmbed (embedFile)
--import Data.List (isPrefixOf)
import Data.Map as Map (delete, keys)
import Data.Maybe (fromMaybe)
--import Data.Set as Set (delete, insert)
--import Debian.AutoBuilder.Types.Packages (apply, cd, debdir, {-depends,-} inGroups, packageMap, patch, uri)
import qualified Debian.AutoBuilder.Types.Packages as P
   (clonePackage, {-flags, groups,-} modifyPackage, {-Package,-} PackageFlag(BuildDep, CabalDebian, NoDoc{-, RelaxDep-}), packageMap)
import Debian.AutoBuilder.Types.Packages as P (release, PackageFlag, hackage, debianize, git, {-deletePackage,-} flag, PackageId, spec)
import qualified Debian.AutoBuilder.Types.Packages as P (TSt)
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeBaseName)

import Control.Monad.State (get)
import Debian.Debianize as D
    (CabalInfo, CabalM, execCabalM, debInfo, binaryDebDescription, compilerFlavor, flags, relations, conflicts, replaces)
import Debian.Relation (BinPkgName(..), Relation(Rel), Relations)
import Debian.Releases (DebianRelease(..), HasBaseRelease(baseRelease), ReleaseTree(..), UbuntuRelease(..))
--import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec({-Commit,-} Branch))
import Distribution.Compiler (CompilerFlavor(GHCJS))
import System.FilePath((</>))

-- | A Distro is any organization that provides packages.
-- data MyDistro = SeeReason8 | SeeReason86 deriving (Eq, Show)

type TSt m = P.TSt m

--seeReason8 = Distro "seereason"
--seeReason86 = Distro "seereason86"

-- data Build = Production | Testing
-- build = Production
-- build = Testing

-- repo = "http://src.seereason.com" :: String
-- localRepo :: String -> String
-- localRepo home = "file://" ++ home ++ "/darcs"

privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String

-- happstackRepo = "http://hub.darcs.net/stepcut/happstack" :: String
--happstackRepo = repo ++ "/happstack"

--asciiToString :: B.ByteString -> String
--asciiToString = map (chr . fromIntegral) . B.unpack

#if 0
-- Versions for a no-ghcjs build.
ghcjs_only :: Monad m => P.PackageId -> TSt m ()
ghcjs_only i = deletePackage i

ghcjs_also :: Monad m => P.PackageId -> TSt m P.PackageId
ghcjs_also i = return i

skip2 :: Monad m => Reason -> P.PackageId -> TSt m ()
skip2 _reason i = deletePackage i

broken2 :: Monad m => P.PackageId -> TSt m ()
broken2 i = deletePackage i

#else

-- Versions for a yes-ghcjs build
ghcjs_only :: Monad m => P.PackageId -> TSt m P.PackageId
ghcjs_only i = do
  p <- use (P.packageMap . at i) >>= maybe (error ("ghcjs: no such target: " ++ show i)) return
  _ <- putSrcPkgName (makeSrcPkgName (view P.spec p)) i
  _ <- flag (P.CabalDebian ["--ghcjs"]) i
  _ <- flag (P.BuildDep "libghc-cabal-dev") i
  _ <- flag (P.BuildDep "ghcjs") i
  -- flag (P.BuildDep "haskell-devscripts (>= 0.8.21.3)") j
  _ <- flag P.NoDoc i -- sometimes the ghcjs haddock is super slow
  return i

ghcjs_also :: Monad m => P.PackageId -> TSt m (P.PackageId, P.PackageId)
ghcjs_also i = do
  j <- P.clonePackage id i
  -- Just p <- use (P.packageMap . at i)
  (,) <$> pure i <*> ghcjs_only j

skip2 :: Monad m => Reason -> (P.PackageId, P.PackageId) -> TSt m ()
skip2 _reason (i, j) = deletePackage i >> deletePackage j

broken2 :: Monad m => (P.PackageId, P.PackageId) -> TSt m ()
broken2 (i, j) = deletePackage i >> deletePackage j
#endif
{-
ghcjs :: Monad m => P.PackageId -> TSt m P.PackageId
ghcjs i = do
  p <- use (P.packageMap . at i) >>= maybe (error ("ghcjs: no such target: " ++ show i)) return
  _ <- putSrcPkgName (makeSrcPkgName (view P.spec p)) i
  _ <- flag (P.CabalDebian ["--ghcjs"]) i
  _ <- flag (P.BuildDep "libghc-cabal-dev") i
  _ <- flag (P.BuildDep "ghcjs") i
  -- flag (P.BuildDep "haskell-devscripts (>= 0.8.21.3)") j
  _ <- flag P.NoDoc i -- sometimes the ghcjs haddock is super slow
  return i

ghcjs_also :: Monad m => P.PackageId -> TSt m (P.PackageId, P.PackageId)
ghcjs_also i = do
  j <- P.clonePackage id i
  -- Just p <- use (P.packageMap . at i)
  (,) <$> pure i <*> ghcjs j
-}

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage "haskell-src-exts") = "ghcjs-src-exts"
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize'' p s) = fromMaybe (makeSrcPkgName p) s
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ map toLower (takeBaseName url)
makeSrcPkgName (Cd dir _) = "ghcjs-" ++ map toLower (takeBaseName dir)
makeSrcPkgName (Darcs path) = "ghcjs-" ++ map toLower (takeBaseName path)
makeSrcPkgName (Proc x) = makeSrcPkgName x
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: Monad m => String -> P.PackageId -> TSt m P.PackageId
putSrcPkgName name i = P.modifyPackage (over spec (\x -> putSrcPkgName' x name)) i

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize'' cabal _) name = Debianize'' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
putSrcPkgName' p _ = p

skip0 :: Monad m => Reason -> () -> TSt m ()
skip0 _reason () = return ()

skip :: Monad m => Reason -> P.PackageId -> TSt m ()
skip _ = deletePackage

newtype Reason = Reason String

broken :: Monad m => P.PackageId -> TSt m ()
broken = deletePackage

deletePackage :: Monad m => PackageId -> TSt m ()
deletePackage i = P.packageMap %= Map.delete i

sflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
sflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of DebianRelease Squeeze -> flag; _ -> noflag) fl i
pflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
pflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of UbuntuRelease Precise -> flag; _ -> noflag) fl i
tflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
tflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of UbuntuRelease Trusty -> flag; _ -> noflag) fl i
qflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
qflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of UbuntuRelease Quantal -> flag; _ -> noflag) fl i
wflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
wflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of DebianRelease Wheezy -> flag; _ -> noflag) fl i
aflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
aflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of UbuntuRelease Artful -> flag; _ -> noflag) fl i
wskip :: Monad m => P.PackageId -> TSt m (Maybe P.PackageId)
wskip i = do
  r <- (baseRelease . view release <$> get)
  case r of (DebianRelease Wheezy) -> deletePackage i >> return Nothing
            _ -> return $ Just i

noflag :: Monad m => PackageFlag -> PackageId -> TSt m PackageId
noflag _ i = return i

gitrepo x = git ("https://github.com/clckwrks" </> x ++ ".git") []
gitrepo2 x = git ("https://github.com/seereason" </> x ++ ".git") []
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

noTests :: Monad m => TSt m ()
noTests = use P.packageMap >>= mapM_ (flag (P.CabalDebian ["--no-tests"])) . Map.keys

noDoc :: Monad m => TSt m ()
noDoc = use P.packageMap >>= mapM_ (flag (P.CabalDebian ["--disable-haddock"])) . Map.keys

noProf :: Monad m => TSt m ()
noProf = use P.packageMap >>= mapM_ (flag (P.CabalDebian ["--disable-profiling"])) . Map.keys
