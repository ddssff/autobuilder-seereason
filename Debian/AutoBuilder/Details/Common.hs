{-# LANGUAGE OverloadedStrings, RankNTypes, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import Control.Applicative (pure, (<$>))
import Control.Lens (use, Lens', (%=))
import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.String (IsString(fromString))
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (flag, Package(Package, spec), TSt)
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeBaseName)

import Control.Monad.State (get)
import Debian.AutoBuilder.Types.Packages as P (TargetState(release), PackageFlag, hackage, debianize, git)
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

named :: String -> [P.Packages] -> TSt P.Packages
named s = pure . P.Named (fromString s) . P.Packages

-- | Suitable flags for ghcjs library packages.  Won't work
-- for anything complicated (like happstack-ghcjs-client.)
ghcjs_flags :: TSt P.Package -> TSt P.Package
ghcjs_flags mp =
    mp >>= \ p ->
    mp `putSrcPkgName` makeSrcPkgName (P.spec p)
       `flag` P.CabalDebian ["--ghcjs"]
       -- `flag` P.CabalDebian ["--source-package-name=" <> makeSrcPkgName (P.spec p)]
       `flag` P.BuildDep "libghc-cabal-dev" -- Not libghcjs-cabal-dev?
       `flag` P.BuildDep "ghcjs"
       `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.3)"

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize'' p s) = fromMaybe (makeSrcPkgName p) s
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ map toLower (takeBaseName url)
makeSrcPkgName (Cd dir _) = "ghcjs-" ++ map toLower (takeBaseName dir)
makeSrcPkgName (Darcs path) = "ghcjs-" ++ map toLower (takeBaseName path)
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: TSt Package -> String -> TSt Package
putSrcPkgName mp name = (\ p -> p {spec = putSrcPkgName' (spec p) name}) <$> mp

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize'' cabal _) name = Debianize'' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
-- More - we need a traversal - is it Typeable yet?
putSrcPkgName' p _ = p

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str

skip :: Reason -> TSt P.Package -> TSt P.Package
skip _ _ = zero

newtype Reason = Reason String

broken :: TSt P.Package -> TSt P.Package
broken _ = zero

zero = pure $ Package Zero []

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"
-- seereason :: String
-- seereason = "http://src.seereason.com"

ghcFlags p = p `relax` "ghc"
               `relax` "happy"
               `relax` "alex"
               `relax` "xsltproc"
               `relax` "debhelper"
               `relax` "quilt"
               `relax` "python-minimal"
               `relax` "libgmp-dev"

rel :: Release -> a -> a -> a
rel release precise quantal =
    case baseRelease release of
      Quantal -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Package -> P.PackageFlag -> P.Package
ghc74flag p _ = p

sflag :: TSt Package -> PackageFlag -> TSt Package
sflag mp fl = (baseRelease . release <$> get) >>= \ r -> (case r of Squeeze -> flag; _ -> noflag) mp fl
pflag :: TSt Package -> PackageFlag -> TSt Package
pflag mp fl = (baseRelease . release <$> get) >>= \ r -> (case r of Precise -> flag; _ -> noflag) mp fl
tflag :: TSt Package -> PackageFlag -> TSt Package
tflag mp fl = (baseRelease . release <$> get) >>= \ r -> (case r of Trusty -> flag; _ -> noflag) mp fl
qflag :: TSt Package -> PackageFlag -> TSt Package
qflag mp fl = (baseRelease . release <$> get) >>= \ r -> (case r of Quantal -> flag; _ -> noflag) mp fl
wflag :: TSt Package -> PackageFlag -> TSt Package
wflag mp fl = (baseRelease . release <$> get) >>= \ r -> (case r of Wheezy -> flag; _ -> noflag) mp fl
wskip :: TSt P.Package -> TSt P.Package
wskip t = (baseRelease . release <$> get) >>= \ r -> case r of Wheezy -> zero; _ -> t
wonly t = (baseRelease . release <$> get) >>= \ r -> case r of Wheezy -> t; _ -> zero

noflag :: TSt Package -> PackageFlag -> TSt Package
noflag mp _ = mp

relax :: TSt P.Package -> String -> TSt P.Package
relax mp x = (\ p -> p {P.flags = P.flags p ++ [P.RelaxDep x]}) <$> mp

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

hack = debianize . hackage

git' r c = debianize $ git r c
