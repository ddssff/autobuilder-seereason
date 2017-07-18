-- | Targets that are build for both dists (Trusty and Artful.)

{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, OverloadedStrings, RankNTypes, RecordWildCards, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Debian.AutoBuilder.Details.Common where

import Control.Lens (at, Lens', over, use, view, (%=))
import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.FileEmbed (embedFile)
import Data.List (isPrefixOf)
import Data.Map as Map (delete, keys)
import Data.Maybe (fromMaybe)
--import Data.Set as Set (delete, insert)
import Debian.AutoBuilder.Types.Packages (apply, cd, debdir, {-depends,-} inGroups, packageMap, patch, uri)
import qualified Debian.AutoBuilder.Types.Packages as P
   (clonePackage, createPackage, flags, {-groups,-} modifyPackage, Package, PackageFlag(BuildDep, CabalDebian, CabalPin, DebVersion, DevelDep, NoDoc, RelaxDep, SkipVersion, UDeb), packageMap)
import Debian.AutoBuilder.Types.Packages as P (release, PackageFlag, apt, darcs, hackage, debianize, git, {-deletePackage,-} flag, PackageId, proc, spec, TSt)
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeBaseName)

import Control.Monad.State (get)
import Debian.Debianize as D
    (CabalInfo, CabalM, execCabalM, debInfo, binaryDebDescription, compilerFlavor, flags, relations, conflicts, replaces)
import Debian.Relation (BinPkgName(..), Relation(Rel), Relations)
import Debian.Releases (baseRelease, BaseRelease(..))
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage {-, Git-}), GitSpec({-Commit,-} Branch))
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

ghcjs :: P.PackageId -> TSt P.PackageId
ghcjs i = do
  p <- use (P.packageMap . at i) >>= maybe (error ("ghcjs: no such target: " ++ show i)) return
  _ <- putSrcPkgName (makeSrcPkgName (view P.spec p)) i
  _ <- flag (P.CabalDebian ["--ghcjs"]) i
  _ <- flag (P.BuildDep "libghc-cabal-dev") i
  _ <- flag (P.BuildDep "ghcjs") i
  -- flag (P.BuildDep "haskell-devscripts (>= 0.8.21.3)") j
  _ <- flag P.NoDoc i -- sometimes the ghcjs haddock is super slow
  return i

ghcjs_also :: P.PackageId -> TSt (P.PackageId, P.PackageId)
ghcjs_also i = do
  j <- P.clonePackage id i
  -- Just p <- use (P.packageMap . at i)
  (,) <$> pure i <*> ghcjs j

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

putSrcPkgName :: String -> P.PackageId -> TSt P.PackageId
putSrcPkgName name i = P.modifyPackage (over spec (\x -> putSrcPkgName' x name)) i

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize'' cabal _) name = Debianize'' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
putSrcPkgName' p _ = p

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str

skip :: Reason -> P.PackageId -> TSt P.PackageId
skip _ = deletePackage

askip :: Reason -> P.PackageId -> TSt P.PackageId
askip _reason p = do
  rel <- baseRelease <$> use release
  case rel of
    Artful -> deletePackage p
    _ -> return p

newtype Reason = Reason String

broken :: P.PackageId -> TSt P.PackageId
broken = deletePackage

deletePackage :: PackageId -> TSt PackageId
deletePackage i = P.packageMap %= Map.delete i >> return i

deletePackage' :: PackageId -> TSt PackageId
deletePackage' i = P.packageMap %= Map.delete i >> return i

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
aflag :: PackageFlag -> PackageId -> TSt PackageId
aflag fl i = (baseRelease . view release <$> get) >>= \ r -> (case r of Artful -> flag; _ -> noflag) fl i
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

commonTargets :: TSt ()
commonTargets = do
  rel <- baseRelease <$> use release
  _acid_state <- git "https://github.com/acid-state/acid-state" [{-Branch "log-inspection"-}] >>=
                 debianize [] >>= aflag (P.DebVersion "0.14.2-3build2") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _adjunctions <- hackage (Just "4.3") "adjunctions" >>= debianize [] >>= aflag (P.DebVersion "4.3-4build3") >>= ghcjs_also
  _aeson <- let version = case rel of
                            -- Idea is to stick to the version shipped with ghcjs,
                            -- but we are already ahead in the trusty repo.
                            Trusty -> {-"0.9.0.1"-} "0.11.2.0"
                            Artful -> "0.11.3.0" in
            hackage (Just version) "aeson" >>= debianize [] >>= flag (P.CabalPin version) >>=
            flag (P.CabalDebian ["--missing-dependency", "libghc-fail-doc"]) >>= aflag (P.DebVersion "0.11.3.0-1build1") >>= inGroups ["important"] {->>= ghcjs_also-}
  _aeson_qq <-  hackage (Just "0.8.2") "aeson-qq" >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _ansi_terminal <- hackage (Just "0.6.2.3") "ansi-terminal" >>= debianize [] >>= aflag (P.DebVersion "0.6.3-1") >>= ghcjs_also
  _ansi_wl_pprint <- hackage (Just "0.6.7.3") "ansi-wl-pprint" >>= aflag (P.DebVersion "0.6.7.3-3build2") >>= debianize [] >>= ghcjs_also
  _applicative_extras <- hackage (Just "0.1.8") "applicative-extras" >>= flag (P.DebVersion "0.1.8-1") >>= debianize [] >>= ghcjs_also
  _archive <- git "https://github.com/seereason/archive" []
             >>= flag (P.CabalDebian ["--default-package", "archive"])
             >>= inGroups ["autobuilder-group", "important"] >>= debianize []
  _asn1_data <- hackage (Just "0.7.2") "asn1-data" >>= debianize []
  _asn1_encoding <-  (hackage (Just "0.9.4") "asn1-encoding") >>= debianize [] >>= aflag (P.DebVersion "0.9.5-1") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _asn1_parse <-  (hackage (Just "0.9.4") "asn1-parse") >>= debianize [] >>= aflag (P.DebVersion "0.9.4-3build2") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _asn1_types <-  (hackage (Just "0.3.2") "asn1-types") >>= debianize [] >>= aflag (P.DebVersion "0.3.2-3build1") >>= ghcjs_also
  _async <-  (hackage (Just "2.1.1.1") "async") >>= debianize [] >>= aflag (P.DebVersion "2.1.1.1-1")
  _atp_haskell <-  (git "https://github.com/seereason/atp-haskell" []) >>= debianize [] >>= inGroups ["seereason", "th-path", "important"] >>= ghcjs_also
  _attoparsec <-  (hackage (Just "0.13.0.2") "attoparsec") >>= debianize [] >>= aflag (P.DebVersion "0.13.1.0-3build2") >>= inGroups ["important"]
  _authenticate <-  (hackage (Just "1.3.3.2") "authenticate") >>= debianize [] >>= aflag (P.DebVersion "1.3.3.2-3build2") >>= inGroups ["authenticate", "important"]
  _autobuilder <-  (git "https://github.com/ddssff/autobuilder" []) >>= debianize []
                  >>= flag (P.CabalDebian [ "--source-package-name", "autobuilder" ])
                  >>= inGroups ["autobuilder-group", "important"]
  _autobuilder_seereason <-  (git "https://github.com/ddssff/autobuilder-seereason" []) >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _base_compat <-  (hackage (Just "0.9.3") "base-compat") >>= debianize [] >>= aflag (P.DebVersion "0.9.3-1") >>= ghcjs_also
  _base_orphans <-  (hackage (Just "0.5.4") "base-orphans") >>= debianize [] >>= aflag (P.DebVersion "0.5.4-3build1") >>= ghcjs_also
  _base64_bytestring <-  (hackage (Just "1.0.0.1") "base64-bytestring" >>= tflag (P.DebVersion "1.0.0.1-1")) >>= debianize [] >>= aflag (P.DebVersion "1.0.0.1-8build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _bifunctors <-  (hackage (Just "5.4.1") "bifunctors") >>= debianize [] >>= aflag (P.DebVersion "5.4.2-1build2") >>= ghcjs_also
  _blaze_builder <- hackage (Just "0.4.0.2") "blaze-builder" >>= debianize [] >>= aflag (P.DebVersion "0.4.0.2-3build2") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _blaze_html <-  (hackage (Just "0.8.1.3") "blaze-html") >>= debianize [] >>= aflag (P.DebVersion "0.8.1.3-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _blaze_markup <-  (hackage (Just "0.7.1.0") "blaze-markup") >>= debianize [] >>= aflag (P.DebVersion "0.7.1.1-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _boomerang <-  (hackage (Just "1.4.5.2") "boomerang") >>= debianize [] >>= aflag (P.DebVersion "1.4.5.2-3build2") >>= ghcjs_also
  _byteable <-  (hackage (Just "0.1.1") "byteable" >>= tflag (P.DebVersion "0.1.1-1")) >>= debianize [] >>= aflag (P.DebVersion "0.1.1-7build1") >>= ghcjs_also
  _bytestring_builder <- hackage (Just "0.10.8.1.0") "bytestring-builder" >>= flag P.NoDoc >>= debianize [] >>= ghcjs_also
  _bzlib <-  (hackage (Just "0.5.0.5") "bzlib" >>= flag (P.DevelDep "libbz2-dev")) >>= debianize [] >>= aflag (P.DebVersion "0.5.0.5-6build1") >>= ghcjs_also
  _cabal_debian <- git "https://github.com/ddssff/cabal-debian" [] >>=
                   -- If we run cabal-debian it will find debian/Debianize.hs, and then
                   -- it will try to import src/Debian/Debianize/Optparse.hs, which has
                   -- #if directives that will fail.  Just use the debianization in git.
                   -- debianize ["--native", "--executable", "cabal-debian"] >>=
                   inGroups ["autobuilder-group", "important"]
  _cereal <-  (hackage (Just "0.5.3.0") "cereal" {->>= flag (P.CabalPin "0.4.1.1")-}) >>= debianize [] >>= aflag (P.DebVersion "0.5.4.0-1") >>= ghcjs_also -- Concerns about migration in 0.5
  _clckwrks_cli <-  (gitrepo "clckwrks-cli") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_dot_com <- gitrepo "clckwrks-dot-com" >>=
                                 -- This is a change that only relates to the autobuilder
                                 patch $(embedFile "patches/clckwrks-dot-com.diff") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_bugs <-  (gitrepo "clckwrks-plugin-bugs"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_ircbot <-  (gitrepo "clckwrks-plugin-ircbot"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_media <-  (gitrepo "clckwrks-plugin-media"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_page <-  (gitrepo "clckwrks-plugin-page"
                             -- `patch` $(embedFile "patches/clckwrks-plugin-page.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks <-
    P.createPackage (Debianize'' (Patch (DataFiles (DataFiles
                                                  ({-Git "https://github.com/clckwrks/clckwrks" []-} Hackage "clckwrks")
                                                  (Uri "https://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                       "e211065e573ea0239d6449882c9d860d")
                                                  "jstree")
                                                 (Uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                                                      "c88c72230de1fa3c701187b8afba5e52" {-previouly: "5eecb009ae16dc54f261f31da01dbbac", then "a6d5fdbbcb076dd9385dd2135dbfb589"-})
                                                 "json2")
                                      $(embedFile "patches/clckwrks.diff"))
                               Nothing)
                  [P.BuildDep "hsx2hs"]
                  [] >>= inGroups ["clckwrks", "important", "testtarget"]
  _clckwrks_theme_bootstrap <-  (gitrepo "clckwrks-theme-bootstrap" >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_theme_clckwrks <-  (gitrepo "clckwrks-theme-clckwrks" >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clock <- hackage (Just "0.7.2") "clock" >>= debianize [] >>= aflag (P.DebVersion "0.7.2-3build1") >>= ghcjs_also
  _cmark <-  (hackage (Just "0.5.5.1") "cmark") >>= debianize [] >>= aflag (P.DebVersion "0.5.5.1-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _comonad <- hackage (Just "5") "comonad" >>=
                     apply (replacement "comonad" "comonad-transformers") >>=
                     apply (replacement "comonad" "comonad-fd") >>= debianize [] >>= aflag (P.DebVersion "5.0.1-1build2") >>= ghcjs_also
  _conduit <-  (hackage (Just "1.2.10") "conduit") >>= debianize [] >>= aflag (P.DebVersion "1.2.10-2build1") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _conduit_extra <-  (hackage (Just "1.1.13.2") "conduit-extra") >>= debianize [] >>= aflag (P.DebVersion "1.1.16-1build1") >>= inGroups ["conduit", "important", "servant"] >>= ghcjs_also
  _connection <-  (hackage (Just "0.2.5") "connection") >>= debianize [] >>= aflag (P.DebVersion "0.2.8-1build2") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _contravariant <-  (hackage (Just "1.4") "contravariant") >>= debianize [] >>= aflag (P.DebVersion "1.4-3build2") >>= ghcjs_also
  _cookie <-  (hackage (Just "0.4.2.1") "cookie") >>= debianize [] >>= aflag (P.DebVersion "0.4.2.1-3build2") >>= inGroups ["authenticate", "important"]
  _cpphs <-  (hackage (Just "1.20.1") "cpphs") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _crypto_api <-  (hackage (Just "0.13.2") "crypto-api" >>= qflag (P.DebVersion "0.10.2-1build3")) >>= debianize [] >>= aflag (P.DebVersion "0.13.2-7build2") >>= ghcjs_also
               -- The certificate package may need to be updated for version 0.4
  _cryptohash <-  (hackage (Just "0.11.9") "cryptohash") >>= debianize [] >>= aflag (P.DebVersion "0.11.9-4") >>= inGroups ["important"] >>= ghcjs_also
  _cryptohash_cryptoapi <-  (hackage (Just "0.1.4") "cryptohash-cryptoapi") >>= debianize [] >>= aflag (P.DebVersion "0.1.4-3build3") >>= inGroups ["happstack", "important"]
  _cryptonite <- hackage (Just "0.24") "cryptonite" >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _css_text <- hackage (Just "0.1.2.2") "css-text" >>= debianize [] >>= aflag (P.DebVersion "0.1.2.2-3build2") >>= inGroups ["important"]
  _data_default_class <- hackage (Just "0.1.2.0") "data-default-class" >>= debianize [] >>= aflag (P.DebVersion "0.1.2.0-2build1") >>= ghcjs_also
  _data_default <- hackage (Just "0.7.1.1") "data-default" >>= debianize [] >>= aflag (P.DebVersion "0.7.1.1-2build1") >>= ghcjs_also
  _data_default_instances_base <- hackage (Just "0.1.0.1") "data-default-instances-base" >>= debianize [] >>= aflag (P.DebVersion "0.1.0.1-2build1") >>= ghcjs_also
  _data_default_instances_containers <-  (hackage (Just "0.0.1") "data-default-instances-containers") >>= debianize [] >>= aflag (P.DebVersion "0.0.1-7build1") >>= ghcjs_also
  _data_default_instances_dlist <-  (hackage (Just "0.0.1") "data-default-instances-dlist") >>= debianize [] >>= aflag (P.DebVersion "0.0.1-7build1") >>= ghcjs_also
  _data_default_instances_old_locale <-  (hackage (Just "0.0.1") "data-default-instances-old-locale") >>= debianize [] >>= aflag (P.DebVersion "0.0.1-7build1") >>= ghcjs_also
  _debian_haskell <- git "https://github.com/ddssff/debian-haskell" [] >>= {-debianize [] >>=-} flag (P.RelaxDep "cabal-debian") >>= inGroups ["autobuilder-group", "important"]
  _debian_repo <- git "https://github.com/ddssff/debian-repo" [] >>= inGroups ["autobuilder-group", "important"]
  --_diff <- hackage (Just "0.3.4") "Diff" >>= debianize [] >>= inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _diff <- git "https://github.com/seereason/Diff" [] >>= debianize [] >>= aflag (P.DebVersion "0.3.4-2build1") >>= inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _digest <-  (hackage (Just "0.0.1.2") "digest") >>= debianize [] >>= aflag (P.DebVersion "0.0.1.2-7build1") >>= ghcjs_also
  _digestive_functors <-  (hackage (Just "0.2.1.0") "digestive-functors" >>= flag (P.CabalPin "0.2.1.0")) >>= debianize [] >>= inGroups ["seereason", "important"]  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
      -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
  _digestive_functors_happstack <-  (git "https://github.com/seereason/digestive-functors" []
                                            >>= cd "digestive-functors-happstack"
                                            >>= flag (P.DebVersion "0.1.1.5-2")) >>= debianize [] >>= inGroups ["digestive-functors", "appraisalscribe", "important"]
  _digestive_functors_hsp <-  (darcs ("http://src.seereason.com/digestive-functors-hsp") >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["seereason", "important"]
  _distributive <-  (hackage (Just "0.5.0.2") "distributive") >>= debianize [] >>= aflag (P.DebVersion "0.5.2-1") >>= ghcjs_also
  _doctemplates <- hackage (Just "0.1.0.2") "doctemplates" >>= debianize [] >>= ghcjs_also

  _edit_distance_vector <- hackage Nothing "edit-distance-vector" >>= debianize [] >>= ghcjs_also
  _microlens <- hackage (Just "0.4.8.0") "microlens" >>= debianize [] >>= aflag (P.DebVersion "0.4.8.0-1") >>= ghcjs_also
  _bytestring_conversion <- hackage (Just "0.3.1") "bytestring-conversion" >>= debianize [] >>= aflag (P.DebVersion "0.3.1-3build3") >>= ghcjs_also
  _http_media <- hackage (Just "0.6.4") "http-media" >>= debianize [] >>= aflag (P.DebVersion "0.6.4-3build3") >>= ghcjs_also
  _string_conversions <- hackage (Just "0.4.0.1") "string-conversions" >>= debianize [] >>= aflag (P.DebVersion "0.4.0.1-1build1") >>= ghcjs_also
  _vault <- hackage (Just "0.3.0.7") "vault" >>= debianize [] >>= aflag (P.DebVersion "0.3.0.7-1build1") >>= ghcjs_also
  _double_conversion <- hackage (Just "2.0.2.0") "double-conversion" >>= debianize [] >>= aflag (P.DebVersion "2.0.2.0-1build1") >>= ghcjs_also
  _websockets <- hackage (Just "0.11.2.0") "websockets" >>= debianize [] >>= ghcjs_also
  _happstack_websockets <- git "https://github.com/seereason/happstack-websockets" [] >>= debianize [] >>= ghcjs
  -- _websockets <- git "https://github.com/jaspervdj/websockets.git" [{-Commit "e2b9c0cef1402ffe8f0cc8e1bbfeecbd647cc2d" is version 0.9.7-}] >>= debianize []

  _email_validate <-  (hackage (Just "2.2.0") "email-validate") >>= debianize [] >>= aflag (P.DebVersion "2.2.0-3build2") >>= inGroups ["important"]
  _enclosed_exceptions <- hackage (Just "1.0.2") "enclosed-exceptions" >>= aflag (P.DebVersion "1.0.2-3build2") >>= debianize [] >>= inGroups ["ghcjs-comp"] >>= ghcjs_also
  _entropy <-  (hackage (Just "0.3.7") "entropy") >>= debianize [] >>= aflag (P.DebVersion "0.3.7-5build1") >>= ghcjs_also
  _exceptions <-  (hackage (Just "0.8.3") "exceptions") >>= debianize [] >>= aflag (P.DebVersion "0.8.3-4") >>= ghcjs_also
  _extra <-  (hackage (Just "1.5") "extra") >>= debianize [] >>= ghcjs_also
  -- In ghc-8 this has no modules, so use NoDoc to avoid a haddock error
  _fail <- hackage (Just "4.9.0.0") "fail" >>= flag (P.BuildDep "hscolour") >>= debianize [] >>= flag P.NoDoc >>= aflag (P.DebVersion "4.9.0.0-1build2") >>= ghcjs_also
  _fb <- git "https://github.com/ddssff/fb.git" [] >>= debianize [] >>= aflag (P.DebVersion "1.0.13-3build2") >>= inGroups [ "authenticate", "appraisalscribe", "important"]
  _fgl <-  (hackage (Just "5.5.3.0") "fgl") >>= debianize [] >>= aflag (P.DebVersion "5.5.3.1-1") >>= inGroups ["platform"] >>= ghcjs_also
  _filemanip <- git "https://github.com/ddssff/filemanip" [] >>= debianize [] >>= ghcjs_also
  _filemanip_extra <- git "https://github.com/seereason/filemanip-extra" [] >>= debianize [] >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _fmlist <- hackage Nothing "fmlist" >>= debianize [] >>= aflag (P.DebVersion "0.9-6build1") >>= ghcjs_also
  _fmlist <- hackage Nothing "foundation" >>= debianize [] >>= ghcjs_also
  _free <-  (hackage (Just "4.12.4") "free") >>= debianize [] >>= aflag (P.DebVersion "4.12.4-3build3") >>= ghcjs_also
  _generic_deriving <-  (hackage (Just "1.10.7") "generic-deriving") >>= debianize [] >>= aflag (P.DebVersion "1.11.2-1") >>= ghcjs_also
  _ghcjs <-
      case rel of
        Trusty -> git "https://github.com/ddssff/ghcjs-debian" [] >>= relax "cabal-install" >>= inGroups ["ghcjs-comp"]
        Artful -> git "https://github.com/ddssff/ghcjs-debian" [Branch "ghc-8.0"] >>= inGroups ["ghcjs-comp"]
  _ghcjs_dom <- hackage (Just "0.2.4.0" {-"0.7.0.4"-} {-"0.4.0.0"-}) "ghcjs-dom" >>=
                flag (P.CabalPin "0.2.4.0") >>=
                debianize [] >>=
                inGroups ["glib"] >>= ghcjs
  _ghcjs_dom_hello <- hackage (Just "3.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>=
                      ghcjs >>=
                      skip (Reason "see cairo and glib")
  _ghcjs_jquery <-  git "https://github.com/ghcjs/ghcjs-jquery" [] >>=
                    debianize [] {-`putSrcPkgName` "ghcjs-ghcjs-jquery"-} >>=
                    patch $(embedFile "patches/ghcjs-jquery.diff") >>=
                    ghcjs
  _groom <-  (hackage (Just "0.1.2") "groom") >>= debianize [] >>= ghcjs_also
  _haddock_library <-
      case rel of
        Artful -> hackage (Just "1.4.3") "haddock-library" >>= flag (P.DebVersion "1.4.3-1") >>= debianize [] >>= inGroups ["ghcjs-comp"] >>= ghcjs_also
        Trusty -> hackage (Just "1.2.1") "haddock-library" >>= debianize [] >>= inGroups ["ghcjs-comp"] >>= ghcjs_also
  _happstack_authenticate_0 <-  (git "https://github.com/Happstack/happstack-authenticate-0.git" []
                             >>= flag (P.CabalDebian [ "--debian-name-base", "happstack-authenticate-0",
                                                    "--cabal-flags", "migrate",
                                                    "--executable", "happstack-authenticate-migrate" ])) >>= debianize [] >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_authenticate <- git "https://github.com/Happstack/happstack-authenticate.git" [] >>= debianize [] >>= aflag (P.DebVersion "2.3.4.7-1build3") >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_clckwrks <-  (git ("https://github.com/Happstack/happstack-clckwrks") [] >>=
                             cd "clckwrks-theme-happstack"
                             -- >>= patch $(embedFile "patches/clckwrks-theme-happstack.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _happstack_dot_com <-  (git ("https://github.com/Happstack/happstack-clckwrks") []
                                   >>= cd "happstack-dot-com"
                                   -- This is a change that only relates to the autobuilder
                                   >>= patch $(embedFile "patches/happstack-dot-com.diff")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _happstackDotCom_doc <- darcs ("http://src.seereason.com/happstackDotCom-doc") >>= inGroups ["happstack", "important"]
  _happstack_extra <-  (git "https://github.com/seereason/happstack-extra.git" []) >>= debianize []
  _happstack_fay_ajax <-  (hackage (Just "0.2.0") "happstack-fay-ajax" >>= patch $(embedFile "patches/happstack-fay-ajax.diff")) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
      -- ,  (hackage "fay-hsx" >>= patch $(embedFile "patches/fay-hsx.diff")) >>= debianize []
  _happstack_fay <-  (hackage (Just "0.2.0") "happstack-fay" >>= patch $(embedFile "patches/happstack-fay.diff")) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _happstack_foundation <-  (git "https://github.com/Happstack/happstack-foundation.git" []) >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_foundation_example <-
       (git "https://github.com/Happstack/happstack-foundation.git" []
                                   >>= cd "examples/ControlVAuth"
                                   >>= flag (P.CabalDebian ["--source-package-name", "happstack-foundation-example",
                                                         "--default-package", "happstack-foundation-example"])) >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_hsp <-  (git "https://github.com/Happstack/happstack-hsp.git" []) >>= debianize [] >>= aflag (P.DebVersion "7.3.7.2-1build1") >>= inGroups ["happstack", "lens", "important"]
  _happstack_jmacro <-  (git "https://github.com/Happstack/happstack-jmacro.git" []) >>= debianize [] >>= aflag (P.DebVersion "7.0.11-3build2") >>= inGroups ["happstack", "lens", "important"]
  _happstack_lite <-  (hackage (Just "7.3.6") "happstack-lite") >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_plugins <-  (hackage (Just "7.0.2") "happstack-plugins" >>= patch $(embedFile "patches/happstack-plugins.diff")) >>= debianize [] >>= skip (Reason "Needs plugins-auto")
  _happstack_scaffolding <-  (git "https://github.com/seereason/happstack-scaffolding" [] >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["seereason", "important"]
  _happstack_search <- darcs ("http://src.seereason.com/happstack-search") >>= inGroups ["happstack", "important"]
              -- ,  (hackage (Just "7.4.6.2") "happstack-server") >>= debianize []
  _happstack_server <- git "https://github.com/Happstack/happstack-server" [] >>=
                       debianize [] >>=
                       flag (P.DebVersion "7.4.6.4-1build1") >>= inGroups ["happstack", "important"]
  _happstack_server_ghcjs <- -- P.clonePackage id _happstack_server >>= ghcjs_only
                             git "https://github.com/seereason/happstack-server" [Branch "websockets"] >>=
                             flag (P.CabalDebian ["--ghcjs"]) >>= flag (P.BuildDep "libghc-cabal-dev") >>=
                             flag (P.CabalDebian ["--cabal-flags", "-template_haskell"]) >>= flag (P.BuildDep "ghcjs") >>= flag P.NoDoc >>=
                             debianize [] >>= inGroups ["happstack", "important"] {- >>= \i -> use (packageMap . at i) >>= \pkg -> trace ("Package: " ++ show pkg) (return i) -}
  _happstack_server_tls <-  (git "https://github.com/Happstack/happstack-server-tls" []) >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_static_routing <-  (hackage (Just "0.4.2") "happstack-static-routing") >>= debianize [] {->>= inGroups ["happstack", "important"]-} >>= skip (Reason "compile error")
  _happstack_util <- hackage (Just "6.0.3") "happstack-util" >>=
                     patch $(embedFile "patches/happstack-util.diff") >>=
                     flag (P.DebVersion "6.0.3-1") >>=
                     debianize [] >>=
                     inGroups ["happstack", "important"]
  _haskell_either <-  (hackage (Just "4.4.1.1") "either") >>= aflag (P.DebVersion "4.4.1.1-3build3") >>= debianize []
  _haskell_help <- git ("https://github.com/seereason/sr-help") [] >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _haskell_lexer <- hackage (Just "1.0.1") "haskell-lexer" >>= aflag (P.DebVersion "1.0.1-3build1") >>= debianize [] >>= ghcjs_also
  _haTeX <- hackage Nothing "HaTeX" >>=
            -- git "https://github.com/seereason/HaTeX" [Branch "linebreak"] >>= -- adds mapLaTeXT, fix \\ format
            -- patch $(embedFile "patches/HaTeX-texty.diff") >>=
            -- patch $(embedFile "patches/HaTeX-doc.diff") >>=
            debianize [] >>= ghcjs_also
  _hJavaScript <- hackage (Just "0.4.7") "HJavaScript"
                  >>= patch $(embedFile "patches/hjavascript.diff")
                  >>= tflag (P.DebVersion "0.4.7-6")
                  >>= debianize []
               -- Not used, and not building.
               -- ,  (hackage (Just "0.3.5") "hoauth") >>= debianize []
  _hJScript <- git "https://github.com/seereason/HJScript" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _highlighting_kate <- hackage (Just "0.6.4") "highlighting-kate" >>= debianize [] >>= aflag (P.DebVersion "0.6.4-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _hostname <- hackage (Just "1.0") "hostname"
                              >>= wflag (P.DebVersion "1.0-4")
                              >>= pflag (P.DebVersion "1.0-4build1")
                              >>= qflag (P.DebVersion "1.0-4build3")
                              >>= sflag (P.DebVersion "1.0-1~hackage1")
                              >>= tflag (P.DebVersion "1.0-6") >>= debianize []
                              >>= aflag (P.DebVersion "1.0-12build1") >>= ghcjs_also
               -- The Sid package has no profiling libraries, so dependent packages
               -- won't build.  Use our debianization instead.  This means keeping
               -- up with sid's version.
  _hourglass <-  (hackage (Just "0.2.10") "hourglass") >>= debianize [] >>= aflag (P.DebVersion "0.2.10-3build1") >>= ghcjs_also
  _hslogger <-  (hackage (Just "1.2.10") "hslogger") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _hslua <-  (hackage (Just "0.4.1") "hslua") >>= debianize [] >>= aflag (P.DebVersion "0.4.1-10build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _hsx_jmacro <-  (git "https://github.com/Happstack/hsx-jmacro.git" []) >>= debianize [] >>= aflag (P.DebVersion "7.3.8-3build2") >>= inGroups ["happstack", "lens", "important"]
  _hsx2hs <- {- git "https://github.com/seereason/hsx2hs.git" [] -}
             {- git "file:///home/dsf/git/hsx2hs" [] -}
             hackage (Just "0.14.0") "hsx2hs" >>=
             patch $(embedFile "patches/hsx2hs.diff") >>=
             debianize [] >>=
             inGroups ["happstack", "lens", "important"] >>= ghcjs
  flag (P.CabalDebian ["--executable", "hsx2hs"]) _hsx2hs
  _html <-  (hackage (Just "1.0.1.2") "html"
                             >>= tflag (P.DebVersion "1.0.1.2-7")
                             >>= pflag (P.DebVersion "1.0.1.2-5")) >>= debianize [] >>= aflag (P.DebVersion "1.0.1.2-13build1") >>= inGroups ["platform"] >>= ghcjs_also
  _http <-  (hackage Nothing "HTTP") >>= debianize [] >>= aflag (P.DebVersion "1:4000.3.7-1build1") >>= inGroups ["platform"] >>= ghcjs_also
  _http_api_data <- hackage (Just "0.2.4") "http-api-data" >>= debianize [] >>= aflag (P.DebVersion "0.2.4-5build2") >>= ghcjs_also
  _http_client <-  (hackage (Just "0.4.30" {-"0.5.4"-}) "http-client") >>= debianize [] >>= aflag (P.DebVersion "0.4.31.2-1build1") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _http_client_tls <- hackage (Just "0.2.4.1" {-"0.3.3"-}) "http-client-tls" >>= debianize [] >>= aflag (P.DebVersion "0.2.4.1-3build2") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _http_conduit <-  (hackage (Just "2.1.11" {-"2.2.2.1"-}) "http-conduit") >>= debianize [] >>= aflag (P.DebVersion "2.1.11-4") >>= inGroups ["conduit", "important"] -- fb isn't ready for 2.2
  _http_streams <-  (hackage (Just "0.8.5.3") "http-streams") >>= debianize [] >>= aflag (P.DebVersion "0.8.5.3-1build1") >>= inGroups ["platform", "appraisalscribe", "important"]
  _http_types <- hackage (Just "0.9.1") "http-types" >>= debianize [] >>= aflag (P.DebVersion "0.9.1-3build2") >>= inGroups ["happstack", "important"] -- web-routes specifies << 0.9
  _hUnit <-  (hackage (Just "1.3.1.1") "HUnit") >>= debianize [] >>= aflag (P.DebVersion "1.3.1.2-3build1") >>= inGroups ["platform"] >>= ghcjs_also
  _hxt <-  (hackage (Just "9.3.1.15") "hxt" >>= flag (P.CabalDebian ["--cabal-flags", "network-uri"])) >>= debianize [] >>= aflag (P.DebVersion "9.3.1.16-2build3") >>= ghcjs_also
  _hxt_charproperties <- hackage (Just "9.2.0.1") "hxt-charproperties" >>= debianize [] >>= aflag (P.DebVersion "9.2.0.1-6build1") >>= ghcjs_also
  _hxt_regex_xmlschema <- hackage (Just "9.2.0.2") "hxt-regex-xmlschema" >>= debianize [] >>= aflag (P.DebVersion "9.2.0.3-2build2") >>= ghcjs_also
  _hxt_unicode <-  (hackage (Just "9.0.2.4") "hxt-unicode") >>= debianize [] >>= aflag (P.DebVersion "9.0.2.4-6build1") >>= ghcjs_also
  _integer_logarithms <- hackage (Just "1.0.1") "integer-logarithms" >>= debianize [] >>= aflag (P.DebVersion "1.0.1-1") >>= ghcjs_also
  _io_streams <- hackage (Just "1.3.6.1") "io-streams" >>= debianize [] >>= aflag (P.DebVersion "1.3.6.1-1build1") >>= inGroups ["important"] -- http-streams-0.8.4.0 requires io-streams < 1.4
  _irc <-  (hackage (Just "0.6.1.0") "irc") >>= debianize [] >>= aflag (P.DebVersion "0.6.1.0-7build2") >>= inGroups ["important"]
  _ircbot <-  (hackage (Just "0.6.5.1") "ircbot") >>= debianize [] >>= aflag (P.DebVersion "0.6.5.1-1") >>= inGroups ["happstack", "important"]
  _ixset <-  (git "https://github.com/Happstack/ixset.git" []) >>= debianize [] >>= aflag (P.DebVersion "1.0.7-3build2") >>= inGroups ["happstack", "important"] >>= ghcjs_also -- ,  (hackage (Just "1.0.7") "ixset") >>= debianize []
  _ixset_typed <-  (hackage (Just "0.3.1") "ixset-typed") >>= debianize [] >>= aflag (P.DebVersion "0.3.1-3build2") >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _libjs_jcrop <- apt "jessie" "libjs-jcrop" >>= \p ->
                 (baseRelease . view release <$> get) >>= \ r ->
                 case r of
                   Artful -> patch $(embedFile "patches/libjs-jcrop.diff") p >>= proc
                   _ -> skip (Reason "Trusty has libjs-jcrop") p
  _jqueryui18 <- darcs ("http://src.seereason.com/jqueryui18")
  _jmacro <-  (hackage (Just "0.6.14") "jmacro") >>= debianize [] >>= aflag (P.DebVersion "0.6.14-3build2") >>= inGroups ["happstack", "lens", "th-path", "important"] >>= ghcjs_also
  _juicyPixels <- hackage (Just "3.2.8") "JuicyPixels" >>= patch $(embedFile "patches/JuicyPixels.diff") >>= debianize [] >>= aflag (P.DebVersion "3.2.8.1-1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _jwt <-  (hackage (Just "0.7.2") "jwt") >>= debianize [] >>= aflag (P.DebVersion "0.7.2-4build3") >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _kan_extensions <- hackage (Just "5.0.2") "kan-extensions" >>= debianize [] >>= aflag (P.DebVersion "5.0.2-1build2") >>= ghcjs_also
  _lens <- hackage (Just "4.15.1") "lens" >>=
           patch $(embedFile "patches/lens.diff") >>=
           -- git "https://github.com/ekmett/lens" [Commit "950eb5be34fb40bf0111ded6bc91c1ffcd2a786b"] >>=
           apply (replacement "lens" "microlens-compat") >>=
           debianize [] >>= inGroups ["lens", "important"] >>= ghcjs_also
  _lifted_base <-  (hackage (Just "0.2.3.8") "lifted-base") >>= debianize [] >>= aflag (P.DebVersion "0.2.3.10-1") >>= ghcjs_also
  _listLike <- git "https://github.com/JohnLato/ListLike" [] >>=
               flag (P.CabalDebian ["--cabal-flags", "safe"]) >>=
               (if rel == Artful then flag (P.DebVersion "4.5.1-1build1") else return) >>=
               debianize [] >>=
               inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _logic_classes <-  (git "https://github.com/seereason/logic-classes" []) >>= debianize [] >>= inGroups ["seereason", "important"]
  _loop <- hackage (Just "0.3.0") "loop" >>= debianize [] >>= ghcjs_also
  _lucid <- hackage (Just "2.9.8") "lucid" >>= debianize [] >>= aflag (P.DebVersion "2.9.8.1-1build1") >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _matrix <-  (hackage (Just "0.3.5.0") "matrix") >>= debianize [] >>= ghcjs_also
               -- ,  (hackage (Just "0.3") "hlatex") >>= debianize []
  _memory <- hackage (Just "0.14.6") "memory" >>= debianize [] >>= ghcjs_also
  _mime <- git ("https://github.com/seereason/haskell-mime") [] >>= debianize [] >>= inGroups ["autobuilder-group"]
  _mime_types <-  (hackage (Just "0.1.0.7") "mime-types") >>= debianize [] >>= aflag (P.DebVersion "0.1.0.7-3build2") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _mirror <-  (git "https://github.com/seereason/mirror" []
                        >>= flag (P.CabalDebian ["--executable", "debian-mirror"])) >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _mmorph <-  (hackage (Just "1.0.6") "mmorph") >>= debianize [] >>= aflag (P.DebVersion "1.0.9-1") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _monad_control <-  (hackage (Just "1.0.1.0") "monad-control") >>= debianize [] >>= aflag (P.DebVersion "1.0.1.0-3build1") >>= ghcjs_also
  _monad_logger <- hackage (Just "0.3.24") "monad-logger" >>= debianize [] >>= aflag (P.DebVersion "0.3.24-1") >>= inGroups [ "authenticate", "important"]
  _monadRandom <- hackage (Just "0.4.2.3") "MonadRandom" >>= aflag (P.DebVersion "0.4.2.3-3build1") >>= debianize []
  _network <-  (hackage (Just "2.6.3.1") "network") >>= debianize [] >>= aflag (P.DebVersion "2.6.3.1-3build1") >>= inGroups ["platform"] >>= ghcjs_also
  _network_info <-  (hackage (Just "0.2.0.8") "network-info") >>= debianize [] >>= aflag (P.DebVersion "0.2.0.8-3build1") >>= ghcjs_also
  _network_uri <-  (hackage (Just "2.6.1.0") "network-uri") >>= debianize [] >>= aflag (P.DebVersion "2.6.1.0-3build2") >>= ghcjs_also
  -- _nodejs <- uri "https://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz" "5523ec4347d7fe6b0f6dda1d1c7799d5" >>=
  --            debdir (Git "https://github.com/seereason/nodejs-debian" []) >>= inGroups ["ghcjs-comp"]
  _nodejs <- case rel of
               Artful -> uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
                         debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~trusty1.debian.tar.xz" "a93243ac859fae3a6832522b55f698bd") >>=
                         flag (P.RelaxDep "libssl-dev") >>=
                         inGroups ["ghcjs-comp"]
               Trusty -> uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
                         debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~trusty1.debian.tar.xz" "a93243ac859fae3a6832522b55f698bd") >>=
                         flag (P.RelaxDep "libssl-dev") >>=
                         inGroups ["ghcjs-comp"]
  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= patch $(embedFile "patches/old-locale.diff") >>= debianize [] >>= aflag (P.DebVersion "1.0.0.7-5build1") >>= ghcjs_also
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= patch $(embedFile "patches/old-time.diff") >>= debianize [] >>= aflag (P.DebVersion "1.1.0.3-5build1") >>= ghcjs_also
  _openssl_streams <-  (hackage (Just "1.2.1.1") "openssl-streams") >>= debianize [] >>= aflag (P.DebVersion "1.2.1.1-1build1") >>= inGroups ["important", "platform"]
  _optparse_applicative <-  (hackage (Just "0.12.1.0") "optparse-applicative") >>= debianize [] >>= aflag (P.DebVersion "0.12.1.0-3build2") >>= ghcjs_also
  _pandoc <- hackage (Just "1.19.2.1") "pandoc" >>=
             patch $(embedFile "patches/pandoc.diff") >>=
             flag (P.BuildDep "alex") >>=
             flag (P.BuildDep "happy") >>=
             debianize [] >>=
             inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  -- pandoc depends on pandoc-types==1.17
  _pandoc_types <- hackage (Just "1.17.0.5") "pandoc-types" >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _patches_vector <- hackage Nothing "patches-vector" >>= debianize [] >>= ghcjs_also
  _parseargs <-  (hackage (Just "0.2.0.7") "parseargs") >>= debianize [] >>= aflag (P.DebVersion "0.2.0.8-1") >>= ghcjs_also
               -- , apt (rel release "wheezy" "quantal") "haskell-parsec2" >>= patch $(embedFile "patches/parsec2.diff")
  _parsec <-  (hackage (Just "3.1.11") "parsec" >>= apply (substitute "parsec2" "parsec3")) >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _pem <-  (hackage (Just "0.2.2") "pem") >>= debianize [] >>= aflag (P.DebVersion "0.2.2-7build1") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _polyparse <-  (hackage (Just "1.12") "polyparse") >>= debianize [] >>= aflag (P.DebVersion "1.12-3build2") >>= ghcjs_also
  _prelude_extras <-  (hackage (Just "0.4.0.3") "prelude-extras") >>= debianize [] >>= aflag (P.DebVersion "0.4.0.3-3build1") >>= ghcjs_also
  _pretty_show <- hackage (Just "1.6.12") "pretty-show" >>= flag (P.BuildDep "happy") >>= aflag (P.DebVersion "1.6.13-1") >>= debianize [] >>= ghcjs_also
  _process_extras <-
       (git "https://github.com/seereason/process-extras" []) >>= debianize []
                   >>= apply (substitute "process-extras" "process-listlike")
                   >>= inGroups ["autobuilder-group"] >>= ghcjs_also
  _profunctors <-  (hackage (Just "5.2") "profunctors"
                     >>= apply (replacement "profunctors" "profunctors-extras")) >>= debianize []
                     >>= aflag (P.DebVersion "5.2-3build3") >>= ghcjs_also
  _propLogic <-  (git "https://github.com/ddssff/PropLogic" []) >>= debianize []
  _pureMD5 <- hackage (Just "2.1.3") "pureMD5" >>= debianize [] >>= aflag (P.DebVersion "2.1.3-3build3") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _pwstore_purehaskell <-  (hackage (Just "2.1.4") "pwstore-purehaskell"
                              >>= flag (P.SkipVersion "2.1.2")
                              -- >>= patch $(embedFile "patches/pwstore-purehaskell.diff")
                              -- >>= flag (P.DebVersion "2.1-1~hackage1")
                           ) >>= debianize []
               -- Retired
               -- , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
  _quickCheck <- hackage (Just "2.9.2") "QuickCheck" >>= flag (P.BuildDep "libghc-random-prof") {->>= flag (P.CabalDebian ["--no-tests"])-} >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _random <-  (hackage (Just "1.1") "random" >>= flag (P.SkipVersion "1.0.1.3")) >>= aflag (P.DebVersion "1.1-5build1") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also -- 1.1.0.3 fixes the build for ghc-7.4.2 / base < 4.6
  _reflection <-  (hackage (Just "2.1.2") "reflection") >>= debianize [] >>= aflag (P.DebVersion "2.1.2-3build1") >>= ghcjs_also -- avoid rebuild
  _reform_blaze <- git "https://github.com/Happstack/reform-blaze.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform_happstack <- git "https://github.com/Happstack/reform-happstack.git" [] >>= debianize [] >>= aflag (P.DebVersion "0.2.5.1-3build2") >>= inGroups ["happstack", "important"]
  _reform_hsp <- git "https://github.com/Happstack/reform-hsp.git" [] >>= flag (P.BuildDep "hsx2hs") >>= debianize [] >>= aflag (P.DebVersion "0.2.7.1-2build2") >>= inGroups ["happstack", "important"]
  _regex_base <-  (hackage (Just "0.93.2") "regex-base"
                             >>= tflag (P.DebVersion "0.93.2-4")
                             >>= pflag (P.DebVersion "0.93.2-2"))
                             >>= aflag (P.DebVersion "0.93.2-10build1") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _regex_compat <-  (hackage (Just "0.95.1") "regex-compat"
                             >>= pflag (P.DebVersion "0.95.1-2")
                             >>= tflag (P.DebVersion "0.95.1-4"))
                             >>= aflag (P.DebVersion "0.95.1-10build1") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _regex_pcre_builtin <-  (hackage (Just "0.94.4.8.8.35") "regex-pcre-builtin"
                              -- Need to email Audrey Tang <audreyt@audreyt.org> about this.
                              >>= patch $(embedFile "patches/regex-pcre-builtin.diff")
                              >>= flag (P.DevelDep "libpcre3-dev")) >>= debianize [] >>= ghcjs_also
  _regex_posix <-  (hackage (Just "0.95.2") "regex-posix" >>= tflag (P.DebVersion "0.95.2-3")) >>= debianize [] >>= aflag (P.DebVersion "0.95.2-9build1") >>= inGroups ["platform"] >>= ghcjs_also
  _regex_tdfa <-  (hackage (Just "1.2.2") "regex-tdfa"
                          -- Although it might be nice to start using regex-tdfa-rc everywhere
                          -- we are using regex-tdfa, the cabal package names are different so
                          -- packages can't automatically start using regex-tdfa-rc.
                          >>= apply (substitute "regex-tdfa" "regex-tdfa-rc")) >>= debianize [] >>= aflag (P.DebVersion "1.2.2-3build2") >>= ghcjs_also
  _regex_tdfa_text <- hackage (Just "1.0.0.3") "regex-tdfa-text" >>= debianize [] >>= ghcjs_also
  _resourcet <-  (hackage (Just "1.1.7.4") "resourcet") >>= debianize [] >>= aflag (P.DebVersion "1.1.9-1build1") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _safe <-  (hackage (Just "0.3.9") "safe") >>= debianize [] >>= aflag (P.DebVersion "0.3.14-1") >>= ghcjs_also
  _safecopy <- git "https://github.com/acid-state/safecopy" [] >>= debianize [] >>= aflag (P.DebVersion "0.9.3.2-1build1") >>= ghcjs_also
  _scientific <- hackage (Just "0.3.4.11") "scientific" >>= debianize [] >>= ghcjs_also
               -- ,  (hackage (Just "0.4.1.1") "arithmoi" >>= flag (P.BuildDep "llvm-dev")) >>= debianize []
  _seereason_base <- git "https://github.com/seereason/seereason-base" [] >>= debianize [] >>= inGroups ["seereason", "important"]
  _seereason_keyring <- darcs ("http://src.seereason.com/seereason-keyring") >>= flag (P.UDeb "seereason-keyring-udeb")
  _seereason_ports <-  (git "https://github.com/seereason/seereason-ports" []) >>= debianize []
  _semigroupoids <-  (hackage (Just "5.1") "semigroupoids"
                     >>= apply (replacement "semigroupoids" "semigroupoid-extras")) >>= debianize [] >>= aflag (P.DebVersion "5.1-4") >>= ghcjs_also
  _semigroups <-  (hackage (Just "0.18.3") "semigroups") >>= aflag (P.DebVersion "0.18.3-1") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _sendfile <-  (hackage (Just "0.7.9") "sendfile" >>= tflag (P.DebVersion "0.7.9-1")) >>= debianize [] >>= aflag (P.DebVersion "0.7.9-8build1") >>= ghcjs_also
  _servant <- hackage (Just "0.8.1") "servant" >>= debianize [] >>= aflag (P.DebVersion "0.8.1-2build3") >>= ghcjs_also
  _servant_server <- hackage (Just "0.8.1") "servant-server" >>= debianize [] >>= inGroups ["servant-server"] >>= inGroups ["servant"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _set_extra <- hackage Nothing "set-extra" >>= debianize [] >>= aflag (P.DebVersion "1.4-3build1") >>= ghcjs_also
  _sha <-  (hackage (Just "1.6.4.2") "SHA") >>= debianize [] >>= aflag (P.DebVersion "1.6.4.2-6build1") >>= ghcjs_also -- apt (rel release "wheezy" "quantal") "haskell-sha"
  _shakespeare <-  (hackage (Just "2.0.13") "shakespeare") >>= debianize [] >>= aflag (P.DebVersion "2.0.13-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _skylighting <- hackage (Just "0.1.1.5") "skylighting" >>= debianize [] >>= ghcjs_also
  _show_please <- hackage Nothing "show-please" >>= debianize [] >>= ghcjs_also
  _socks <-  (hackage (Just "0.5.5") "socks") >>= debianize [] >>= aflag (P.DebVersion "0.5.5-3build2") >>= ghcjs_also
  _split <- hackage (Just "0.2.3.1") "split" >>= {-patch $(embedFile "patches/split.diff") >>= tflag (P.DebVersion "0.2.2-1") >>=-} debianize [] >>= aflag (P.DebVersion "0.2.3.2-1") >>= ghcjs_also
  _sr_extra <-  (git ("https://github.com/seereason/sr-extra") []
                              -- Don't push out libghc-extra-dev, it now comes from Neil Mitchell's repo
                              {- `apply` (replacement "sr-extra" "Extra") -}
                       ) >>= debianize [] >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _sr_order <- git "https://github.com/seereason/sr-order" [] >>= debianize [] >>= ghcjs_also
  _haskell_src_exts <- hackage (Just "1.18.2") "haskell-src-exts" >>=
                       debianize [] >>=
                       flag (P.BuildDep "happy") >>=
                       inGroups ["important"] >>= ghcjs_also
  -- This goes with haskell-src-exts-1.18.*
  -- _haskell_src_exts_simple <- hackage "haskell-src-exts-simple" >>= debianize []
  _haskell_src_meta <-
      -- git "https://github.com/ddssff/haskell-src-meta" [] >>=
      hackage (Just "0.7.0") "haskell-src-meta" >>=
      debianize [] >>=
      inGroups ["happstack", "important"] >>= ghcjs_also
  _stateVar <-  (hackage (Just "1.1.0.4") "StateVar") >>= debianize [] >>= aflag (P.DebVersion "1.1.0.4-4build1") >>= ghcjs_also
  _streaming_commons <-  (hackage (Just "0.1.15.5") "streaming-commons") >>= debianize [] >>= aflag (P.DebVersion "0.1.17-1build1") >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _system_filepath <-  (hackage (Just "0.4.13.4") "system-filepath") >>= debianize [] >>= aflag (P.DebVersion "0.4.13.4-6build2") >>= ghcjs_also
               -- , P.Package { P.spec = Debianize'' (Patch (Hackage (Just "0.4.4.1") "xml-enumerator") $(embedFile "patches/xml-enumerator.diff")) Nothing , P.flags = [] }
  _syb_with_class <- git "http://github.com/Happstack/syb-with-class" [] >>= debianize [] >>= aflag (P.DebVersion "0.6.1.7-3build1") >>= ghcjs_also
  _tagged <- hackage (Just "0.8.5") "tagged" >>= debianize [] >>= aflag (P.DebVersion "0.8.5-2build1") >>= ghcjs_also
  _tagsoup <-  (hackage (Just "0.14.1") "tagsoup") >>= debianize [] >>= aflag (P.DebVersion "0.14.1-1build1") >>= ghcjs_also
  _tagstream_conduit <-  (hackage (Just "0.5.5.3") "tagstream-conduit") >>= debianize [] >>= aflag (P.DebVersion "0.5.5.3-7build2") >>= inGroups ["conduit", "authenticate", "important"]
  _temporary <-  (hackage (Just "1.2.0.4") "temporary") >>= debianize [] >>= aflag (P.DebVersion "1.2.0.4-3build1") >>= ghcjs_also
  _test_framework <- hackage (Just "0.8.1.1") "test-framework" >>= patch $(embedFile "patches/test-framework.diff") >>= debianize [] >>= aflag (P.DebVersion "0.8.1.1-7build2") >>= ghcjs_also
  _test_framework_hunit <-  (hackage (Just "0.3.0.2") "test-framework-hunit") >>= debianize [] >>= aflag (P.DebVersion "0.3.0.2-3build2") >>= ghcjs_also
  _test_framework_quickcheck2 <- git "https://github.com/seereason/test-framework" [] >>= patch $(embedFile "patches/test-framework-quickcheck2.diff") >>= cd "quickcheck2" >>= debianize [] >>=
                flag (P.DebVersion "0.3.0.4-1build1") >>= ghcjs_also
  _test_framework_smallcheck <-  (hackage (Just "0.2") "test-framework-smallcheck") >>= debianize []
  _test_framework_th <-  (hackage (Just "0.2.4") "test-framework-th" >>= tflag (P.DebVersion "0.2.4-1build4")) >>= aflag (P.DebVersion "0.2.4-9build2") >>= debianize []
  _texmath <- hackage (Just "0.9") "texmath" >>= patch $(embedFile "patches/texmath.diff") >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _tf_random <-  (hackage (Just "0.5") "tf-random") >>= debianize [] >>= aflag (P.DebVersion "0.5-7build1") >>= inGroups ["platform"] >>= ghcjs_also
  _th_desugar <- {-(git "http://github.com/goldfirere/th-desugar" [])-} hackage (Just "1.6") "th-desugar" >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_expand_syns <-  (hackage (Just "0.4.0.0") "th-expand-syns") >>= debianize [] >>= aflag (P.DebVersion "0.4.3.0-1") >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_lift <- hackage (Just "0.7.6") "th-lift" >>= debianize [] >>= aflag (P.DebVersion "0.7.7-1") >>= ghcjs_also
  _th_lift_instances <-
      hackage (Just "0.1.11") "th-lift-instances" >>=
      -- git "https://github.com/ddssff/th-lift-instances" [] >>=
      debianize [] >>= aflag (P.DebVersion "0.1.11-1build1") >>= inGroups ["important"] >>= ghcjs_also
  _th_orphans <-  (hackage Nothing "th-orphans") >>= debianize [] >>= aflag (P.DebVersion "0.13.3-1build1") >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_reify_many <-  (hackage (Just "0.1.6") "th-reify-many") >>= debianize [] >>= aflag (P.DebVersion "0.1.6-3build2") >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_typegraph <- git "http://github.com/seereason/th-typegraph" [] >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _threads <-  (hackage (Just "0.5.1.4") "threads") >>= debianize [] >>= aflag (P.DebVersion "0.5.1.4-3build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _time_compat <-  (hackage (Just "0.1.0.3") "time-compat") >>= debianize [] >>= aflag (P.DebVersion "0.1.0.3-7build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _tls <-  (hackage (Just "1.3.10") "tls") >>= debianize [] >>= aflag (P.DebVersion "1.3.10-2") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _transformers_base <- hackage (Just "0.4.4") "transformers-base" >>= debianize [] >>= aflag (P.DebVersion "0.4.4-6build1") >>= ghcjs_also
  _transformers_compat <- hackage (Just "0.5.1.4") "transformers-compat" >>=
                          -- flag (P.CabalPin "0.4.0.4") >>= -- waiting for newer monad-control and monad-parallel
                          {-patch $(embedFile "patches/transformers-compat.diff") >>=-}
                          debianize [] >>= aflag (P.DebVersion "0.5.1.4-2build1") >>= ghcjs_also

  _unicode_names <-  (git "https://github.com/seereason/unicode-names" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1")) >>= debianize []
  _unicode_properties <- git "https://github.com/seereason/unicode-properties" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1") >>= debianize []
  _unix_compat <-  (hackage (Just "0.4.2.0") "unix-compat") >>= debianize [] >>= aflag (P.DebVersion "0.4.3.1-1") >>= ghcjs_also
  _unixutils <-  (git "https://github.com/seereason/haskell-unixutils" []) >>= debianize [] >>= aflag (P.DebVersion "1.54.1-4build2") >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _userid <- git "https://github.com/Happstack/userid" [] >>= debianize [] >>= aflag (P.DebVersion "0.1.2.8-1build1") >>= inGroups ["authenticate", "happstack", "important"] >>= ghcjs_also
  _utility_ht <- hackage (Just "0.0.14") "utility-ht" >>= debianize [] >>= aflag (P.DebVersion "0.0.14-1") >>= ghcjs_also
  _uuid <- hackage (Just "1.3.12") "uuid" >>= debianize [] >>= aflag (P.DebVersion "1.3.12-3build3") >>= ghcjs_also
  _uuid_orphans <- git "https://github.com/seereason/uuid-orphans" [] >>= debianize [] >>= inGroups ["clckwrks", "important"] >>= ghcjs_also
  _uuid_types <-  (hackage (Just "1.0.3") "uuid-types") >>= debianize [] >>= aflag (P.DebVersion "1.0.3-4") >>= ghcjs_also
  _utf8_string <- hackage (Just "1.0.1.1") "utf8-string" >>=
                  flag (P.DebVersion "1.0.1.1-4build1") >>=
                  patch $(embedFile "patches/utf8-string.diff") >>=
                  flag (P.RelaxDep "hscolour") >>=
                  flag (P.RelaxDep "cpphs") >>=
                  debianize [] >>= ghcjs_also
  _vc_darcs <- darcs ("http://src.seereason.com/vc-darcs")
  _vc_git_dired <- git "https://github.com/ddssff/vc-git-dired" []
  _void <-  (hackage (Just "0.7.2") "void") >>= debianize [] >>= aflag (P.DebVersion "0.7.2-1") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _web_plugins <-  (git "http://github.com/clckwrks/web-plugins" []) >>= debianize []
  _web_routes <- git "https://github.com/Happstack/web-routes.git" [] >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _web_routes_boomerang <- git "https://github.com/Happstack/web-routes-boomerang.git" [] >>= debianize [] >>= aflag (P.DebVersion "0.28.4.2-3build2") >>= inGroups ["happstack", "important"]
  _web_routes_happstack <- git "https://github.com/Happstack/web-routes-happstack.git" [] >>= debianize [] >>= aflag (P.DebVersion "0.23.10-3build2") >>= inGroups ["happstack", "important"]
  _web_routes_hsp <- git "https://github.com/Happstack/web-routes-hsp.git" [] >>= debianize [] >>= aflag (P.DebVersion "0.24.6.1-3build2") >>= inGroups ["happstack", "important"]
  _web_routes_th <- git "https://github.com/Happstack/web-routes-th.git" [] >>= debianize [] >>= aflag (P.DebVersion "0.22.6.1-1build1") >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _webkitgtk3 <- hackage (Just "0.13.1.3") "webkitgtk3" >>= flag (P.CabalPin "0.13.1.3") >>= flag (P.BuildDep "libwebkitgtk-3.0-dev") >>= debianize [] >>= inGroups ["glib"] >>= skip (Reason "The ghcjs version doesn't need this, see cairo and glib")
  _wl_pprint_extras <- hackage (Just "3.5.0.5") "wl-pprint-extras" >>=
                       patch $(embedFile "patches/wl-pprint-extras.diff") >>=
                       debianize [] >>=
                       aflag (P.DebVersion "3.5.0.5-5build3") >>= ghcjs_also
  _wl_pprint_text <-  (hackage (Just "1.1.0.4") "wl-pprint-text") >>= debianize [] >>= aflag (P.DebVersion "1.1.1.0-1build1") >>= ghcjs_also
               -- Our applicative-extras repository has several important patches.
  _x509 <-  (hackage (Just "1.6.5") "x509") >>= debianize [] >>= aflag (P.DebVersion "1.6.5-1build2") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_store <-  (hackage (Just "1.6.2") "x509-store") >>= debianize [] >>= aflag (P.DebVersion "1.6.2-2build3") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_system <-  (hackage (Just "1.6.4") "x509-system") >>= debianize [] >>= aflag (P.DebVersion "1.6.4-2build3") >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_validation <-  (hackage (Just "1.6.5") "x509-validation") >>= debianize [] >>= aflag (P.DebVersion "1.6.5-2build3") >>= inGroups ["authenticate", "important"] >>= ghcjs_also :: TSt (PackageId, PackageId)
  _xhtml <-  (hackage (Just "3000.2.1") "xhtml" >>= wflag (P.DebVersion "3000.2.1-1") >>= qflag (P.DebVersion "3000.2.1-1build2") >>= tflag (P.DebVersion "3000.2.1-4")) >>= debianize [] >>= ghcjs_also
  _xml <- hackage (Just "1.3.14") "xml" >>= debianize [] >>= aflag (P.DebVersion "1.3.14-6build2") >>= ghcjs_also -- apt (rel release "wheezy" "quantal") "haskell-xml"
  _xml_conduit <-  (hackage (Just "1.3.5") "xml-conduit") >>= debianize [] >>= aflag (P.DebVersion "1.3.5-3build2") >>= inGroups ["conduit", "authenticate", "important"]
  _xss_sanitize <-  (hackage (Just "0.3.5.7") "xss-sanitize" >>= qflag (P.DebVersion "0.3.2-1build1")) >>= debianize [] >>= aflag (P.DebVersion "0.3.5.7-3build2") >>= inGroups ["important"]
  _yaml <-  (hackage (Just "0.8.23") "yaml") >>= debianize [] >>= aflag (P.DebVersion "0.8.23-2") >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _zenc <- hackage (Just "0.1.1") "zenc" >>= debianize [] >>= ghcjs_also
  _zip_archive <-  (hackage (Just "0.3.0.4") "zip-archive") >>= flag (P.BuildDep "zip") >>= debianize [] >>= aflag (P.DebVersion "0.3.0.6-1build1") >>= ghcjs_also
  _zlib <-  (hackage (Just "0.6.1.2") "zlib"
                      -- >>= flag (P.CabalPin "0.5.4.2") -- Cabal-1.22.6.0 is not ready for zlib-0.6
                      >>= flag (P.DevelDep "zlib1g-dev")) >>= debianize [] >>= aflag (P.DebVersion "0.6.1.2-1build1") >>= inGroups ["platform"] >>= ghcjs_also

#if 0
  -- Specify suspected dependencies
  _asn1_types `depends` [_hourglass]
  -- _th_typegraph `depends` [_set_extra, _th_desugar, _th_orphans]
  _x509 `depends` [_pem, _asn1_parse]
  _x509_validation `depends` [_x509_store]
  _happstack_authenticate `depends` [_authenticate, _happstack_hsp, _happstack_jmacro, _shakespeare, _web_routes_happstack]
  _happstack_authenticate `depends` [_userid]
  _happstack_scaffolding `depends` [_userid]
  _ixset `depends` [_safecopy]
  _seereason_base `depends` [_happstack_scaffolding]
  _happstack_hsp `depends` [_happstack_server]
  _happstack_jmacro `depends` [_happstack_server]
  _shakespeare `depends` [_blaze_html, _blaze_markup]
  _web_routes_happstack `depends` [_happstack_server]
  _sr_extra `depends` [_quickCheck]
  _th_desugar `depends` [_th_reify_many{-, _syb-}]
  _jmacro `depends` [_parseargs, _wl_pprint_text, _haskell_src_meta]
  _pandoc `depends` [_juicyPixels, _pandoc_types, _yaml]
  _haTeX `depends` [_quickCheck, _wl_pprint_extras, _matrix]
  _authenticate `depends` [_tagstream_conduit,_xml_conduit, _http_conduit]
  _connection `depends` [_x509_system, _socks]
  _matrix `depends` [_loop]
#endif

  noTests
  return ()

noTests :: TSt ()
noTests = use P.packageMap >>= mapM_ (flag (P.CabalDebian ["--no-tests"])) . Map.keys
