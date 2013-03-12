{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS -Wall -Werror -fno-warn-missing-signatures -fno-warn-unused-imports -fno-warn-orphans #-}
#!/usr/bin/env runhaskell -package=base-3.0.3.0
-- Currently this will not run as a script even with the line above.
-- The reason is unclear.  Either use the wrapper script in
-- /usr/bin/autobuilder or run in the interpreter:
--
--   ghci
--   > :load autobuilder.hs
--   > getParams ["lucid-seereason", "--all-targets", "--flush-pool"] >>= Debian.AutoBuilder.Main.main
--
-- This may run very slowly.

-- Import the symbols we use below.
import Control.Exception (SomeException, try, throw)
import Data.List as List (isSuffixOf, isPrefixOf, find, map)
import Data.Map as Map (Map, elems, empty, insertWith)
import Data.Maybe
import Data.Monoid (mappend)
import Data.Set as Set (Set, empty, fold, member, insert, map)
import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Types.ParamRec
import Debian.AutoBuilder.Types.Packages
import Debian.Release (ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.URI
import Debian.Version (parseDebianVersion)
import Prelude hiding (map)
import System.Console.GetOpt
import System.Environment (getArgs, getEnv)
import System.Exit
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)

import Config
import Targets (private)
import Debian.Debianize.Details (seereasonDefaultAtoms)
import Usage

main =
    hPutStrLn stderr "Autobuilder starting..." >>
    getArgs >>= \ args ->
    getEnv "HOME" >>= \ home ->
    try (help (getParams home args) >>= M.main defaultAtoms) >>=
    either (\ (e :: SomeException) -> hPutStrLn stderr ("Exception: " ++ show e) >> throw e) return

-- | Look for the doHelp flag in any parameter set, if given output
-- help message and exit.  If --help was given it will appear in all
-- the parameter sets, so we only examine the first.
help :: [ParamRec] -> IO [ParamRec]
help recs =
    case (any doHelp recs) of
      True -> hPutStr stderr (usage "Usage: " optSpecs) >> return []
      False -> return recs

-- |given a list of strings as they would be returned from getArgs,
-- build the list of ParamRec which defines the build.
-- 
-- Example: getParams ["lucid-seereason" "--all-targets"] >>= return . map buildRelease
--            -> [ReleaseName {relName = "lucid-seereason"}]
getParams :: String -> [String] -> [ParamRec]
getParams home args =
    doParams (getOpt' (ReturnInOrder Left) optSpecs args)
    where
      -- Turn the parameter information into a list of parameter records
      -- containing all the info needed during runtime.  Each return record
      -- represents a separate autobuilder run.
      doParams :: ([Either String (ParamRec -> ParamRec)], -- The list of functions to apply to the default record
                   [String],               -- non-options
                   [String],               -- unrecognized options
                   [String])               -- error messages
               -> [ParamRec]
      doParams (fns, [], [], []) =
          List.map (\ params -> params {packages = finalizeTargets params}) . reverse $ f [] fns
          where
            f recs [] = recs
            f recs (Left rel : xs) = f (defParams home rel : recs) xs
            f (rec : more) (Right fn : xs) = f (fn rec : more) xs
            f _ _ = error "First argument must be a release name"
      doParams (_, _, badopts, errs) =
          error (usage ("Bad options: " ++ show badopts ++ ", errors: " ++ show errs) optSpecs)

      finalizeTargets :: ParamRec -> Packages
      finalizeTargets params =
          Packages { group = Set.empty
                   , list = Map.elems $ if allTargets (targets params)
                                        then collectAll knownTargets Map.empty
                                        else Set.fold collectName Map.empty (targetNames (targets params) :: Set TargetName) }
          where
            collectName :: TargetName -> Map.Map TargetName Packages -> Map.Map TargetName Packages
            collectName n collected =
              case findByName n knownTargets of
                [] -> error $ "Unknown target name: " ++ show n
                ps -> foldr (\ p collected' -> 
                              case p of
                                (Package {}) -> Map.insertWith check (name p) p collected'
                                _ -> error $ "Internal error: " ++ show p) collected ps
            findByName n known =
              case known of
                NoPackage -> []
                ps@(Packages {}) ->
                  if Set.member n (group ps)
                  then concatMap findAll (list ps)
                  else concatMap (findByName n) (list ps)
                p@(Package {}) ->
                  if name p == n
                  then [p]
                  else []
            findAll known =
              case known of
                NoPackage -> []
                ps@(Packages {}) -> concatMap findAll (list ps)
                p@(Package {}) -> [p]
{-
            collectByName known n collected =
                case known of
                  NoPackage -> collected
                  p@(Package {}) -> if name p == n then Map.insertWith check n p collected else collected
                  ps@(Packages {}) ->
                      if Set.member n (group ps)
                      then foldr collectAll collected (packages ps)
                      else foldr (collect' n) collected (packages ps)
            collect' n known collected = collectByName known n collected
-}
            collectAll :: Packages -> Map.Map TargetName Packages -> Map.Map TargetName Packages
            collectAll p collected =
                case p of
                  NoPackage -> collected
                  Package {} -> Map.insertWith check (name p) p collected
                  Packages {} -> foldr collectAll collected (list p)
            check old new = if old /= new then error ("Multiple packages with same name: " ++ show old ++ ", " ++ show new) else old
            -- FIXME - make myTargets a set
            knownTargets = mappend (myTargets home (const True) (relName (buildRelease params)))
                                   (if testWithPrivate params then private home else NoPackage)

{-
instance Eq Packages where
    (Package {name = n1, spec = s1}) == (Package {name = n2, spec = s2}) = n1 == n2 && s1 == s2
    (Packages {group = g1, list = l1}) == (Packages {group = g2, list = l2}) = g1 == g2 && l1 == l2
    NoPackage == NoPackage = True
    _ == _ = False

instance Show Packages where
    show _ = "<Packages>"
-}

-- Assemble all the configuration info above.

-- |See Documentation in "Debian.AutoBuilder.ParamClass".
defParams _home myBuildRelease =
    ParamRec
    { vendorTag = myVendorTag
    , oldVendorTags = ["seereason"]
    , autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    , releaseSuffixes = myReleaseSuffixes
    , buildRelease = ReleaseName {relName = myBuildRelease}
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    -- What we plan to build
    , targets = TargetSpec {allTargets = False, targetNames = Set.empty}
    , doUpload = myDoUpload
    , doNewDist = myDoNewDist
    , flushPool = myFlushPool
    , useRepoCache = True
    , forceBuild = myForceBuild
    , buildTrumped = myBuildTrumped
    , doSSHExport = myDoSSHExport
    , report = False
    , doHelp = False
    -- Things that are occasionally useful
    , goals = myGoals
    , dryRun = False
    , allowBuildDependencyRegressions = False
    , setEnv = []
    , showSources = False
    , showParams = False
    , flushAll = False
    , flushSource = False
    , flushRoot = False
    , verbosity = myVerbosity
    , topDirParam = Nothing
    , createRelease = []
    , doNotChangeVersion = False
    -- Things that rarely change
    , sources = mySources myBuildRelease myDebianMirrorHost myUbuntuMirrorHost
    , globalRelaxInfo = myGlobalRelaxInfo
    , strictness = Lax
    , flushDepends = False
    , includePackages = myIncludePackages myBuildRelease
    , excludePackages = myExcludePackages myBuildRelease
    , components = myComponents myBuildRelease
    , ghcVersion = {- trace ("ghcVersion: " ++ show (myCompilerVersion myBuildRelease)) $ -} myCompilerVersion myBuildRelease
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = myReleaseAliases myBuildRelease
    , archList = [Binary "i386",Binary "amd64"]
    , newDistProgram = "newdist -v"
    -- 6.14 adds the ExtraDevDep parameter.
    -- 6.15 changes Epoch parameter arity to 2
    -- 6.18 renames type Spec -> RetrieveMethod
    -- 6.35 added the CabalDebian flag
    , requiredVersion = [(parseDebianVersion ("6.51" :: String), Nothing)]
    , hackageServer = myHackageServer
    -- Things that are probably obsolete
    , debug = False
    , discard = Set.map TargetName myDiscards
    , testWithPrivate = False
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    , packages = NoPackage
    }
