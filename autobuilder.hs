{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -Werror -fno-warn-missing-signatures -fno-warn-unused-imports #-}
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
import Control.Exception (SomeException, try)
import Data.List (isSuffixOf, isPrefixOf, find)
import Data.Maybe
import qualified Data.Set as Set
import qualified Debian.AutoBuilder.Main as M
import Debian.AutoBuilder.Params
import Debian.Release (ReleaseName(ReleaseName, relName), Arch(Binary))
import Debian.Repo.Cache (SourcesChangedAction(SourcesChangedError))
import Debian.URI
import Debian.Version (parseDebianVersion)
import System.Console.GetOpt
import System.Environment (getArgs, getEnv)
import System.Exit
import System.IO (hPutStr, hPutStrLn, hFlush, stderr)

import Config
import Usage

main = try (getArgs >>= getParams >>= M.main) >>=
       either (\ (e :: SomeException) -> hPutStrLn stderr ("Exception: " ++ show e)) return

-- |given a list of strings as they would be returned from getArgs,
-- build the list of ParamRec which defines the build.
-- 
-- Example: getParams ["lucid-seereason" "--all-targets"] >>= return . map buildRelease
--            -> [ReleaseName {relName = "lucid-seereason"}]
getParams :: [String] -> IO [ParamRec]
getParams args =
    getEnv "HOME" >>= \ home ->
    hPutStrLn stderr "Autobuilder starting..." >>
    doParams home (getOpt' Permute (optSpecs home) args)
    where
      -- Turn the parameter information into a list of parameter records
      -- containing all the info needed during runtime.
      doParams ::  FilePath -> ([ParamRec -> ParamRec], [String], [String], [String]) -> IO [ParamRec]
      doParams home (fns, dists, [], []) = 
          maybeDoHelp home . map (finalizeTargets home) . map (\ p -> foldr ($) p fns) . map (defParams home) $ dists
      doParams home (_, _, badopts, errs) =
          hPutStr stderr (usage ("Bad options: " ++ show badopts ++
                           ", errors: " ++ show errs) (optSpecs home)) >> return []
      -- Finalize the target list in a parameter set, turning the targets field into a value
      -- with the constructor TargetSet.
      finalizeTargets :: FilePath -> ParamRec -> ParamRec
      finalizeTargets home p =
          p { targets =
                  case targets p of
                    TargetSet xs -> TargetSet xs
                    TargetNames xs -> TargetSet (Set.map findSpec xs)
                    AllTargets -> TargetSet allTargets }
          where
            findSpec s = case Set.toList (Set.filter (\ t -> name t == s) allTargets) of
                           [x] -> x
                           [] -> error $ "Package name not found: " ++ s ++ "\navailable: " ++ show (Set.toList (Set.map name allTargets))
                           xs -> error $ "Multiple packages named " ++ s ++ " found: " ++ show xs
            -- FIXME - make myTargets a set
            allTargets = myTargets home (const True) (relName (buildRelease p))
      -- Look for the doHelp flag in the parameter set, if given output
      -- help message and exit.  If --help was given it will appear in all
      -- the parameter sets, so we only examine the first.
      maybeDoHelp home xs@(x : _)
          | doHelp x = hPutStr stderr (usage "Usage: " (optSpecs home)) >>
                       exitWith ExitSuccess >> return xs
          | True = return xs
      maybeDoHelp _ [] = return []

-- |Each option is defined as a function transforming the parameter record.
optSpecs :: FilePath -> [OptDescr (ParamRec -> ParamRec)]
optSpecs home =
    [ Option ['v'] ["verbose"] (NoArg (\ p -> p {verbosity = verbosity p + 1}))
      "Increase progress reporting.  Can be used multiple times."
    , Option ['q'] ["quiet"] (NoArg (\ p -> p {verbosity = verbosity p - 1}))
      "Decrease progress reporting. Can be used multiple times."
    , Option [] ["show-params"] (NoArg (\ p -> p {showParams = True}))
      "Display the parameter set" 
    , Option [] ["flush-repo-cache"] (NoArg (\ p -> p {useRepoCache = False}))
      (unlines [ "Ignore the existing cached information about the remote repositories,"
               , "instead rebuild it from scratch and save the new result" ])
    , Option [] ["flush-pool"] (NoArg (\ p -> p {flushPool = True}))
      "Flush the local repository before building."
    , Option [] ["flush-root"] (NoArg (\ p -> p {flushRoot = True}))
      "Discard and recreate the clean and build environments."
    , Option [] ["flush-source"] (NoArg (\ p -> p {flushSource = True}))
      "Discard and re-download all source code."
    , Option [] ["flush-all"] (NoArg (\ p -> p {flushAll = True}))
      "Remove and re-create the entire autobuilder working directory."
    , Option [] ["do-upload"] (NoArg (\ p -> p {doUpload = True}))
      "Upload the packages to the remote server after a successful build."
    , Option [] ["do-newdist"] (NoArg (\ p -> p {doNewDist = True}))
      "Run newdist on the remote server after a successful build and upload."
    , Option ['n'] ["dry-run"] (NoArg (\ p -> p {dryRun = True}))
      "Exit as soon as we discover a package that needs to be built."
    , Option [] ["all-targets"] (NoArg (\ p ->  p {targets = AllTargets}))
      "Add all known targets for the release to the target list."
    , Option [] ["allow-build-dependency-regressions"]
                 (NoArg (\ p -> p {allowBuildDependencyRegressions = True}))
      (unlines [ "Normally it is an error if a build dependency has an older version"
               , "number than during the previous looks older than it did during the"
               , "previous build.  This option relaxes that assumption, in case the"
               , "newer version of the dependency was withdrawn from the repository,"
               , "or was flushed from the local repository without being uploaded."])
    , Option [] ["target"] (ReqArg (\ s p -> p {targets = addTarget s p}) "PACKAGE")
      "Add a target to the target list."
    , Option [] ["discard"] (ReqArg (\ s p -> p {discard = Set.insert s (discard p)}) "PACKAGE")
      "Add a target to the discard list."
    , Option [] ["goal"] (ReqArg (\ s p -> p { goals = goals p ++ [s]
                                             , targets = TargetSet (myTargets home (const True) (relName (buildRelease p)))}) "PACKAGE")
      (unlines [ "If one or more goal package names are given the autobuilder"
               , "will only build these packages and any of their build dependencies"
               , "which are in the package list.  If no goals are specified, all the"
               , "targets will be built.  (As of version 5.2 there are known bugs with"
               , "this this option which may cause the autobuilder to exit before the"
               , "goal package is built.)"])
    , Option [] ["force"] (ReqArg (\ s p -> p {forceBuild = forceBuild p ++ [s]}) "PACKAGE")
      ("Build the specified source package even if it doesn't seem to need it.")
    , Option [] ["build-trumped"] (ReqArg (\ s p -> p {buildTrumped = buildTrumped p ++ [s]}) "PACKAGE")
      ("Build the specified source package even if it seems older than the uploaded version.")
    , Option ['h'] ["help", "usage"] (NoArg (\ p -> p {doHelp = True}))
      "Print a help message and exit."
    ]
    where
      addTarget s p =
          case targets p of
            AllTargets -> AllTargets
            TargetNames xs -> TargetNames (Set.insert s xs)
            TargetSet _ -> error "optSpecs: unexpected value in target specs"
{-
      allTargets p =
          p {targets = let name = (relName (buildRelease p)) in TargetList (myTargets (releaseTargetNamePred name) name)})
      ++ [find s p]
      find s p = case filter (\ t -> sourcePackageName t == s) (myTargets (const True) (relName (buildRelease p))) of
                   [x] -> x
                   [] -> error $ "Package not found: " ++ s
                   xs -> error $ "Multiple packages found: " ++ show (map sourcePackageName xs)
-}

-- Assemble all the configuration info above.

-- |See Documentation in "Debian.AutoBuilder.ParamClass".
defParams home myBuildRelease =
    ParamRec
    { vendorTag = myVendorTag
    , oldVendorTags = ["seereason"]
    , autobuilderEmail = "SeeReason Autobuilder <autobuilder@seereason.org>"
    , releaseSuffixes = myReleaseSuffixes
    , buildRelease = ReleaseName {relName = myBuildRelease}
    , uploadURI = myUploadURI myBuildRelease
    , buildURI = myBuildURI myBuildRelease
    -- What we plan to build
    , targets = TargetNames Set.empty
    , doUpload = myDoUpload
    , doNewDist = myDoNewDist
    , flushPool = myFlushPool
    , useRepoCache = True
    , forceBuild = myForceBuild
    , buildTrumped = myBuildTrumped
    , doSSHExport = myDoSSHExport
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
    , strictness = Moderate
    , includePackages = myIncludePackages myBuildRelease
    , excludePackages = myExcludePackages myBuildRelease
    , components = myComponents myBuildRelease
    , ghcVersion = myCompilerVersion myBuildRelease
    , developmentReleaseNames = myDevelopmentReleaseNames
    , releaseAliases = myReleaseAliases myBuildRelease
    , archList = [Binary "i386",Binary "amd64"]
    , newDistProgram = "newdist -v"
    , requiredVersion = [(parseDebianVersion "5.17", Nothing)]
    -- Things that are probably obsolete
    , debug = False
    , discard = myDiscards home
    , extraReleaseTag = Nothing
    , preferred = []
    , buildDepends = []
    , noClean = False
    , cleanUp = False
    , ifSourcesChanged = SourcesChangedError
    }
