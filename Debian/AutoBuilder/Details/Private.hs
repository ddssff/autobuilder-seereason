{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Control.Lens ((%=), use, view)
import Data.FileEmbed (embedFile)
import Data.Map as Map (keys, map)
import Debian.AutoBuilder.Types.Packages as P (Packages(APackage), PackageFlag(BuildDep, CabalDebian, NoDoc, SetupDep), flag, mapPackages, patch, debianize, darcs, git, cd, TSt, packageMap)
import Debian.AutoBuilder.Details.Common -- (privateRepo, named, ghcjs_flags)
import Debian.Repo.Fingerprint (GitSpec(Branch))
import System.FilePath ((</>))

-- Individual packages, alphabetized

appraisalscribe = git "ssh://git@github.com/seereason/appraisalscribe" [] >>= debianize
appraisalscribe_paths = git "ssh://git@github.com/seereason/appraisalscribe-paths" [] >>= debianize
appraisalscribe_data = git "ssh://git@github.com/seereason/appraisalscribe-data" [] >>= debianize
-- appraisalscribe-data-tests is a huge package because it
-- contains lots of test data, it makes more sense to just check
-- it out of git and run it rather than constantly uploading it to
-- the repository.
-- appraisalscribe_data_tests = debianize (git "ssh://git@github.com/seereason/appraisalscribe-data-tests" [])


clckwrks_plugin_stripe = darcs (privateRepo </> "clckwrks-plugin-stripe") >>= flag (P.BuildDep "hsx2hs") >>= debianize
clckwrks_theme_seereasonpartners = darcs (privateRepo </> "seereasonpartners-clckwrks") >>= cd "clckwrks-theme-seereasonpartners" >>=  debianize >>= flag (P.BuildDep "hsx2hs") >>= flag P.NoDoc
clckwrks_theme_appraisalscribe = darcs (privateRepo </> "clckwrks-theme-appraisalscribe") >>= flag (P.BuildDep "hsx2hs") >>= debianize

happstack_ghcjs_client =    git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] >>= cd "happstack-ghcjs-client" >>= debianize >>= ghcjs_flags
happstack_ghcjs_server =    git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] >>= cd "happstack-ghcjs-server" >>= debianize
ghcjs_ghcjs_webmodule =     git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] >>= cd "happstack-ghcjs-webmodule" >>= debianize >>= ghcjs_flags
happstack_ghcjs_webmodule = git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] >>= cd "happstack-ghcjs-webmodule" >>= debianize

happstack_ontology = git "ssh://git@github.com/seereason/happstack-ontology" [] >>= flag (P.BuildDep "hsx2hs") >>= debianize
image_cache = git "https://github.com/seereason/image-cache.git" [] {- >>= flag (P.CabalDebian ["--cabal-flags", "-pretty-112"]) -} >>= debianize
-- The debian/Debianize.hs script has a dependency on
-- happstack-foundation, which must be installed in the parent
-- environment *before* we can create the debianization.  We don't
-- really have a mechanism to ensure this is installed in the parent
-- environment, except making it a dependency of the autobuilder
-- itself.
mimo = git "ssh://git@github.com/seereason/mimo.git" [] >>= debianize
mimo_bootstrap =
    git "ssh://git@github.com/seereason/mimo-bootstrap.git" [] >>=
        flag (P.SetupDep "libghc-mimo-dev") >>=
        flag (P.BuildDep "haskell-mimo-utils") >>=
        debianize
mimo_optimum =
    git "ssh://git@github.com/seereason/mimo-optimum.git" [] >>=
        flag (P.SetupDep "libghc-mimo-dev") >>=
        flag (P.BuildDep "haskell-mimo-utils") >>=
        debianize
mimo_paste =
    git "ssh://git@github.com/seereason/mimo-paste.git" [] >>=
        flag (P.SetupDep "libghc-mimo-dev") >>=
        flag (P.BuildDep "haskell-mimo-utils") >>=
        debianize
ontology = git "ssh://git@github.com/seereason/ontology.git" [] >>= debianize
seereason = git "ssh://git@github.com/seereason/seereason" [] >>= debianize
seereasonpartners_dot_com =  darcs (privateRepo </> "seereasonpartners-clckwrks") >>=
                             cd "seereasonpartners-dot-com" >>=
                             patch $(embedFile "patches/seereasonpartners-dot-com.diff") >>=
                             debianize
-- stripeRepo = "ssh://git@github.com/stripe-haskell/stripe"
-- stripeRepo = "https://github.com/seereason/stripe"
stripeRepo = "https://github.com/dmjio/stripe"
stripe_core = git stripeRepo [] >>= cd "stripe-core" >>= debianize
stripe_http_streams = git stripeRepo [] >>= cd "stripe-http-streams" >>= flag (P.CabalDebian [{-"--no-tests"-}]) >>=  debianize
stripe_haskell = git stripeRepo [] >>= cd "stripe-haskell" >>= flag (P.CabalDebian [{-"--no-tests"-}]) >>= debianize
-- stripe_http_conduit = debianize (darcs (privateRepo </> "stripe") `cd` "stripe-http-conduit")
task_manager = git "ssh://git@github.com/seereason/task-manager.git" [] >>= debianize
th_path = git "ssh://git@github.com/seereason/th-path.git" [] >>= debianize

noTests :: TSt ()
noTests = do
  ids <- Map.keys <$> use P.packageMap
  mapM_ (flag (P.CabalDebian ["--no-tests"])) ids

libraries :: TSt ()
libraries =
    -- (named "libraries") =<<
    sequence
    [ ontology
    , stripe_core
    , stripe_http_streams
    -- , stripe_haskell
    , clckwrks_plugin_stripe
    -- , mimo
    -- , mimo_bootstrap
    -- , mimo_optimum
    -- , mimo_paste
    , th_path
    , th_path >>= ghcjs_flags
    , task_manager
    , happstack_ghcjs_client
    , happstack_ghcjs_server
    , happstack_ghcjs_webmodule
    , ghcjs_ghcjs_webmodule
    ] >> noTests

applications :: TSt ()
applications =
    -- (named "applications") =<<
    sequence
    [ appraisalscribe
    , appraisalscribe_paths
    , appraisalscribe_paths >>= ghcjs_flags
    , appraisalscribe_data
    , appraisalscribe_data >>= ghcjs_flags
    , image_cache
    , image_cache >>= ghcjs_flags
    , seereason
    , happstack_ontology
    , seereasonpartners_dot_com
    , clckwrks_theme_seereasonpartners
    , clckwrks_theme_appraisalscribe
    ] >> noTests
