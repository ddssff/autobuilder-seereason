{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Types.Packages as P (Packages(APackage), PackageFlag(BuildDep, CabalDebian, NoDoc, SetupDep), flag, mapPackages, patch, debianize, darcs, git, cd, TSt)
import Debian.AutoBuilder.Details.Common -- (privateRepo, named, ghcjs_flags)
import Debian.Repo.Fingerprint (GitSpec(Branch))
import System.FilePath ((</>))

noTests :: TSt Packages -> TSt Packages
noTests = mapPackages (\ p -> p `flag` P.CabalDebian ["--no-tests"])

libraries :: TSt P.Packages
libraries =
    noTests $
    (named "libraries" . map APackage) =<<
    sequence
    [ ontology
    , stripe_core
    , stripe_http_streams
    -- , stripe_haskell
    , clckwrks_plugin_stripe
    , mimo
    , mimo_bootstrap
    , mimo_optimum
    , mimo_paste
    , th_path
    , ghcjs_flags th_path
    , task_manager
    , happstack_ghcjs_client
    , happstack_ghcjs_server
    , happstack_ghcjs_webmodule
    , ghcjs_ghcjs_webmodule
    ]

applications :: TSt P.Packages
applications =
    noTests $
    (named "applications" . map APackage) =<<
    sequence
    [ appraisalscribe
    , appraisalscribe_data
    , image_cache
    , seereason
    , happstack_ontology
    , seereasonpartners_dot_com
    , clckwrks_theme_seereasonpartners
    , clckwrks_theme_appraisalscribe
    ]

-- Individual packages, alphabetized

appraisalscribe = debianize (git "ssh://git@github.com/seereason/appraisalscribe" [])
appraisalscribe_data = debianize (git "ssh://git@github.com/seereason/appraisalscribe-data" [])
-- appraisalscribe-data-tests is a huge package because it
-- contains lots of test data, it makes more sense to just check
-- it out of git and run it rather than constantly uploading it to
-- the repository.
-- appraisalscribe_data_tests = debianize (git "ssh://git@github.com/seereason/appraisalscribe-data-tests" [])


clckwrks_plugin_stripe = debianize (darcs (privateRepo </> "clckwrks-plugin-stripe") `flag` P.BuildDep "hsx2hs")
clckwrks_theme_seereasonpartners = debianize (darcs (privateRepo </> "seereasonpartners-clckwrks") `cd` "clckwrks-theme-seereasonpartners" `flag` P.BuildDep "hsx2hs" `flag` P.NoDoc)
clckwrks_theme_appraisalscribe = debianize (darcs (privateRepo </> "clckwrks-theme-appraisalscribe") `flag` P.BuildDep "hsx2hs")

happstack_ghcjs_client = ghcjs_flags $ debianize (git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] `cd` "happstack-ghcjs-client")
happstack_ghcjs_server =               debianize (git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] `cd` "happstack-ghcjs-server")
ghcjs_ghcjs_webmodule =  ghcjs_flags $ debianize (git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] `cd` "happstack-ghcjs-webmodule")
happstack_ghcjs_webmodule =            debianize (git "ssh://git@github.com/seereason/happstack-ghcjs" [Branch "vdom-event"] `cd` "happstack-ghcjs-webmodule")

happstack_ontology = debianize (git "ssh://git@github.com/seereason/happstack-ontology" [] `flag` P.BuildDep "hsx2hs")
image_cache = debianize (git "https://github.com/seereason/image-cache.git" [] {- `flag` P.CabalDebian ["--cabal-flags", "-pretty-112"] -} )
-- The debian/Debianize.hs script has a dependency on
-- happstack-foundation, which must be installed in the parent
-- environment *before* we can create the debianization.  We don't
-- really have a mechanism to ensure this is installed in the parent
-- environment, except making it a dependency of the autobuilder
-- itself.
mimo = debianize (git "ssh://git@github.com/seereason/mimo.git" [])
mimo_bootstrap = debianize (git "ssh://git@github.com/seereason/mimo-bootstrap.git" [] `flag` P.SetupDep "libghc-mimo-dev")
mimo_optimum = debianize (git "ssh://git@github.com/seereason/mimo-optimum.git" [] `flag` P.SetupDep "libghc-mimo-dev")
mimo_paste = debianize (git "ssh://git@github.com/seereason/mimo-paste.git" [] `flag` P.SetupDep "libghc-mimo-dev")
ontology = git "ssh://git@github.com/seereason/ontology.git" []
seereason = debianize (git "ssh://git@github.com/seereason/seereason" [])
seereasonpartners_dot_com = debianize (darcs (privateRepo </> "seereasonpartners-clckwrks") `cd` "seereasonpartners-dot-com" `patch` $(embedFile "patches/seereasonpartners-dot-com.diff"))
-- stripeRepo = "ssh://git@github.com/stripe-haskell/stripe"
-- stripeRepo = "https://github.com/seereason/stripe"
stripeRepo = "https://github.com/dmjio/stripe"
stripe_core = debianize (git stripeRepo [] `cd` "stripe-core")
stripe_http_streams = debianize (git stripeRepo [] `cd` "stripe-http-streams" `flag` P.CabalDebian [{-"--no-tests"-}])
stripe_haskell = debianize (git stripeRepo [] `cd` "stripe-haskell" `flag` P.CabalDebian [{-"--no-tests"-}])
-- stripe_http_conduit = debianize (darcs (privateRepo </> "stripe") `cd` "stripe-http-conduit")
task_manager = debianize (git "ssh://git@github.com/seereason/task-manager.git" [])
th_path = debianize (git "ssh://git@github.com/seereason/th-path.git" [])
