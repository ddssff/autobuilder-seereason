{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Types.Packages as P (Packages(APackage), PackageFlag(BuildDep, CabalDebian, NoDoc), flag, mapPackages, patch, debianize, darcs, git, cd, TSt)
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
    , stripe_haskell
    , clckwrks_plugin_stripe
    , mimo
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
ghcjs_ghcjs_webmodule = ghcjs_flags $ debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-webmodule")
happstack_ghcjs_client = debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-client"
                                      `flag` P.BuildDep "libghc-cabal-122-dev"
                                      `flag` P.BuildDep "ghcjs"
                                      `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.3)"
                                      `flag` P.CabalDebian ["--ghcjs", "--source-package=ghcjs-happstack-ghcjs-client"])
happstack_ghcjs_server = debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-server")
happstack_ghcjs_webmodule = debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-webmodule")
happstack_ontology = debianize (git "ssh://git@github.com/seereason/happstack-ontology" [] `flag` P.BuildDep "hsx2hs")
image_cache = debianize (git "https://github.com/seereason/image-cache.git" [])
-- The debian/Debianize.hs script has a dependency on
-- happstack-foundation, which must be installed in the parent
-- environment *before* we can create the debianization.  We don't
-- really have a mechanism to ensure this is installed in the parent
-- environment, except making it a dependency of the autobuilder
-- itself.
mimo = debianize (git "ssh://git@github.com/seereason/mimo.git" [])
ontology = git "ssh://git@github.com/seereason/ontology.git" []
seereason = git "ssh://git@github.com/seereason/seereason" []
seereasonpartners_dot_com = debianize (darcs (privateRepo </> "seereasonpartners-clckwrks") `cd` "seereasonpartners-dot-com" `patch` $(embedFile "patches/seereasonpartners-dot-com.diff"))
stripe_core = debianize (git "ssh://git@github.com/stripe-haskell/stripe" [Branch "stripe-haskell-transition"]
                                 `cd` "stripe-core")
stripe_http_streams = debianize (git "ssh://git@github.com/stripe-haskell/stripe" [Branch "stripe-haskell-transition"]
                                    `cd` "stripe-http-streams"
                                    `flag` P.CabalDebian [{-"--no-tests"-}])
stripe_haskell = debianize (git "ssh://git@github.com/stripe-haskell/stripe" [Branch "stripe-haskell-transition"]
                                    `cd` "stripe"
                                    `flag` P.CabalDebian [{-"--no-tests"-}])
-- stripe_http_conduit = debianize (darcs (privateRepo </> "stripe") `cd` "stripe-http-conduit")
task_manager = debianize (git "ssh://git@github.com/seereason/task-manager.git" [])
