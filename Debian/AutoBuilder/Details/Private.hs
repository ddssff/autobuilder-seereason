{-# LANGUAGE OverloadedStrings, PackageImports, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Debian.AutoBuilder.Types.Packages as P (Packages(APackage), PackageFlag(BuildDep, NoDoc), flag, patch, debianize, darcs, git, cd)
import Debian.AutoBuilder.Details.Common (privateRepo, named, ghcjs_flags)
import System.FilePath ((</>))

libraries _home =
    named "libraries" $ map APackage $
    [ -- Retired, should be withdrawn from repos
      -- darcs "haskell-generic-formlets3" (privateRepo </> "generic-formlets3")
    -- , darcs "haskell-document" (privateRepo </> "haskell-document")
      darcs (privateRepo </> "haskell-ontology")
    , debianize (darcs (privateRepo </> "stripe")
                   `cd` "stripe-core"
                   `patch` $(embedFile "patches/stripe-core.diff"))
    , debianize (darcs (privateRepo </> "stripe")
                   `cd` "stripe-http-conduit")
    , debianize (darcs (privateRepo </> "clckwrks-plugin-stripe")
                   `flag` P.BuildDep "hsx2hs")
    -- The debian/Debianize.hs script has a dependency on
    -- happstack-foundation, which must be installed in the parent
    -- environment *before* we can create the debianization.  We don't
    -- really have a mechanism to ensure this is installed in the
    -- parent environment, except making it a dependency of the
    -- autobuilder itself.
    , debianize (git "ssh://git@github.com/seereason/mimo.git" []) -- Disabled until safecopy instances are fixed
    , debianize (git "ssh://git@github.com/seereason/task-manager.git" [])
    , ghcjs_flags (debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-client"))
    , debianize (darcs (privateRepo </> "happstack-ghcjs") `cd` "happstack-ghcjs-server")
    ] {- ++ clckwrks14 -}

applications _home =
    named "applications" $ map APackage $
    [ debianize (darcs (privateRepo </> "appraisalscribe"))
    , debianize (git "ssh://git@github.com/seereason/appraisalscribe-data" [])

    -- appraisalscribe-data-tests is a huge package because it
    -- contains lots of test data, it makes more sense to just check
    -- it out of git and run it rather than constantly uploading it to
    -- the repository.
    -- , debianize (git "ssh://git@github.com/seereason/appraisalscribe-data-tests" [])

    , debianize (git "https://github.com/seereason/image-cache.git" [])
    , darcs (privateRepo </> "seereason")
    , debianize (darcs (privateRepo </> "happstack-ontology")
                   `flag` P.BuildDep "hsx2hs")
    -- Obsolete
    -- , darcs "haskell-creativeprompts" (privateRepo </> "creativeprompts")
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , debianize (darcs (privateRepo </> "seereasonpartners-clckwrks")
                   `cd` "seereasonpartners-dot-com"
                   `patch` $(embedFile "patches/seereasonpartners-dot-com.diff"))
    , debianize (darcs (privateRepo </> "seereasonpartners-clckwrks")
                   `cd` "clckwrks-theme-seereasonpartners"
                   `flag` P.BuildDep "hsx2hs"
                   `flag` P.NoDoc)
    , debianize (darcs (privateRepo </> "clckwrks-theme-appraisalscribe")
                   `flag` P.BuildDep "hsx2hs")
    ]

