{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Data.Set (singleton)
import Debian.AutoBuilder.Types.Packages as P (PackageFlag(CabalPin, ModifyAtoms, BuildDep, NoDoc, CabalDebian),
                                               Packages(Package, Packages), RetrieveMethod(Debianize, Hackage, Cd, Darcs, Dir),
                                               flag, flags, spec, name, patch, debianize, hackage, rename, method, darcs, cd)
import Debian.Debianize (sourcePackageName, execDebM)
import Debian.Debianize.Prelude ((~=))
import Debian.Relation (SrcPkgName(..))
import Debian.AutoBuilder.Details.Common (repo, privateRepo)
import System.FilePath ((</>))

libraries _home =
    P.Packages (singleton "libraries") $
    [ -- Retired, should be withdrawn from repos
      -- darcs "haskell-generic-formlets3" (privateRepo </> "generic-formlets3")
    -- , darcs "haskell-document" (privateRepo </> "haskell-document")
      darcs "haskell-ontology" (privateRepo </> "haskell-ontology")
    , debianize (darcs "haskell-stripe-core" (privateRepo </> "stripe")
                   `cd` "stripe-core"
                   `patch` $(embedFile "patches/stripe-core.diff"))
    , debianize (darcs "haskell-stripe-http-conduit" (privateRepo </> "stripe")
                   `cd` "stripe-http-conduit")
    , debianize (darcs "haskell-clckwrks-plugin-stripe" (privateRepo </> "clckwrks-plugin-stripe")
                   `flag` P.BuildDep "hsx2hs")
    -- The debian/Debianize.hs script has a dependency on
    -- happstack-foundation, which must be installed in the parent
    -- environment *before* we can create the debianization.  We don't
    -- really have a mechanism to ensure this is installed in the
    -- parent environment, except making it a dependency of the
    -- autobuilder itself.
    , debianize (method "haskell-mimo" (Darcs (privateRepo </> "mimo")))
    ] {- ++ clckwrks14 -}

applications _home =
    P.Packages (singleton "applications") $
    [ debianize (darcs "appraisalscribe" (privateRepo </> "appraisalscribe"))
    , debianize (darcs "appraisalscribe-unstable" (privateRepo </> "appraisalscribe-unstable"))
    -- , debianize (darcs "haskell-artvaluereport2" (privateRepo </> "artvaluereport2"))
    , debianize (darcs "haskell-appraisalscribe-data" (privateRepo </> "appraisalscribe-data"))
    , debianize (darcs "haskell-appraisalscribe-data-unstable" (privateRepo </> "appraisalscribe-data-unstable"))
    , darcs "haskell-seereason" (privateRepo </> "seereason")
    , darcs "haskell-happstack-ontology" (privateRepo </> "happstack-ontology")
    -- Obsolete
    -- , darcs "haskell-creativeprompts" (privateRepo </> "creativeprompts")
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , debianize (darcs "seereason-darcs-backups" (repo </> "autobuilder-config")
                  `flag` P.CabalDebian ["--source-package-name", "seereason-darcs-backups"])
    , debianize (method "seereasonpartners-dot-com"
                      (Cd "seereasonpartners-dot-com" (Darcs (privateRepo </> "seereasonpartners-clckwrks")))
                   `patch` $(embedFile "patches/seereasonpartners-dot-com.diff"))
    , debianize (method "haskell-clckwrks-theme-seereasonpartners"
                      (Cd "clckwrks-theme-seereasonpartners" (Darcs (privateRepo </> "seereasonpartners-clckwrks")))
                   `flag` P.BuildDep "hsx2hs"
                   `flag` P.NoDoc)
    , debianize (method "haskell-clckwrks-theme-appraisalscribe"
                      (Darcs (privateRepo </> "clckwrks-theme-appraisalscribe"))
                   `flag` P.BuildDep "hsx2hs")

    -- Merged into appraisalscribe
    -- , debianize (method "appraisalreportonline-dot-com"
    --                   (Cd "appraisalreportonline-dot-com" (Darcs (privateRepo </> "appraisalreportonline-clckwrks"))))
    -- Theme is now haskell-clckwrks-theme-appraisalscribe
    -- , debianize (method "haskell-clckwrks-theme-appraisalreportonline"
    --                   (Cd "clckwrks-theme-appraisalreportonline" (Darcs (privateRepo </> "appraisalreportonline-clckwrks")))
    --                `flag` P.BuildDep "hsx2hs")
    ]

_clckwrks14 =
      [ P.Package { P.name = "clckwrks-14"
                  , P.spec = Debianize (Hackage "clckwrks")
                  , P.flags = [P.CabalPin "0.14.2",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-clckwrks-14")) ] }
      , debianize (hackage "clckwrks"
                     `rename` "clckwrks-13"
                     `flag` P.CabalPin "0.13.2"
                     `flag` P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-clckwrks-13"))
                     `patch` $(embedFile "patches/clckwrks-13.diff"))
      , P.Package { P.name = "blaze-html-5"
                  , P.spec = Debianize (Hackage "blaze-html")
                  , P.flags = [P.CabalPin "0.5.1.3",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-blaze-html-5")) ] }
      , P.Package { P.name = "happstack-authenticate-9"
                  , P.spec = Debianize (Hackage "happstack-authenticate")
                  , P.flags = [P.CabalPin "0.9.8",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-happstack-authenticate-9")) ] }
      , P.Package { P.name = "http-types-7"
                  , P.spec = Debianize (Hackage "http-types")
                  , P.flags = [P.CabalPin "0.7.3.0.1",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-http-types-7")) ] }
      , debianize (hackage "web-plugins"
                     `patch` $(embedFile "patches/web-plugins.diff")
                     `rename` "web-plugins-1"
                     `flag` P.CabalPin "0.1.2"
                     `flag` P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-web-plugins-1")))
      , P.Package { P.name = "case-insensitive-0"
                  , P.spec = Debianize (Hackage "case-insensitive")
                  , P.flags = [P.CabalPin "0.4.0.4",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "case-insensitive-0")) ] }
      -- Because this target has a debian/Debianize.hs script, the
      -- debianization will be performed by running that rather than
      -- calling callDebianize directly.  That means that the
      -- ModifyAtoms flag here (won't work.  So now what?)
{-
      , P.Package { P.name = "clckwrks-theme-clcksmith"
                  , P.spec = Debianize (Cd "clckwrks-theme-clcksmith" (Darcs (privateRepo </> "clcksmith")))
                  -- Haddock gets upset about the HSX.QQ modules.  Not sure why.
                  , P.flags = [P.BuildDep "haskell-hsx-utils", P.NoDoc] }
      , debianize (P.Package
                        { P.name = "clcksmith"
                        , P.spec = Darcs (privateRepo </> "clcksmith")
                        , P.flags = [] }
                    `patch` $(embedFile "patches/clcksmith.diff")
                    `flag` P.BuildDep "haskell-hsx-utils"
                    `flag` P.CabalDebian ["--missing-dependency", "libghc-clckwrks-theme-clcksmith-doc"])
-}
      ]
