{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Data.Set (singleton)
import Debian.AutoBuilder.Types.Packages as P (PackageFlag(CabalPin, ModifyAtoms, BuildDep, NoDoc, CabalDebian),
                                               Packages(..), RetrieveMethod(Debianize, Hackage, Cd, Darcs, Dir),
                                               flag, flags, spec, patch, debianize, hackage, rename, method, darcs, cd)
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
    , debianize (method (Darcs (privateRepo </> "mimo")))
    ] {- ++ clckwrks14 -}

applications _home =
    P.Packages (singleton "applications") $
    [ debianize (darcs (privateRepo </> "appraisalscribe"))
    , debianize (darcs (privateRepo </> "appraisalscribe-data"))
    , darcs (privateRepo </> "seereason")
    , debianize (darcs (privateRepo </> "happstack-ontology")
                   `flag` P.BuildDep "hsx2hs")
    -- Obsolete
    -- , darcs "haskell-creativeprompts" (privateRepo </> "creativeprompts")
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , debianize (method (Cd "seereasonpartners-dot-com" (Darcs (privateRepo </> "seereasonpartners-clckwrks")))
                   `patch` $(embedFile "patches/seereasonpartners-dot-com.diff"))
    , debianize (method (Cd "clckwrks-theme-seereasonpartners" (Darcs (privateRepo </> "seereasonpartners-clckwrks")))
                   `flag` P.BuildDep "hsx2hs"
                   `flag` P.NoDoc)
    , debianize (method (Darcs (privateRepo </> "clckwrks-theme-appraisalscribe"))
                   `flag` P.BuildDep "hsx2hs")

    -- Merged into appraisalscribe
    -- , debianize (method "appraisalreportonline-dot-com"
    --                   (Cd "appraisalreportonline-dot-com" (Darcs (privateRepo </> "appraisalreportonline-clckwrks")))) []
    -- Theme is now haskell-clckwrks-theme-appraisalscribe
    -- , debianize (method "haskell-clckwrks-theme-appraisalreportonline"
    --                   (Cd "clckwrks-theme-appraisalreportonline" (Darcs (privateRepo </> "appraisalreportonline-clckwrks")))
    --                `flag` P.BuildDep "hsx2hs")
    ]

_clckwrks14 =
      [ P.Package { P.spec = Debianize (Hackage "clckwrks")
                  , P.flags = [P.CabalPin "0.14.2",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-clckwrks-14")) ] }
      , debianize (hackage "clckwrks"
                     `rename` "clckwrks-13"
                     `flag` P.CabalPin "0.13.2"
                     `flag` P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-clckwrks-13"))
                     `patch` $(embedFile "patches/clckwrks-13.diff"))
      , P.Package { P.spec = Debianize (Hackage "blaze-html")
                  , P.flags = [P.CabalPin "0.5.1.3",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-blaze-html-5")) ] }
      , P.Package { P.spec = Debianize (Hackage "happstack-authenticate")
                  , P.flags = [P.CabalPin "0.9.8",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-happstack-authenticate-9")) ] }
      , P.Package { P.spec = Debianize (Hackage "http-types")
                  , P.flags = [P.CabalPin "0.7.3.0.1",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-http-types-7")) ] }
      , debianize (hackage "web-plugins"
                     `patch` $(embedFile "patches/web-plugins.diff")
                     `rename` "web-plugins-1"
                     `flag` P.CabalPin "0.1.2"
                     `flag` P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "haskell-web-plugins-1")))
      , P.Package { P.spec = Debianize (Hackage "case-insensitive")
                  , P.flags = [P.CabalPin "0.4.0.4",
                               P.ModifyAtoms (execDebM $ sourcePackageName ~= Just (SrcPkgName "case-insensitive-0")) ] }
      -- Because this target has a debian/Debianize.hs script, the
      -- debianization will be performed by running that rather than
      -- calling callDebianize directly.  That means that the
      -- ModifyAtoms flag here (won't work.  So now what?)
{-
      , P.Package { P.spec = Debianize (Cd "clckwrks-theme-clcksmith" (Darcs (privateRepo </> "clcksmith")))
                  -- Haddock gets upset about the HSX.QQ modules.  Not sure why.
                  , P.flags = [P.BuildDep "haskell-hsx-utils", P.NoDoc] }
      , debianize (P.Package
                        { P.spec = Darcs (privateRepo </> "clcksmith")
                        , P.flags = [] }
                    `patch` $(embedFile "patches/clcksmith.diff")
                    `flag` P.BuildDep "haskell-hsx-utils"
                    `flag` P.CabalDebian ["--missing-dependency", "libghc-clckwrks-theme-clcksmith-doc"])
-}
      ]
