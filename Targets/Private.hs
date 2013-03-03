{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import Data.FileEmbed (embedFile)
import Data.Lens.Lazy (modL, setL)
import Data.Map as Map (alter)
import Data.Set (singleton)
import Data.Version (Version(Version))
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Debian.Debianize (Atoms, debianNameMap, sourcePackageName, VersionSplits(..), insertSplit)
import Debian.Relation (SrcPkgName(..))
import Distribution.Package (PackageName(PackageName))
import Targets.Common

libraries _home =
    P.Packages (singleton "libraries") $
    [ darcs "haskell-generic-formlets3" (privateRepo ++ "/generic-formlets3")
    -- , darcs "haskell-document" (privateRepo ++ "/haskell-document")
    , darcs "haskell-ontology" (privateRepo ++ "/haskell-ontology")
    ] ++ clckwrks14

applications _home =
    P.Packages (singleton "applications") $
    [ debianize (darcs "haskell-artvaluereport2" (privateRepo ++ "/artvaluereport2"))
    , debianize (darcs "haskell-artvaluereport-data" (privateRepo ++ "/artvaluereport-data"))
    , darcs "haskell-seereason" (privateRepo ++ "/seereason")
    , darcs "haskell-happstack-ontology" (privateRepo ++ "/happstack-ontology")
    , darcs "haskell-creativeprompts" (privateRepo ++ "/creativeprompts")
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , darcs "seereason-darcs-backups" (repo ++ "/autobuilder-config")
{-
    , P.Package { P.name = "clcksmith"
                , P.spec = Debianize (Darcs (privateRepo ++ "/clcksmith"))
                , P.flags = [P.ExtraDep "haskell-hsx-utils",
                             P.CabalDebian ["--missing-dependency", "libghc-clckwrks-theme-clcksmith-doc"],
                             P.ModifyAtoms extraSplits] }
    , P.Package { P.name = "clckwrks-theme-clcksmith"
                , P.spec = Debianize (Cd "clckwrks-theme-clcksmith" (Darcs (privateRepo ++ "/clcksmith")))
                -- Haddock gets upset about the HSX.QQ modules.  Not sure why.
                , P.flags = [P.ExtraDep "haskell-hsx-utils", P.NoDoc] }
-}
    , debianize (method "seereasonpartners-dot-com"
                      (Cd "seereasonpartners-dot-com" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks"))))
    , debianize (method "haskell-clckwrks-theme-seereasonpartners"
                      (Cd "clckwrks-theme-seereasonpartners" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks")))
                   `flag` P.ExtraDep "haskell-hsx-utils"
                   `flag` P.NoDoc)
    , debianize (method "appraisalreportonline-dot-com"
                      (Cd "appraisalreportonline-dot-com" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks"))))
    , debianize (method "haskell-clckwrks-theme-appraisalreportonline"
                      (Cd "clckwrks-theme-appraisalreportonline" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks")))
                   `flag` P.ExtraDep "haskell-hsx-utils")
    ]

-- | All packages that want to use the older packages in clckwrks14
-- need to apply extraSplits to their debianization.
extraSplits :: Atoms -> Atoms
extraSplits =
    extraSplit "clckwrks" (Version [0, 15] []) .
    extraSplit "clckwrks" (Version [0, 14] []) .
    extraSplit "blaze-html" (Version [0, 6] []) .
    extraSplit "happstack-authenticate" (Version [0, 10] []) .
    extraSplit "http-types" (Version [0, 8] []) .
    extraSplit "case-insensitive" (Version [1] [])
    where
      extraSplit base ver =
          modL debianNameMap (Map.alter (Just . f) (PackageName base))
          where
            f Nothing = insertSplit base ver (VersionSplits {oldestPackage = base, splits = []})
            f (Just sp) = insertSplit base ver sp

clckwrks14 =
      [ P.Package { P.name = "clckwrks-14"
                  , P.spec = Debianize (Hackage "clckwrks")
                  , P.flags = [P.CabalPin "0.14.2",
                               P.ModifyAtoms (setL sourcePackageName (Just (SrcPkgName "haskell-clckwrks-14")) . extraSplits) ] }
      , P.Package { P.name = "blaze-html-5"
                  , P.spec = Debianize (Hackage "blaze-html")
                  , P.flags = [P.CabalPin "0.5.1.3",
                               P.ModifyAtoms (setL sourcePackageName (Just (SrcPkgName "haskell-blaze-html-5")) . extraSplits) ] }
      , P.Package { P.name = "happstack-authenticate-9"
                  , P.spec = Debianize (Hackage "happstack-authenticate")
                  , P.flags = [P.CabalPin "0.9.8",
                               P.ModifyAtoms (setL sourcePackageName (Just (SrcPkgName "haskell-happstack-authenticate-9")) . extraSplits) ] }
      , P.Package { P.name = "http-types-7"
                  , P.spec = Debianize (Hackage "http-types")
                  , P.flags = [P.CabalPin "0.7.3.0.1",
                               P.ModifyAtoms (setL sourcePackageName (Just (SrcPkgName "haskell-http-types-7")) . extraSplits) ] }
      , P.Package { P.name = "case-insensitive-0"
                  , P.spec = Debianize (Hackage "case-insensitive")
                  , P.flags = [P.CabalPin "0.4.0.4",
                               P.ModifyAtoms (setL sourcePackageName (Just (SrcPkgName "case-insensitive-0")) . extraSplits) ] }
      ]

rename :: P.Packages -> TargetName -> P.Packages
rename p s = p {P.name = s}
