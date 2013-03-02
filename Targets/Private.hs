{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import Data.Set (singleton)
import Data.Lens.Lazy (modL, setL)
import Data.Map as Map (insert)
import Data.Version (Version(Version))
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Debian.Debianize (Atoms, debianNameMap, sourcePackageName, VersionSplits(..))
import Debian.Relation (SrcPkgName(..))
import Distribution.Package (PackageName(PackageName))
import Targets.Common

libraries _home =
    P.Packages (singleton "libraries") $
    [ P.Package { P.name = "haskell-generic-formlets3"
                , P.spec = Darcs (privateRepo ++ "/generic-formlets3")
                , P.flags = [] }
{-  , P.Package { P.name = "haskell-document"
                , P.spec = Darcs (privateRepo ++ "/haskell-document")
                , P.flags = [] } -}
    , P.Package { P.name = "haskell-ontology"
                , P.spec = Darcs (privateRepo ++ "/haskell-ontology")
                , P.flags = [] }
    ] ++ clckwrks14

applications _home =
    P.Packages (singleton "applications") $
    [ P.Package { P.name = "haskell-artvaluereport2"
                , P.spec = Debianize (Darcs (privateRepo ++ "/artvaluereport2"))
                , P.flags = [] }
{-  , P.Package { P.name = "haskell-artvaluereport"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport")
                , P.flags = [] }
    , P.Package { P.name = "appraisalreportonline"
                , P.spec = Debianize (Darcs (privateRepo ++ "/appraisalreportonline"))
                , P.flags = [] } -}
    , P.Package { P.name = "haskell-artvaluereport-data"
                , P.spec = Debianize (Darcs (privateRepo ++ "/artvaluereport-data"))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
{-  , P.Package { P.name = "haskell-happstack-mailinglist"
                , P.spec = Darcs (privateRepo ++ "/mailingList")
                , P.flags = [] } -}
    , P.Package { P.name = "haskell-seereason"
                , P.spec = Darcs (privateRepo ++ "/seereason")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-ontology"
                , P.spec = Darcs (privateRepo ++ "/happstack-ontology")
                , P.flags = [] }
    , P.Package { P.name = "haskell-creativeprompts"
                , P.spec = Darcs (privateRepo ++ "/creativeprompts")
                , P.flags = [] }
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , P.Package { P.name = "seereason-darcs-backups"
                , P.spec = Darcs (repo ++ "/autobuilder-config")
                , P.flags = [] }
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
    , P.Package { P.name = "seereasonpartners-dot-com"
                , P.spec = Debianize (Cd "seereasonpartners-dot-com" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks")))
                , P.flags = [] }
    , P.Package { P.name = "haskell-clckwrks-theme-seereasonpartners"
                , P.spec = Debianize (Cd "clckwrks-theme-seereasonpartners" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils", P.NoDoc] }
    , P.Package { P.name = "appraisalreportonline-dot-com"
                , P.spec = Debianize (Cd "appraisalreportonline-dot-com" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks")))
                , P.flags = [] }
    , P.Package { P.name = "haskell-clckwrks-theme-appraisalreportonline"
                , P.spec = Debianize (Cd "clckwrks-theme-appraisalreportonline" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    ]

extraSplits =
    extraSplit "clckwrks" [0, 15] .
    extraSplit "blaze-html" [0, 6] .
    extraSplit "happstack-authenticate" [0, 10] .
    extraSplit "http-types" [0, 8] .
    extraSplit "case-insensitive" [1]
    where
      extraSplit :: String -> [Int] -> Atoms -> Atoms
      extraSplit base ver =
          modL debianNameMap (Map.insert
                                 (PackageName base)
                                 (VersionSplits {oldestPackage = base ++ "-" ++ show (last ver - 1),
                                                 splits = [(Version ver [], base)]}))

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
