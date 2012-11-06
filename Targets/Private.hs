{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import Data.Set (singleton)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Targets.Common

libraries _home =
    P.Packages (singleton "libraries") $
    [ P.Package { P.name = "haskell-document"
                , P.spec = Darcs (privateRepo ++ "/haskell-document")
                , P.flags = [] }
    , P.Package { P.name = "haskell-generic-formlets3"
                , P.spec = Darcs (privateRepo ++ "/generic-formlets3")
                , P.flags = [] }
    , P.Package { P.name = "haskell-ontology"
                , P.spec = Darcs (privateRepo ++ "/haskell-ontology")
                , P.flags = [] }
    ]

applications _home =
    P.Packages (singleton "applications") $
    [ P.Package { P.name = "haskell-artvaluereport"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport")
                , P.flags = [] }
    , P.Package { P.name = "haskell-artvaluereport2"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport2")
                , P.flags = [] }
    , P.Package { P.name = "haskell-artvaluereport-data"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport-data")
                , P.flags = [] }
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
{-
    , P.Package { P.name = "prefeteria"
                , P.spec = Darcs (privateRepo ++ "/prefeteria") []
                , P.flags = [] }
-}
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , P.Package { P.name = "seereason-darcs-backups"
                , P.spec = Debianize (Darcs (privateRepo ++ "/clcksmith"))
                , P.flags = [] }
    , P.Package { P.name = "clcksmith"
                , P.spec = Darcs (privateRepo ++ "/clcksmith")
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    , P.Package { P.name = "clckwrks-theme-clcksmith"
                , P.spec = Debianize (Cd "clckwrks-theme-clcksmith" (Darcs (privateRepo ++ "/clcksmith")))
                -- Haddock gets upset about the HSX.QQ modules.  Not sure why.
                , P.flags = [P.ExtraDep "haskell-hsx-utils", P.NoDoc, P.DebVersion "0.1-1~hackage1"] }
    , P.Package { P.name = "seereasonpartners-dot-com"
                , P.spec = Cd "seereasonpartners-dot-com" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-clckwrks-theme-seereasonpartners"
                , P.spec = Debianize (Cd "clckwrks-theme-seereasonpartners" (Darcs (privateRepo ++ "/seereasonpartners-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils", P.NoDoc] }
    , P.Package { P.name = "appraisalreportonline-dot-com"
                , P.spec = Cd "appraisalreportonline-dot-com" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-clckwrks-theme-appraisalreportonline"
                , P.spec = Debianize (Cd "clckwrks-theme-appraisalreportonline" (Darcs (privateRepo ++ "/appraisalreportonline-clckwrks")))
                , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
    ]
