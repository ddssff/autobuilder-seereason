{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

libraries _home =
    [ Target { sourcePackageName = "haskell-filecache"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-filecache"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-document"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-document"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-generic-formlets3"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/generic-formlets3"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ontology"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-ontology"
             , relaxInfo = [] }
    ]

applications _home =
    [ Target { sourcePackageName = "haskell-appraisal"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/artvaluereport"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-mailinglist"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/mailingList"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-seereason"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/seereason"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-creativeprompts"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/creativeprompts"
             , relaxInfo = [] }
    , Target { sourcePackageName = "prefeteria"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/prefeteria"
             , relaxInfo = [] }
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , Target { sourcePackageName = "seereason-darcs-backups"
             , sourceSpec = "darcs:http://src.seereason.com/autobuilder-config"
             , relaxInfo = [] }
    ]
