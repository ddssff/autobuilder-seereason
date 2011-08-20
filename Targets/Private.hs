{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import qualified Debian.AutoBuilder.Params as P
import Targets.Common

libraries _home =
    [ P.Package { P.name = "haskell-filecache"
                , P.spec = "darcs:" ++ privateRepo ++ "/haskell-filecache"
                , P.flags = [] }
    , P.Package { P.name = "haskell-document"
                , P.spec = "darcs:" ++ privateRepo ++ "/haskell-document"
                , P.flags = [] }
    , P.Package { P.name = "haskell-generic-formlets3"
                , P.spec = "darcs:" ++ privateRepo ++ "/generic-formlets3"
                , P.flags = [] }
    , P.Package { P.name = "haskell-ontology"
                , P.spec = "darcs:" ++ privateRepo ++ "/haskell-ontology"
                , P.flags = [] }
    ]

applications _home =
    [ P.Package { P.name = "haskell-appraisal"
                , P.spec = "darcs:" ++ privateRepo ++ "/artvaluereport"
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-mailinglist"
                , P.spec = "darcs:" ++ privateRepo ++ "/mailingList"
                , P.flags = [] }
    , P.Package { P.name = "haskell-seereason"
                , P.spec = "darcs:" ++ privateRepo ++ "/seereason"
                , P.flags = [] }
    , P.Package { P.name = "haskell-creativeprompts"
                , P.spec = "darcs:" ++ privateRepo ++ "/creativeprompts"
                , P.flags = [] }
    , P.Package { P.name = "prefeteria"
                , P.spec = "darcs:" ++ privateRepo ++ "/prefeteria"
                , P.flags = [] }
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , P.Package { P.name = "seereason-darcs-backups"
                , P.spec = "darcs:http://src.seereason.com/autobuilder-config"
                , P.flags = [] }
    ]
