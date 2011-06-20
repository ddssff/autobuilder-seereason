{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (privateTargets) where

import Debian.AutoBuilder.ParamClass (Target(..))

privateRepo = "ssh://upload@src.seereason.com/srv/darcs"

privateTargets home =
    [ Target { sourcePackageName = "haskell-filecache"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-filecache"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-document"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-document"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-appraisal"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/artvaluereport"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-mailinglist"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/mailingList"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-generic-formlets3"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/generic-formlets3"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-ontology"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/haskell-ontology"
             , relaxInfo = [] }
{-  , Target { sourcePackageName = "haskell-happstack-examples"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/happstack-examples"
             , relaxInfo = [] } -}

--    , Target { sourcePackageName = "happstack-blog"
--             , sourceSpec = "darcs:" ++ privateRepo ++ "/happstack-cms"
--             , relaxInfo = [] }
--    , Target { sourcePackageName = "happstack-imagegallery"
--             , sourceSpec = "darcs:" ++ privateRepo ++ "/imagegallery"
--             , relaxInfo = [] }
{-  -- Uses newSession, which was removed from happstack
    , Target { sourcePackageName = "haskell-algebrazam"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/AlgebraZam"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-algebrazam-quiz"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/algebrazam-quiz"
             , relaxInfo = [] } -}
{-  -- Compile error at the moment, but this package is not a current priority.
    , Target { sourcePackageName = "haskell-senioritymatters"
             , sourceSpec = "darcs:" ++ privateRepo ++ "/SeniorityMatters"
             , relaxInfo = [] } -}
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
