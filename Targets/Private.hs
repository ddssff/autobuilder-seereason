{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.Private (libraries, applications) where

import Data.Monoid (mconcat)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.RetrieveMethod
import Targets.Common

libraries _home = mconcat $
    [ P.Package { P.name = "haskell-filecache"
                , P.spec = Darcs (privateRepo ++ "/haskell-filecache") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-document"
                , P.spec = Darcs (privateRepo ++ "/haskell-document") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-generic-formlets3"
                , P.spec = Darcs (privateRepo ++ "/generic-formlets3") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-ontology"
                , P.spec = Darcs (privateRepo ++ "/haskell-ontology") Nothing
                , P.flags = [] }
    ]

applications _home = mconcat $
    [ P.Package { P.name = "haskell-artvaluereport"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-artvaluereport2"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport2") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-artvaluereport-data"
                , P.spec = Darcs (privateRepo ++ "/artvaluereport-data") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-mailinglist"
                , P.spec = Darcs (privateRepo ++ "/mailingList") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-seereason"
                , P.spec = Darcs (privateRepo ++ "/seereason") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-ontology"
                , P.spec = Darcs (privateRepo ++ "/happstack-ontology") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-creativeprompts"
                , P.spec = Darcs (privateRepo ++ "/creativeprompts") Nothing
                , P.flags = [] }
{-
    , P.Package { P.name = "prefeteria"
                , P.spec = Darcs (privateRepo ++ "/prefeteria") Nothing
                , P.flags = [] }
-}
    -- There is a debianization in the repo that contains this file
    -- (Targets.hs), and it creates a package named seereason-darcs-backups,
    -- which performs backups on the darcs repo.
    , P.Package { P.name = "seereason-darcs-backups"
                , P.spec = Darcs "http://src.seereason.com/autobuilder-config" Nothing
                , P.flags = [] }
    ]
