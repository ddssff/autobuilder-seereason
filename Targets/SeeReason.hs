{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.SeeReason (targets) where

import qualified Debian.AutoBuilder.Params as P
import Debian.AutoBuilder.Spec
import Targets.Common

targets _home =
    [ P.Package { P.name = "autobuilder"
                , P.spec = Darcs "http://src.seereason.com/autobuilder" Nothing
                , P.flags = [] }
    , P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-agi"
                , P.spec=Darcs "http://src.seereason.com/haskell-agi" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Darcs "http://src.seereason.com/haskell-debian-repo" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-decimal"
                , P.spec = Darcs "http://src.seereason.com/decimal" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Darcs (repo ++ "/digestive-functors-hsp") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra" Nothing
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-frisby"
                , P.spec = DebDir (Cd "frisby" (Darcs "http://src.seereason.com/frisby" Nothing)) (Darcs "http://src.seereason.com/frisby-debian" Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-authenticate"
                , P.spec = Darcs (repo ++ "/happstack-authenticate") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs "http://patch-tag.com/r/stepcut/happstackDotCom" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-facebook"
                , P.spec = Darcs (repo ++ "/happstack-facebook") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-hsp"
                , P.spec = DebDir (Cd "happstack-hsp" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/happstack-hsp-debian") Nothing)
                , P.flags = [] }
{-  , P.Package { P.name = "haskell-acid-state"
                , P.spec = DebDir (Darcs "http://src.seereason.com/acid-state" Nothing) (Darcs "http://src.seereason.com/haskell-acid-state-debian" Nothing)
                , P.flags = []
                } -}
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help" Nothing
                , P.flags = [] }
{-
    , P.Package { P.name = "haskell-hinotify"
             -- , P.spec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
                , P.spec = DebDir (Darcs "http://src.seereason.com/hinotify" Nothing) (Darcs "http://src.seereason.com/hinotify-debian" Nothing)
                , P.flags = [] }
-}
    , P.Package { P.name = "haskell-hsx-jmacro"
                , P.spec = DebDir (Cd "hsx-jmacro" (Darcs happstackRepo Nothing)) (Darcs (repo ++ "/haskell-hsx-jmacro-debian") Nothing)
                , P.flags = []
                }
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-iconv"
                , P.spec = Darcs "http://src.seereason.com/iconv" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-logic"
                , P.spec = Darcs "http://src.seereason.com/haskell-logic" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs "http://src.seereason.com/haskell-proplogic-debian" Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision" Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-unixutils"
                , P.spec = Darcs (repo ++ "/haskell-unixutils") Nothing
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-happstack"
                , P.spec = Cd "web-routes-happstack" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-mtl"
                , P.spec = Cd "web-routes-mtl" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }      
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Cd "web-routes" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-hsp"
                , P.spec = Cd "web-routes-hsp" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-th"
                , P.spec = Cd "web-routes-th" (Darcs (repo ++ "/web-routes") Nothing)
                , P.flags = [] }
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring" Nothing
                , P.flags = [] }
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs" Nothing
                , P.flags = [] }
    -- All the packages come out empty
    -- , P.Package { P.name = "haskell-logic-tptp"
    --             , P.spec = DebDir (Uri "http://www.cs.miami.edu/~tptp/TPTP/Distribution/TPTP-v5.2.0.tgz" "56370c0928dbdaf48cf0528c96331b32") (Darcs ("" ++ localRepo _home ++ "/logic-TPTP-debian") Nothing)
    --             , P.flags = [] }
    ]
