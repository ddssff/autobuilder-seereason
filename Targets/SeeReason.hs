{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
module Targets.SeeReason (seeReasonTargets) where

import Debian.AutoBuilder.ParamClass (Target(..))
import Targets.Common

seeReasonTargets _home =
    [ Target { sourcePackageName = "autobuilder"
             , sourceSpec = "darcs:http://src.seereason.com/autobuilder"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-agi"
             , sourceSpec="darcs:http://src.seereason.com/haskell-agi"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-archive"
             , sourceSpec = "darcs:http://src.seereason.com/archive"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-consumer"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-consumer"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-mirror"
             , sourceSpec = "darcs:http://src.seereason.com/mirror"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-debian-repo"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-debian-repo"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-decimal"
             , sourceSpec = "darcs:http://src.seereason.com/decimal"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-digestive-functors-hsp"
             , sourceSpec = case build of
                              Production -> "darcs:http://src.seereason.com/digestive-functors-hsp"
                              Testing -> "darcs:" ++ localRepo _home ++ "/digestive-functors-hsp"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-extra"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-extra"
             , relaxInfo = ["cabal-debian"] }
    , Target { sourcePackageName = "haskell-formlets-hsp"
             , sourceSpec = case build of
                              Production -> "darcs:http://src.seereason.com/formlets-hsp"
                              Testing -> "darcs:" ++ localRepo _home ++ "/formlets-hsp"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-authenticate"
             , sourceSpec = case build of
                              Production -> "darcs:" ++ repo ++ "/happstack-authenticate"
                              Testing -> "darcs:" ++ localRepo _home ++ "/happstack-authenticate"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstackdotcom"
             , sourceSpec = case build of
                              Production -> "darcs:http://patch-tag.com/r/stepcut/happstackDotCom"
                              Testing -> "darcs:" ++ localRepo _home ++ "/happstackDotCom"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstackdotcom-doc"
             , sourceSpec = "darcs:http://src.seereason.com/happstackDotCom-doc"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-extra"
             , sourceSpec = case build of
                              Production -> "darcs:http://src.seereason.com/happstack-extra"
                              Testing -> "darcs:" ++ localRepo _home ++ "/happstack-extra"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-facebook"
             , sourceSpec = case build of
                              Production -> "darcs:http://src.seereason.com/happstack-facebook"
                              Testing -> "darcs:" ++ localRepo _home ++ "/happstack-facebook"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-happstack-hsp"
             , sourceSpec = case build of
                              Production -> "deb-dir:(cd:happstack-hsp:darcs:" ++ happstackRepo ++ "):(darcs:http://src.seereason.com/happstack-hsp-debian)"
                              Testing -> "deb-dir:(cd:happstack-hsp:darcs:" ++ happstackRepo ++ "):(darcs:" ++ localRepo _home ++ "/happstack-hsp-debian)"
             , relaxInfo = [] }
    , Target{ sourcePackageName = "haskell-acid-state"
            , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/acid-state):(darcs:http://src.seereason.com/haskell-acid-state-debian)"
            , relaxInfo = []
            }
    , Target { sourcePackageName = "haskell-help"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-help"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hinotify"
             -- , sourceSpec = "deb-dir:(darcs:http://haskell.org/~kolmodin/code/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/hinotify):(darcs:http://src.seereason.com/hinotify-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-jmacro"
             , sourceSpec = "deb-dir:(cd:hsx-jmacro:darcs:" ++ happstackRepo ++ "):(darcs:" ++ repo ++ "/haskell-hsx-jmacro-debian)"
             , relaxInfo = []
             }
    , Target { sourcePackageName = "haskell-html-entities"
             , sourceSpec = "darcs:http://src.seereason.com/html-entities"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-iconv"
             , sourceSpec = "darcs:http://src.seereason.com/iconv"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-logic"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-logic"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-mime"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-mime"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-proplogic"
             , sourceSpec="deb-dir:(uri:http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz:e2fb3445dd16d435e81d7630d7f78c01):(darcs:http://src.seereason.com/haskell-proplogic-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-revision"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-revision"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-unixutils"
             , sourceSpec = "darcs:http://src.seereason.com/haskell-unixutils"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-happstack"
             , sourceSpec = "cd:web-routes-happstack:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-mtl"
             , sourceSpec = "cd:web-routes-mtl:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }      
    , Target { sourcePackageName = "haskell-web-routes"
             , sourceSpec = "cd:web-routes:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-hsp"
             , sourceSpec = "cd:web-routes-hsp:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-web-routes-th"
             , sourceSpec = "cd:web-routes-th:darcs:" ++ repo ++ "/web-routes"
             , relaxInfo = [] }
    , Target { sourcePackageName = "seereason-keyring"
             , sourceSpec = "darcs:http://src.seereason.com/seereason-keyring"
             , relaxInfo = [] }
    , Target { sourcePackageName = "vc-darcs"
             , sourceSpec = "darcs:http://src.seereason.com/vc-darcs"
             , relaxInfo = [] }
    ]