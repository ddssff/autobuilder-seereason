
failingTargets release =
    [ Target { sourcePackageName = "haskell-uvector-algorithms"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/haskell-uvector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uvector"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.1.0/uvector-0.1.1.0.tar.gz:423e254dbbef0b57687f8adc737f7901):(darcs:http://src.seereason.com/haskell-uvector-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-derive"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/derive/2.3.0.2/derive-2.3.0.2.tar.gz:7f8ad00e17c1cea5ad103b1481dfc250):(darcs:" ++ repo ++ "/haskell-derive-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-geni"
             , sourceSpec="deb-dir:(darcs:http://code.haskell.org/GenI):(darcs:http://src.seereason.com/haskell-geni-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-chart"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.14/Chart-0.14.tar.gz:a7189cd1483d50e2de7f2b20bb3f97d8):(darcs:http://src.seereason.com/haskell-chart-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-guarded-rewriting"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/guarded-rewriting/0.1/guarded-rewriting-0.1.tar.gz:0d4a284236a8a3241d93e8aec014198a):(darcs:http://src.seereason.com/haskell-guarded-rewriting-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-logic-tptp"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/logic-TPTP/0.2.0.1/logic-TPTP-0.2.0.1.tar.gz:5aae329f353bc3aafe2d47162108a02a):(darcs:http://src.seereason.com/logic-TPTP-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-hsx-xhtml"
             , sourceSpec = "deb-dir:(darcs:http://src.seereason.com/hsx-xhtml):(darcs:http://src.seereason.com/hsx-xhtml-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-benchpress"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/shellac-debian)"
             , relaxInfo = [] }
--  , Target { sourcePackageName = "tree-widget"
--           , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "gtk2hs"
--           , sourceSpec = "apt:" ++ sid ++ ":gtk2hs"
--           , relaxInfo = [] }
--  , Target { sourcePackageName = "haskell-restarter"
--           , sourceSpec = "darcs:http://src.seereason.com/Restarter"
--           , relaxInfo = [] } -}
--    , Target { sourcePackageName = "yui-compressor"
--             , sourceSpec = "apt:" ++ sid ++ ":yui-compressor"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "rhino"
--             , sourceSpec = "apt:" ++ sid ++ ":rhino"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "maven-repo-helper"
--             , sourceSpec = "apt:" ++ sid ++ ":maven-repo-helper"
--             , relaxInfo = [] 
--             }
--    , Target { sourcePackageName = "libstax-java"
--             , sourceSpec = "proc:apt:" ++ sid ++ ":libstax-java"
--             , relaxInfo = [] 
--             }
    ]

--    , Target { sourcePackageName = "eclipse-clp"
--             , sourceSpec = "deb-dir:(uri:http://eclipseclp.org/Distribution/6.0_160/src/eclipse_src.tgz:75d074bf0ee66948e6afd3b69e51e81e):(darcs:http://src.seereason.com/eclipse-clp-debian)"
--             , relaxInfo = [] }
--    , Target { sourcePackageName="haskell-logict"
--             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/logict/0.4/logict-0.4.tar.gz:39eeb4aa1d7a67b1c4865f01ca417b7d):(darcs:http://src.seereason.com/debian/haskell-logict-debian)"
--             , relaxInfo = [] }
--    , Target { sourcePackageName = "haskell-special-functors"
--      , sourceSpec =
--      "deb-dir:(uri:http://hackage.haskell.org/packages/archive/special-functors/1.0/special-functors-1.0.tar.gz:4547f0a1b4146d3621bcc95b11148939):(darcs:http://src.seereason.com/haskell-special-functors-debian)"
--      , relaxInfo = [] }
--    , Target { sourcePackageName = "tptp"
--             , sourceSpec = "deb-dir:(uri:http://www.cs.miami.edu/~tptp/TPTP/Distribution/TPTP-v4.1.0.tgz:3cffa92b1def9b8b9865f65d0b775b86):(darcs:http://src.seereason.com/tptp-debian)"
--             , relaxInfo = [] }
