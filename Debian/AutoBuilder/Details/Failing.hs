
failingTargets release =
    [ P.Package { P.name = "haskell-uvector-algorithms"
             , P.spec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/haskell-uvector-algorithms-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-uvector"
             , P.spec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.1.0/uvector-0.1.1.0.tar.gz:423e254dbbef0b57687f8adc737f7901):(darcs:http://src.seereason.com/haskell-uvector-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-derive"
             , P.spec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/derive/2.3.0.2/derive-2.3.0.2.tar.gz:7f8ad00e17c1cea5ad103b1481dfc250):(darcs:" ++ repo ++ "/haskell-derive-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-geni"
             , P.spec="deb-dir:(darcs:http://code.haskell.org/GenI):(darcs:http://src.seereason.com/haskell-geni-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-chart"
             , P.spec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.14/Chart-0.14.tar.gz:a7189cd1483d50e2de7f2b20bb3f97d8):(darcs:http://src.seereason.com/haskell-chart-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-guarded-rewriting"
             , P.spec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/guarded-rewriting/0.1/guarded-rewriting-0.1.tar.gz:0d4a284236a8a3241d93e8aec014198a):(darcs:http://src.seereason.com/haskell-guarded-rewriting-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-hsx-xhtml"
             , P.spec = "deb-dir:(darcs:http://src.seereason.com/hsx-xhtml):(darcs:http://src.seereason.com/hsx-xhtml-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-benchpress"
             , P.spec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , P.flags = [] }
    , P.Package { P.name = "haskell-shellac"
             , P.spec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/shellac-debian)"
             , P.flags = [] }
--  , P.Package { P.name = "tree-widget"
--           , P.spec = "darcs:http://src.seereason.com/tree-widget"
--           , P.flags = [] } -}
--  , P.Package { P.name = "gtk2hs"
--           , P.spec = "apt:" ++ sid ++ ":gtk2hs"
--           , P.flags = [] }
--  , P.Package { P.name = "haskell-restarter"
--           , P.spec = "darcs:http://src.seereason.com/Restarter"
--           , P.flags = [] } -}
--    , P.Package { P.name = "yui-compressor"
--             , P.spec = "apt:" ++ sid ++ ":yui-compressor"
--             , P.flags = [] 
--             }
--    , P.Package { P.name = "rhino"
--             , P.spec = "apt:" ++ sid ++ ":rhino"
--             , P.flags = [] 
--             }
--    , P.Package { P.name = "maven-repo-helper"
--             , P.spec = "apt:" ++ sid ++ ":maven-repo-helper"
--             , P.flags = [] 
--             }
--    , P.Package { P.name = "libstax-java"
--             , P.spec = "proc:apt:" ++ sid ++ ":libstax-java"
--             , P.flags = [] 
--             }
    ]

--    , P.Package { P.name = "eclipse-clp"
--             , P.spec = "deb-dir:(uri:http://eclipseclp.org/Distribution/6.0_160/src/eclipse_src.tgz:75d074bf0ee66948e6afd3b69e51e81e):(darcs:http://src.seereason.com/eclipse-clp-debian)"
--             , P.flags = [] }
--    , P.Package { P.name="haskell-logict"
--             , P.spec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/logict/0.4/logict-0.4.tar.gz:39eeb4aa1d7a67b1c4865f01ca417b7d):(darcs:http://src.seereason.com/debian/haskell-logict-debian)"
--             , P.flags = [] }
--    , P.Package { P.name = "haskell-special-functors"
--      , P.spec =
--      "deb-dir:(uri:http://hackage.haskell.org/packages/archive/special-functors/1.0/special-functors-1.0.tar.gz:4547f0a1b4146d3621bcc95b11148939):(darcs:http://src.seereason.com/haskell-special-functors-debian)"
--      , P.flags = [] }
--    , P.Package { P.name = "tptp"
--             , P.spec = "deb-dir:(uri:http://www.cs.miami.edu/~tptp/TPTP/Distribution/TPTP-v4.1.0.tgz:3cffa92b1def9b8b9865f65d0b775b86):(darcs:http://src.seereason.com/tptp-debian)"
--             , P.flags = [] }
