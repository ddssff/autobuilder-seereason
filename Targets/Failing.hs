
failingTargets release =
    [ Target { sourcePackageName = "haskell-uvector-algorithms"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector-algorithms/0.2/uvector-algorithms-0.2.tar.gz:5d4088a73dd174fc0ef74b43f91443fa):(darcs:http://src.seereason.com/haskell-uvector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-uvector"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/uvector/0.1.1.0/uvector-0.1.1.0.tar.gz:423e254dbbef0b57687f8adc737f7901):(darcs:http://src.seereason.com/haskell-uvector-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-statistics"
             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/statistics/0.3.5/statistics-0.3.5.tar.gz:b351bee9514e26555f170676b3c66139):(darcs:http://src.seereason.com/haskell-statistics-debian)"
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
    , Target { sourcePackageName = "haskell-vector-algorithms"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/vector-algorithms/0.3.4/vector-algorithms-0.3.4.tar.gz:1457802a2e0babf239c31b45d09d6b40):(darcs:http://src.seereason.com/haskell-vector-algorithms-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-benchpress"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/benchpress/0.2.2.3/benchpress-0.2.2.3.tar.gz:48cd691ebfd4dc6c5e6f5201ca545fac):(darcs:http://src.seereason.com/debian/haskell-benchpress-debian)"
             , relaxInfo = [] }
    , Target { sourcePackageName = "haskell-shellac"
             , sourceSpec = "deb-dir:(uri:http://hackage.haskell.org/packages/archive/Shellac/0.9.1/Shellac-0.9.1.tar.gz:0a563883b3acedb9c0d4308b44772f0f):(darcs:http://src.seereason.com/shellac-debian)"
             , relaxInfo = [] }
    -- We need debhelper >= 7.0.50 for darcs 2.3.0.
    -- However, one of its unit tests fails:
    -- #   Failed test 'unavailable jobserver'
    -- #   at t/buildsystems/buildsystem_tests line 540.
--  , Target { sourcePackageName = "debhelper"
--           , sourceSpec = "apt:" ++ sid ++ ":debhelper"
--           , relaxInfo = [] } -}
--  -- Required for darcs 2.3.0
--  -- Fails because of missing dependency libssh2-1-dev
--  , Target { sourcePackageName = "curl"
--           , sourceSpec = "apt:" ++ sid ++ ":curl"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "tree-widget"
--           , sourceSpec = "darcs:http://src.seereason.com/tree-widget"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "gtk2hs"
--           , sourceSpec = "apt:" ++ sid ++ ":gtk2hs"
--           , relaxInfo = [] }
--  , Target { sourcePackageName="haskell-chart"
--           , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/Chart/0.11/Chart-0.11.tar.gz:b7f67defe06694eef580542947106fc0):(darcs:http://src.seereason.com/haskell-chart-debian)"
--           , relaxInfo = [] }
--  , Target { sourcePackageName="haskell-criterion"
--           , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/criterion/0.1.2/criterion-0.1.2.tar.gz:0e4d1c2f546ab650e03c610034c20226):(darcs:http://src.seereason.com/haskell-criterion-debian)"
--           , relaxInfo = [] } -}
--  , Target { sourcePackageName = "haskell-restarter"
--           , sourceSpec = "darcs:http://src.seereason.com/Restarter"
--           , relaxInfo = [] } -}
--    , Target { sourcePackageName = "jquery"
--             , sourceSpec = "apt:" ++ sid ++ ":jquery"
--             , relaxInfo = [] 
--             }
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
--    , Target { sourcePackageName = "jqueryui"
--             , sourceSpec = "apt:" ++ sid ++ ":jqueryui"
--             , relaxInfo = [] 
--             }
    ]
