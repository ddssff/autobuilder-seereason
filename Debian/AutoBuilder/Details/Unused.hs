
-- Broken targets:
--
-- Text/JSONb/Decode.hs:48:3:
--     Not in scope: data constructor `Done'
--     Perhaps you meant `Attoparsec.Done' (imported from Data.Attoparsec.Char8)
--
-- Text/JSONb/Decode.hs:49:3:
--     Not in scope: data constructor `Fail'
--     Perhaps you meant `Attoparsec.Fail' (imported from Data.Attoparsec.Char8)
--
-- Text/JSONb/Decode.hs:50:3:
--     Not in scope: data constructor `Partial'
--     Perhaps you meant `Attoparsec.Partial' (imported from Data.Attoparsec.Char8)
jsonb = P.Packages (singleton "jsonb") $
    [ debianize (hackage "JSONb" `flag` P.DebVersion "1.0.7-1~hackage1")
    , debianize (hackage "data-object-json") ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib _release = P.Packages (singleton "glib") $
    [ debianize (hackage "glib")
                    `flag` P.BuildDep "haskell-gtk2hs-buildtools-utils"
                    `flag` P.BuildDep "libglib2.0-dev"
    , apt "wheezy" "haskell-criterion"
    , apt "wheezy" "haskell-ltk"
    , apt "wheezy" "haskell-chart"
    , apt "wheezy" "haskell-gio"
    , apt "wheezy" "haskell-gtk"
    , apt "wheezy" "haskell-gtksourceview2"
    , apt "wheezy" "haskell-pango" ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = P.Packages (singleton "higgsset") $
    [ debianize (hackage "unpack-funcs")
    , debianize (hackage "HiggsSet")
    , debianize (hackage "TrieMap" `flag` P.DebVersion "4.0.1-1~hackage1") ]

frisby = P.Packages (singleton "frisby")
    [ darcs "haskell-frisby" (repo </> "frisby")
        `debdir` (Darcs (repo </> "frisby-debian"))
        `cd` "frisby"
    , darcs "haskell-decimal" (repo </> "decimal") ]

haddock _release =
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base ==
    -- 4.3.* so we can't use "debianize "haddock" []".  I don't think
    -- we really need this, or the hackage version.  Version 2.10.0 is
    -- included with ghc 7.4.0.
    [ apt "wheezy" "haskell-haddock" ]

-- These have been failing for some time, and I don't think we've missed them.
failing _release =
    [ debianize (hackage "funsat")
    , apt "wheezy" "haskell-statistics"
    , debianize (hackage "cryptocipher"
                   `patch`  $(embedFile "patches/cryptocipher.diff"))
    ]

diagrams = P.Packages (singleton "diagrams")
    [ debianize (hackage "diagrams")
    , debianize (hackage "diagrams-lib")
    , debianize (hackage "diagrams-builder")
    , debianize (hackage "diagrams-core")
    , debianize (hackage "diagrams-contrib")
    , debianize (hackage "diagrams-gtk")
    , debianize (hackage "diagrams-cairo")
    , debianize (hackage "diagrams-svg")
    , debianize (hackage "dual-tree")
    , debianize (hackage "monoid-extras")
    , debianize (hackage "newtype")
    , debianize (hackage "active")
    , debianize (hackage "Boolean")
    , debianize (hackage "MemoTrie")
    , debianize (hackage "blaze-svg")
    , debianize (hackage "force-layout")
    , debianize (hackage "cairo")
    , debianize (hackage "hint")
    , debianize (hackage "vector-space")
    , debianize (hackage "vector-space-points")
    , debianize (hackage "MonadCatchIO-mtl")
    ]

-- Debian package has versioned dependencies on binary, but the
-- virtual binary package provided with ghc 7.4 (0.5.1.0) is
-- newer than the version of binary in hackage (0.5.0.2.)  This
-- means we try to pull in bogus debs for libghc-binary-* and
-- dependency problems ensue.
agda _release =
    [ apt "wheezy" "agda"
    , apt "wheezy" "agda-bin"
    , apt "wheezy" "agda-stdlib" ]

other _release =
    [ apt "wheezy" "darcs"
    , debianize (hackage "aeson-native" `patch` $(embedFile "patches/aeson-native.diff"))
    , apt "wheezy" "haskell-binary-shared" -- for leksah
    , debianize (hackage "cairo" `flag` P.BuildDep "haskell-gtk2hs-buildtools-utils") -- for leksah
    , debianize (hackage "cabal-dev") -- build-env for cabal
    , debianize (hackage "gnuplot" `flag` P.DebVersion "0.4.2-1~hackage1")
    , apt "wheezy" "bash-completion"
    ]
