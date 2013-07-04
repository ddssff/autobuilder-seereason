{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Public ( targets ) where

import Data.FileEmbed (embedFile)
import Data.Lens.Lazy (setL, modL)
import Data.Map as Map (insertWith)
import Data.Monoid ((<>))
import Data.Set as Set (empty, singleton, union)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (RetrieveMethod(..), PackageFlag(CabalPin), rename,
                                          hackage, debianize, flag, patch, darcs, apt, git, debdir, mflag, cd)
import Debian.Debianize (compat)
import Debian.Relation (BinPkgName(..))
import Debian.Debianize (installData, doExecutable, InstallFile(..))
import System.FilePath((</>))
import Debian.AutoBuilder.Details.Common (repo, {-localRepo,-} happstackRepo)

patchTag :: String
patchTag = "http://patch-tag.com/r/stepcut"
darcsHub :: String
darcsHub = "http://hub.darcs.net/stepcut"

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> P.Packages
targets _home release =
    P.Packages empty $
    [ main _home release
    , autobuilder _home
    , clckwrks _home release
    -- , authenticate _home release
    -- , happstackdotcom _home
    -- , happstack release
    , digestiveFunctors

    , algebra release
    -- , diagrams
    , fixme
    -- , higgsset
    -- , jsonb
    -- , glib
    -- , plugins
    -- , frisby
    -- , failing
    -- , agda
    -- , other
    ]

rel :: String -> a -> a -> a
rel release precise quantal =
    case release of
      "quantal-seereason" -> quantal
      _ -> precise

-- | We don't currently support ghc 7.4
ghc74flag :: P.Packages -> P.PackageFlag -> P.Packages
ghc74flag p _ = p

fixme =
    P.Packages (singleton "fixme") $
    [ debianize (hackage "test-framework-smallcheck")
    , debianize (darcs "haskell-geni" "http://hub.darcs.net/kowey/GenI" `patch` $(embedFile "patches/GenI.diff"))
    ]

autobuilder home =
    -- let repo = localRepo home in
    P.Packages (singleton "autobuilder-group") $
    [ unixutils home
    , debianize (darcs "autobuilder" (repo </> "debian-tools-revert") `cd` "autobuilder")
    , darcs "haskell-cabal-debian" (repo </> "debian-tools-revert") `cd` "cabal-debian"
    , darcs "haskell-debian" (repo ++ "/haskell-debian") `flag` P.RelaxDep "cabal-debian"
    , darcs "haskell-debian-mirror" (repo </> "mirror")
    , darcs "haskell-debian-repo" (repo </> "debian-tools-revert") `cd` "debian-repo"
    , darcs "haskell-archive" (repo </> "archive")
    -- , debianize (darcs "haskell-process-extras" (repo </> "process-extras"))
    -- , debianize (hackage "process-listlike")
    , debianize (darcs "haskell-process-listlike" (repo </> "process-listlike"))
    , darcs "haskell-process-progress" (repo </> "debian-tools") `cd` "process-progress"
    ]

unixutils _home =
    P.Packages (singleton "Unixutils")
    [ darcs "haskell-unixutils" (repo ++ "/haskell-unixutils")
    , darcs "haskell-extra" (repo </> "haskell-extra") `flag` P.RelaxDep "cabal-debian"
    , darcs "haskell-help" (repo </> "haskell-help") ]

main _home release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p
        sflag = case release of "squeeze-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "main") $
    [ compiler release
    , platform release
    , debianize (hackage "hashtables")
    , apt "squeeze" "bugzilla"
    , debianize (hackage "ListLike")
    -- Merged into ListLike-4.0
    -- , debianize (hackage "listlike-instances")
    , apt (rel release "wheezy" "quantal") "cpphs"
    , apt "sid" "debootstrap" `flag` P.UDeb "debootstrap-udeb"
    -- Build fails due to some debianization issue
    -- , apt "wheezy" "geneweb"
    , debianize (hackage "gtk2hs-buildtools"
                   `rename` "gtk2hs-buildtools"
                   `flag` P.BuildDep "alex"
                   `flag` P.BuildDep "happy"
                   `flag` P.Revision "")
    -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize (hackage "aeson")
    , darcs "haskell-agi" (repo </> "haskell-agi")
    , debianize (hackage "ansi-terminal")
    , debianize (hackage "ansi-wl-pprint")
    , debianize (hackage "wl-pprint-text")
    -- Our applicative-extras repository has several important patches.
    , debianize (hackage "applicative-extras" `flag` P.DebVersion "0.1.8-1")
    , debianize (hackage "asn1-data")
    , debianize (hackage "attempt" `pflag` P.DebVersion "0.4.0-1" `qflag` P.DebVersion "0.4.0-1build2")
    , debianize (hackage "errors")
    , debianize (hackage "failure" `qflag` P.DebVersion "0.2.0.1-1build2")
    , debianize (hackage "attoparsec")
    , debianize (hackage "attoparsec-enumerator")
    -- This was merged into attoparsec
    -- , debianize (hackage "attoparsec-text" `patch` $(embedFile "patches/attoparsec-text.diff") `flag` P.Revision "")
    -- Deprecated
    -- , debianize (hackage "attoparsec-text-enumerator")
    , debianize (hackage "base16-bytestring")
    , debianize (hackage "base-unicode-symbols")
    , debianize (hackage "bimap" `flag` P.DebVersion "0.2.4-1~hackage1")
    , debianize (hackage "Validation" `patch` $(embedFile "patches/validation.diff"))
    , debianize (hackage "data-default")
    , debianize (hackage "template-default"
                -- `patch` $(embedFile "patches/template-default.diff")
                )
    , debianize (hackage "bitmap")
    , debianize (hackage "bitset")
    , apt (rel release "wheezy" "quantal") "haskell-bytestring-nums"
    , debianize (hackage "bytestring-trie")
    , debianize (hackage "bzlib" `flag` P.DevelDep "libbz2-dev")
    -- , debianize (hackage "cairo-pdf")
    , debianize (hackage "case-insensitive")
    , debianize (hackage "cabal-install"
                   `patch` $(embedFile "patches/cabal-install.diff"))
    -- , debianize (git "haskell-cabal-install" "https://github.com/haskell/cabal"
    --                      `cd` "cabal-install"
    --                      `patch` $(embedFile "patches/cabal-install.diff")) -}
    , debianize (hackage "CC-delcont" `flag` P.DebVersion "0.2-1~hackage1")
    , apt (rel release "wheezy" "quantal") "haskell-cereal"
    , debianize (hackage "citeproc-hs")
    , debianize (hackage "hexpat")
    , debianize (hackage "List")
    , debianize (hackage "uuid")
    , debianize (hackage "maccatcher" `flag` P.DebVersion "2.1.5-3")
    , debianize (hackage "colour"
                   `pflag` P.DebVersion "2.3.3-1build1"
                   `qflag` P.DebVersion "2.3.3-1build1"
                   `sflag` P.DebVersion "2.3.3-1")
    -- , apt "wheezy" "haskell-configfile"
    , debianize (hackage "ConfigFile")
    , darcs "haskell-consumer" (repo </> "haskell-consumer")
    , darcs "haskell-consumer" (repo </> "haskell-consumer")
    , debianize (darcs "haskell-module-management" (repo </> "module-management")
                   `flag` P.BuildDep "rsync")
    , debianize (hackage "cipher-aes")
    , debianize (hackage "cprng-aes")
    , debianize (hackage "crypto-random-api")
    , debianize (hackage "Crypto")
    , debianize (hackage "crypto-api" `qflag` P.DebVersion "0.10.2-1build3")
    -- The certificate package may need to be updated for version 0.4
    , debianize (hackage "crypto-pubkey-types")
    -- crypto-pubkey-types-0.3.2 depends on older asn1-types
    , debianize (hackage "asn1-types" {- `flag` CabalPin "0.1.3" -})
    , debianize (hackage "byteable")
    , debianize (hackage "cryptohash")
    , debianize (hackage "cpu" `qflag` P.DebVersion "0.1.1-1build1")
    , debianize (hackage "css" `flag` P.DebVersion "0.1-1~hackage1")
    , debianize (hackage "css-text" `pflag` P.DebVersion "0.1.1-3" `qflag` P.DebVersion "0.1.1-3build1")
    , apt (rel release "wheezy" "quantal") "haskell-curl"
    , debianize (hackage "data-accessor")
    , debianize (hackage "data-accessor-template")
    , debianize (hackage "data-default")
    , debianize (hackage "data-default-class")
    , debianize (hackage "data-default-instances-base")
    , debianize (hackage "data-default-instances-containers")
    , debianize (hackage "data-default-instances-dlist")
    , debianize (hackage "data-default-instances-old-locale")
    , debianize (hackage "data-object"
                   `patch` $(embedFile "patches/data-object.diff"))
    , debianize (hackage "dataenc")
    , debianize (hackage "Diff")
    , debianize (hackage "executable-path" `flag` P.DebVersion "0.0.3-1")
    , apt (rel release "wheezy" "quantal") "haskell-digest"
    , apt (rel release "wheezy" "quantal") "haskell-dlist"
    -- Natty only(?)
    , debianize (hackage "double-conversion")
    , debianize (hackage "groom")
    -- , apt "wheezy" "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Debianize (Hackage "EdisonAPI")
                , P.flags = rel release [] [P.DebVersion "1.2.1-18build2"] }
    , (debianize (hackage "EdisonCore" `qflag` P.DebVersion "1.2.1.3-9build2"))
    , apt (rel release "wheezy" "quantal") "haskell-entropy"
    , debianize (hackage "enumerator" `qflag` P.DebVersion "0.4.19-1build2")
    , debianize (hackage "hdaemonize"
                   `patch` $(embedFile "patches/hdaemonize.diff"))
    , debianize (hackage "hsyslog")
    , debianize (hackage "erf" `flag` P.DebVersion "2.0.0.0-3")
    , debianize (hackage "feed") `flag` (rel release (P.DebVersion "0.3.8-3") (P.DebVersion "0.3.8-3build2"))
    -- Darcs 2.8.1 won't build with the current version of haskeline.
    -- , apt "wheezy" "darcs" `patch` $(embedFile "patches/darcs.diff")
    , debianize (hackage "file-embed")
    , debianize (hackage "indents")
    , debianize (hackage "concatenative")
    , debianize (hackage "either")
    , debianize (hackage "MonadRandom")
    , P.Package { P.name = "haskell-formlets"
                , P.spec = Debianize (Patch (Hackage "formlets") $(embedFile "patches/formlets.diff"))
                , P.flags = [P.DebVersion "0.8-1~hackage1"] }
    , P.Package { P.name = "haskell-gd"
                , P.spec = Debianize (Patch (Hackage "gd") $(embedFile "patches/gd.diff"))
                , P.flags = [ P.DevelDep "libgd-dev", P.DevelDep "libc6-dev", P.DevelDep "libfreetype6-dev" ] ++ rel release [] [P.DebVersion "3000.7.3-1build2"] }
    -- , debianize (flags [P.BuildDep "libm-dev", P.BuildDep "libfreetype-dev"] (hackage "gd"))
    , debianize (hackage "cabal-macosx")
    , apt (rel release "wheezy" "quantal") "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , apt (rel release "wheezy" "quantal") "haskell-harp"
    , debianize (hackage "hashable" `flag` CabalPin "1.1.2.5")
    , debianize (hackage "hashed-storage")
    , debianize (hackage "haskeline" `flag` P.DebVersion "0.7.0.3-1~hackage1")
    , debianize (hackage "th-orphans"
                   `patch` $(embedFile "patches/th-orphans.diff")
                   `flag` P.DebVersion "0.6-1~hackage1")
    , debianize (hackage "haskell-src-meta")
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize (hackage "HaXml")
    , debianize (hackage "heap" `flag` P.DebVersion "1.0.0-1~hackage1")
    , debianize (hackage "heist")
    , debianize (hackage "xmlhtml")
    , debianize (hackage "directory-tree")
    , debianize (hackage "MonadCatchIO-transformers" `qflag` P.DebVersion "0.3.0.0-2build2")
    , debianize (hackage "MonadCatchIO-mtl")
    , debianize (hackage "haskell-lexer" `flag` P.DebVersion "1.0-3build2")
    , debianize (hackage "hinotify")
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "wheezy" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    -- Not used, and not building.
    -- , debianize (hackage "hoauth")
    , debianize (hackage "hostname" `pflag` P.DebVersion "1.0-4build1" `qflag` P.DebVersion "1.0-4build3" `sflag` P.DebVersion "1.0-1~hackage1")
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize (hackage "HPDF")
    , debianize (hackage "hs-bibutils")
    , debianize (hackage "hsemail") -- (rel release [] [P.DebVersion "1.7.1-2build2"])
    , P.Package { P.name = "haskell-hsopenssl"
                , P.spec = Debianize (Patch (Hackage "HsOpenSSL") $(embedFile "patches/HsOpenSSL.diff"))
                , P.flags = [ P.DevelDep "libssl-dev", P.DevelDep "libcrypto++-dev" ] }
    , debianize (hackage "HsSyck" `pflag` P.DebVersion "0.50-2" `qflag` P.DebVersion "0.50-2build2")
    , debianize (hackage "HStringTemplate")
    , darcs "haskell-html-entities" (repo </> "html-entities")
    , debianize (hackage "http-types")
    , debianize (hackage "i18n" `flag` P.DebVersion "0.3-1~hackage1")
    , debianize (hackage "iconv")
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs (repo </> "haskell-incremental-sat-solver-debian"))
                , P.flags = [] }
    , debianize (hackage "instant-generics")
    , debianize (hackage "generic-deriving")
    , debianize (hackage "irc")
    , debianize (hackage "ixset")
    , darcs "haskell-json" (repo ++ "/haskell-json")
    , debianize (hackage "language-css" `flag` P.DebVersion "0.0.4.1-1~hackage1")
    , debianize (hackage "largeword")
    -- No cabal file
    -- , debianize (git "haskell-logic-hs" "https://github.com/smichal/hs-logic")
{-  , apt "wheezy" "haskell-leksah"
    , apt "wheezy" "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , P.Package { P.name = "haskell-pointed"
                , P.spec = Debianize (Hackage "pointed")
                , P.flags = [] }
    , P.Package { P.name = "haskell-logic-tptp"
                , P.spec = Debianize (Patch (Hackage "logic-TPTP") $(embedFile "patches/logic-TPTP.diff"))
                , P.flags = [ P.BuildDep "alex", P.BuildDep "happy" ] }
    , apt "sid" "haskell-maybet"
    , darcs "haskell-mime" (repo </> "haskell-mime")
    , debianize (hackage "mmap")
    , debianize (hackage "monad-control")
    , debianize (hackage "monad-par-extras" `flag` P.DebVersion "0.3.2-1~hackage1")
    , debianize (hackage "abstract-deque")
    , debianize (hackage "abstract-par")
    , debianize (hackage "monad-par")
    , debianize (hackage "IORefCAS")
    , debianize (hackage "bits-atomic")
    , debianize (hackage "monadLib")
    -- Putting this in our repo can cause problems, because when it is
    -- installed some packages can't compile unless you add package
    -- qualifiers to their imports.  For this reason, when we run the
    -- autobuilder with the --lax flag we usually get a failure from
    -- some package that builds after monads-tf got installed.  On the
    -- other hand, without monads-tf we lose this dependency chain:
    -- monads-tf -> options -> fay.
    -- , debianize (hackage "monads-tf")
    , apt (rel release "wheezy" "quantal") "haskell-monoid-transformer"
    , debianize (hackage "murmur-hash")
    , debianize (hackage "mwc-random")
    , P.Package { P.name = "haskell-nano-hmac"
                , P.spec = Debianize (Patch (Hackage "nano-hmac") $(embedFile "patches/nano-hmac.diff"))
                , P.flags = [P.DebVersion "0.2.0ubuntu1"] }
    , P.Package { P.name = "haskell-openid"
                , P.spec = Debianize (Patch (Hackage "openid") $(embedFile "patches/openid-ghc76.diff"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-operational"
                , P.spec = Debianize (Hackage "operational")
                , P.flags = [P.OmitLTDeps] }
--    , debianize (hackage "options")
    , debianize (hackage "optparse-applicative") -- [P.CabalPin "0.4.3"] -- fay requires optparse-applicative < 0.5.
    , debianize (hackage "ordered")
    , debianize (hackage "multiset" `ghc74flag` P.CabalPin "0.2.1") -- 0.2.2 requires containers >= 0.5, which comes with ghc 7.6.
    , debianize (hackage "temporary")
    , debianize (hackage "pandoc-types")
    , debianize (hackage "parse-dimacs")
    , debianize (hackage "parseargs")
    , apt (rel release "wheezy" "quantal") "haskell-parsec2" `patch` $(embedFile "patches/parsec2.diff")
    , debianize (hackage "PBKDF2")
    , apt (rel release "wheezy" "quantal") "haskell-pcre-light"
    , debianize (hackage "permutation" `flag` P.DebVersion "0.4.1-1~hackage1")
    , debianize (hackage "pipes")
    , debianize (hackage "polyparse")
    , debianize (hackage "primitive")
    , debianize (hackage "PropLogic")
    , debianize (hackage "PSQueue"
                   `pflag` P.DebVersion "1.1-2"
                   `qflag` P.DebVersion "1.1-2build2"
                   `sflag` P.DebVersion "1.1-1")
    , debianize (hackage "pwstore-purehaskell"
                   -- `patch` $(embedFile "patches/pwstore-purehaskell.diff")
                   -- `flag` P.DebVersion "2.1-1~hackage1"
                )
    , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
    , debianize (hackage "regex-tdfa" `pflag` P.DebVersion "1.1.8-1" `qflag` P.DebVersion "1.1.8-2build2")
    , darcs "haskell-revision" (repo </> "haskell-revision")
    , debianize (hackage "RJson" `patch` $(embedFile "patches/RJson.diff"))
    , debianize (hackage "safe" `flag` P.DebVersion "0.3.3-2")
    , debianize (hackage "safecopy")
    , debianize (hackage "sat"
                   `patch` $(embedFile "patches/sat.diff")
                   `flag` P.DebVersion "1.1.1-1~hackage1")
    , debianize (hackage "semigroups")
    , debianize (hackage "nats")
    , debianize (hackage "sendfile")
    , darcs "haskell-set-extra" (repo </> "set-extra")
    , debianize (darcs "haskell-old-exception" (repo ++ "/old-exception"))
    , apt (rel release "wheezy" "quantal") "haskell-sha"
    , debianize (hackage "shake")
    , debianize (hackage "shakespeare")
    , debianize (hackage "shakespeare-css")
    , debianize (hackage "system-fileio")
    , debianize (hackage "simple-css"
                   `patch` $(embedFile "patches/simple-css.diff")
                   `flag` P.DebVersion "0.0.4-1~hackage1")
    , debianize (hackage "SMTPClient")
    , debianize (hackage "socks")
    , debianize (hackage "split")
    -- Version 1.14, which is in darcs, is too new for the current haskell-src-meta and haskell-derive
    , debianize (-- darcs "haskell-haskell-src-exts" "http://code.haskell.org/haskell-src-exts"
                 hackage "haskell-src-exts"
                   `patch` $(embedFile "patches/haskell-src-exts.diff")
                   `flag` P.BuildDep "happy")
    , debianize (hackage "stb-image")
    , apt (rel release "wheezy" "quantal") "haskell-strict" -- for leksah
    , P.Package { P.name = "haskell-strict-concurrency"
                , P.spec = Debianize (Patch (Hackage "strict-concurrency") $(embedFile "patches/strict-concurrency.diff"))
                , P.flags = rel release [P.DebVersion "0.2.4.1-3"] [P.DebVersion "0.2.4.1-2build3"] }
    , debianize (hackage "strict-io") -- for GenI
    , debianize (hackage "smallcheck")
    , debianize (hackage "syb-with-class")
    , apt (rel release "wheezy" "quantal") "haskell-syb-with-class-instances-text"
    , debianize (hackage "tagged" `flag` P.CabalPin "0.4.5") -- Pinned to avoid a big rebuild
    , debianize (hackage "tagsoup")
    , debianize (hackage "tar")
    , debianize (hackage "terminfo"
                             `flag` P.DevelDep "libncurses5-dev"
                             `flag` P.DevelDep "libncursesw5-dev")
    , debianize (hackage "test-framework")
    , debianize (hackage "test-framework-hunit")
    , debianize (hackage "test-framework-quickcheck")
    , debianize (hackage "test-framework-quickcheck2" `patch` $(embedFile "patches/test-framework-quickcheck2.diff"))
    , debianize (hackage "test-framework-th")
    --
    -- , debianize (hackage "testpack" `patch` $(embedFile "patches/testpack.diff"))
    , debianize (hackage "th-expand-syns")
    , debianize (hackage "th-lift")
    , debianize (hackage "transformers-base" `pflag` P.DebVersion "0.4.1-2" `qflag` P.DebVersion "0.4.1-2build2")
    , debianize (hackage "unicode-names" `flag` P.DebVersion "3.2.0.0-1~hackage1")
    , debianize (hackage "unicode-properties"
                   `patch` $(embedFile "patches/unicode-properties.diff")
                   `flag` P.DebVersion "3.2.0.0-1~hackage1")
    , debianize (hackage "uniplate")
    , debianize (hackage "cmdargs")
    , P.Package { P.name = "haskell-language-javascript"
                , P.spec = Debianize (Hackage "language-javascript")
                , P.flags = [P.BuildDep "happy"] }
    , debianize (hackage "utf8-light" `pflag` P.DebVersion "0.4.0.1-2build1" `qflag` P.DebVersion "0.4.0.1-2build3")
    , debianize (hackage "language-haskell-extract")
    , P.Package { P.name = "haskell-pretty-show", P.spec = (Debianize (Hackage "pretty-show")), P.flags = [] }
    , P.Package { P.name = "haskell-language-ecmascript"
                , P.spec = Debianize (Hackage "language-ecmascript")
                , P.flags = [] }
    , debianize (hackage "charset")
    , debianize (hackage "Elm")
    , debianize (hackage "elm-server" `patch` $(embedFile "patches/elm-server.diff"))
    , debianize (hackage "gdiff")
    , debianize (hackage "hjsmin")
    , debianize (hackage "unix-compat")
    , debianize (hackage "Unixutils-shadow")
    , debianize (hackage "unordered-containers")
    , debianize (hackage "utf8-prelude" `flag` P.DebVersion "0.1.6-1~hackage1")
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt (rel release "wheezy" "quantal") "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize (hackage "unification-fd")
    , debianize (hackage "newtype")
    , debianize (hackage "universe" `patch` $(embedFile "patches/universe.diff"))
    , P.Package { P.name = "haskell-logict"
                , P.spec = Debianize (Hackage "logict")
                , P.flags = [] }
    , debianize (hackage "utility-ht")
    , debianize (hackage "vacuum")
    , debianize (hackage "vector")
    , debianize (hackage "vector-algorithms")
    , P.Package { P.name = "haskell-virthualenv"
                , P.spec = Debianize (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff"))
                , P.flags =  [] }
    , debianize (hackage "vault")
    , P.Package { P.name = "haskell-wai"
                , P.spec = Debianize (Hackage "wai")
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-encodings"
                , P.spec = Debianize (Patch (Hackage "web-encodings") $(embedFile "patches/web-encodings.diff"))
                , P.flags = [] }
    , debianize (hackage "boomerang")
    , apt (rel release "wheezy" "quantal") "haskell-xml"
    , debianize (hackage "cookie")
    , debianize (hackage "lifted-base")
    , debianize (hackage "system-filepath")
    , P.Package { P.name = "haskell-xml-enumerator"
                , P.spec = Debianize (Patch (Hackage "xml-enumerator") $(embedFile "patches/xml-enumerator.diff"))
                , P.flags = [] }
    , debianize (hackage "xml-types")
    , debianize (hackage "xss-sanitize" `qflag` P.DebVersion "0.3.2-1build1")
    , debianize (hackage "yaml-light" `pflag` P.DebVersion "0.1.4-2" `qflag` P.DebVersion "0.1.4-2build1")
    , debianize (hackage "zip-archive")
    , debianize (hackage "regex-pcre-builtin"
                   -- Need to email Audrey Tang <audreyt@audreyt.org> about this.
                   `patch` $(embedFile "patches/regex-pcre-builtin.diff")
                   `flag` P.DevelDep "libpcre3-dev")
    , debianize (hackage "hscolour") `flag` P.RelaxDep "hscolour"
    , debianize (hackage "hslogger")
    , debianize (hackage "extensible-exceptions") -- required for ghc-7.6, not just quantal
    , case release of
      "quantal-seereason" -> P.NoPackage -- This build hangs when performing tests
      _ -> apt "sid" "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery")
                , P.flags = [] }
    , P.Package { P.name = "jquery-goodies"
                , P.spec = Proc (Apt "sid" "jquery-goodies")
                , P.flags = [] }
-- We want to stick with 1.8 for now.
{-  , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "wheezy" "jqueryui")
                , P.flags = [] } -}
    , P.Package { P.name = "jcrop"
                , P.spec = DebDir (Uri (repo </> "jcrop/Jcrop.tar.gz") "028feeb9b6415af3b7fd7d9471c92469") (Darcs (repo ++ "/jcrop-debian"))
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "wheezy" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-missingh"
                , P.spec = Debianize (Hackage "MissingH")
                , P.flags = [P.Revision ""] }
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs (repo </> "seereason-keyring")
                , P.flags = [P.UDeb "seereason-keyring-udeb"] }
    , apt "wheezy" "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs (repo </> "vc-darcs")
                , P.flags = [] }
    , debianize (hackage "HaTeX")
    , debianize (hackage "matrix")
    , debianize (hackage "hlatex")
    , debianize (hackage "latex")
    , debianize (hackage "texmath")
    , debianize (hackage "derive")
    , debianize (hackage "frquotes")
    , P.Package { P.name = "foo2zjs"
                , P.spec = Apt "quantal" "foo2zjs"
                , P.flags = [] }
{-  -- Has no Setup.hs file, not sure how to build this.
    , P.Package { P.name = "hackage"
                , P.spec = Debianize (Darcs "http://code.haskell.org/hackage-server")
                , P.flags = [] } -}
    , debianize (hackage "stringsearch")
    , debianize (hackage "rss" `patch` $(embedFile "patches/rss.diff"))
    , debianize (hackage "async")
    , P.Package { P.name = "csv"
                , P.spec = Debianize (Hackage "csv")
                , P.flags = (rel release [P.DebVersion "0.1.2-2"] [P.DebVersion "0.1.2-2build2"]) }
{-
    -- Needs a build dependency on libXrandr-dev and the cabal package x11.
    , P.Package { P.name = "xmobar"
                , P.spec = Debianize (Hackage "xmobar")
                , P.flags = [] }
-}
    -- Needs update for current http-conduit
    -- , debianize $ (hackage "dropbox-sdk") `patch` $(embedFile "patches/dropbox-sdk.diff")
    , debianize (darcs "haskell-hs3" (repo ++ "/hS3"))
                `flag` P.DebVersion "0.5.6-2"
                `flag` P.ModifyAtoms (doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
    , debianize (hackage "urlencoded")
    , debianize (hackage "resourcet")
    , debianize (hackage "hxt")
    , debianize (hackage "hxt-charproperties") `flag` P.DebVersion "9.1.1-2build1"
    , debianize (hackage "hxt-regex-xmlschema")
    , debianize (hackage "hxt-unicode") `flag` P.DebVersion "9.0.2-2"
    ]

relax p x = p {P.flags = P.flags p ++ [P.RelaxDep x]}

compiler release =
    case release of
      -- "precise-seereason" -> P.Packages (singleton "ghc") [devscripts]
      "squeeze-seereason" -> P.NoPackage -- Omitted to avoid a big rebuild
      _ -> P.Packages (singleton "ghc") [ghc76, devscripts]
    where
      ghc76 = apt "sid" "ghc" -- As of 8 Jun 2013 this contains 7.6.3-3
                `rename` "ghc"
                `relax` "ghc"
                `relax` "happy"
                `relax` "alex"
                `relax` "xsltproc"
                `relax` "debhelper"
                `relax` "quilt"
                `relax` "python-minimal"
                `relax` "libgmp-dev"
                `squeezeRelax` "libgmp3-dev"
                `squeezePatch` $(embedFile "patches/ghc.diff") <>
              case release of
                "squeeze-seereason" -> apt "wheezy" "po4a" <>
                                       apt "wheezy" "debhelper" `patch` $(embedFile "patches/debhelper.diff") <>
                                       apt "wheezy" "dpkg" `patch` $(embedFile "patches/dpkg.diff") <>
                                       apt "wheezy" "makedev"
                _ -> P.NoPackage

      devscripts =
        P.Package { P.name = "haskell-devscripts"
                  , P.spec = Apt "sid" "haskell-devscripts"
                  , P.flags = [P.RelaxDep "python-minimal"] }
{-      P.Package { P.name = "haskell-devscripts"
                  , P.spec = Patch (Apt "experimental" "haskell-devscripts") $(embedFile "patches/haskell-devscripts-0.8.13.diff")
                  , P.flags = [P.RelaxDep "python-minimal"] } -}
      -- haskell-devscripts-0.8.13 is for ghc-7.6 only
      squeezeRelax = case release of "squeeze-seereason" -> relax; "squeeze-seereason-private" -> relax; _ -> \ p _ -> p
      squeezePatch = case release of "squeeze-seereason" -> patch; "squeeze-seereason-private" -> patch; _ -> \ p _ -> p

platform release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p
        sflag = case release of "squeeze-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "platform") $
    [ -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Hackage "happy") (Darcs (repo </> "happy-debian")),
                  P.flags = [P.RelaxDep "happy", P.CabalDebian ["--executable", "happy"],
                             P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize (hackage "stm")
    , debianize (hackage "zlib" `flag` P.DevelDep "zlib1g-dev")
    , debianize (hackage "mtl")
    , debianize (hackage "transformers" `qflag` P.DebVersion "0.3.0.0-1build3")
    , debianize (hackage "parallel")
    , debianize (hackage "syb")
    , debianize (hackage "fgl" `pflag` P.DebVersion "5.4.2.4-2" `qflag` P.DebVersion "5.4.2.4-2build2" `sflag` P.DebVersion "5.4.2.4-1")
    , debianize (hackage "text" `flag` P.CabalPin "0.11.3.0")
    , P.Package { P.name = "alex"
                , P.spec = Debianize (Hackage "alex")
                  -- alex shouldn't rebuild just because alex seems newer, but alex does require
                  -- an installed alex binary to build
                , P.flags = [P.RelaxDep "alex",
                             P.BuildDep "alex",
                             P.CabalDebian ["--executable", "alex"],
                             P.ModifyAtoms (\ atoms -> setL compat (Just 9) $
                                                       foldr (\ name atoms -> modL installData (insertWith union (BinPkgName "alex") (singleton (name, name))) atoms)
                                                           atoms
                                                           [ "AlexTemplate"
                                                           , "AlexTemplate-debug"
                                                           , "AlexTemplate-ghc"
                                                           , "AlexTemplate-ghc-debug"
                                                           , "AlexTemplate-ghc-nopred"
                                                           , "AlexWrapper-basic"
                                                           , "AlexWrapper-basic-bytestring"
                                                           , "AlexWrapper-gscan"
                                                           , "AlexWrapper-monad"
                                                           , "AlexWrapper-monad-bytestring"
                                                           , "AlexWrapper-monadUserState"
                                                           , "AlexWrapper-monadUserState-bytestring"
                                                           , "AlexWrapper-posn"
                                                           , "AlexWrapper-posn-bytestring"
                                                           , "AlexWrapper-strict-bytestring"]) ] }
    , opengl release
    -- , haddock release
    , debianize (hackage "haskell-src"
                             `flag` P.BuildDep "happy"
                             `pflag` P.DebVersion "1.0.1.5-1"
                             `qflag` P.DebVersion "1.0.1.5-1build2")
    -- Versions 2.4.1.1 and 2.4.1.2 change unEscapeString in a way
    -- that breaks our apps: https://github.com/haskell/network/issues/86
    , debianize (hackage "network" `flag` P.CabalPin "2.4.1.0")
    , debianize (hackage "publicsuffixlist")
    , debianize (hackage "HTTP" `pflag` P.DebVersion "1:4000.2.3-1~hackage1" `qflag` P.DebVersion "1:4000.2.3-1build2")
    , debianize (hackage "cgi")
    -- This is bundled with the compiler
    -- , debianize (hackage "process")
    , debianize (hackage "random" `pflag` P.DebVersion "1.0.1.1-1" `qflag` P.DebVersion "1.0.1.1-1build2")
    , debianize (hackage "HUnit")
    , debianize (hackage "QuickCheck" `flag` P.BuildDep "libghc-random-prof")
    , debianize (hackage "parsec" `flag` P.CabalDebian ["--conflicts", "libghc-parsec3-dev:libghc-parsec2-dev,libghc-parsec3-prof:libghc-parsec2-prof,libghc-parsec3-doc:libghc-parsec2-doc",
                                                        "--provides", "libghc-parsec3-dev:libghc-parsec2-dev,libghc-parsec3-prof:libghc-parsec2-prof,libghc-parsec3-doc:libghc-parsec2-doc",
                                                        "--replaces", "libghc-parsec3-dev:libghc-parsec2-dev,libghc-parsec3-prof:libghc-parsec2-prof,libghc-parsec3-doc:libghc-parsec2-doc"])
    , apt (rel release "wheezy" "quantal") "haskell-html"
    , apt (rel release "wheezy" "quantal") "haskell-regex-compat"
    , apt (rel release "wheezy" "quantal") "haskell-regex-base"
    , debianize (hackage "regex-posix")
    , debianize (hackage "xhtml" `qflag` P.DebVersion "3000.2.1-1build2")
    ]

clckwrks _home release =
    let useDevRepo = True
        repo = if useDevRepo
               then "http://hub.darcs.net/stepcut/clckwrks-dev"
               else "http://hub.darcs.net/stepcut/clckwrks"
               -- localRepo _home ++ "/clckwrks"
    in
    P.Packages (singleton "clckwrks") $
        [ happstack release
        , authenticate _home release
        , happstackdotcom _home
        , plugins
        , P.Package { P.name = "haskell-clckwrks"
                    , P.spec = Debianize (Patch
                                          (DataFiles
                                           (DataFiles
                                            (Cd "clckwrks" (Darcs repo))
                                            (Uri "http://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                 "e211065e573ea0239d6449882c9d860d")
                                            "jstree")
                                           (Uri "https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
                                                "2ae07a68fc44f0ef8d92cce25620bd5f")
                                           "json2")
                                          $(embedFile "patches/clckwrks.diff"))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-cli"
                    , P.spec = Debianize (Patch (Cd "clckwrks-cli" (Darcs repo)) $(embedFile "patches/clckwrks-cli.diff"))
                    , P.flags = [] }
        , P.Package { P.name = "haskell-clckwrks-plugin-bugs"
                    , P.spec = Debianize (Cd "clckwrks-plugin-bugs" (Darcs repo))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-plugin-media"
                    , P.spec = Debianize (Cd "clckwrks-plugin-media" (Darcs repo))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-plugin-ircbot"
                    , P.spec = Debianize (Cd "clckwrks-plugin-ircbot" (Darcs repo))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-theme-bootstrap"
                    , P.spec = Debianize (Cd "clckwrks-theme-bootstrap" (Darcs repo))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , P.Package { P.name = "clckwrks-dot-com"
                    , P.spec = Debianize (Patch (Cd "clckwrks-dot-com" (Darcs repo)) $(embedFile "patches/clckwrks-dot-com.diff"))
                    , P.flags = [] }
        , P.Package { P.name = "clckwrks-theme-clckwrks"
                    , P.spec = Debianize (Cd "clckwrks-theme-clckwrks" (Darcs repo))
                    , P.flags = [P.BuildDep "haskell-hsx-utils"] }
        , debianize (hackage "jmacro")
        , debianize (hackage "hsx-jmacro")
        , debianize (hackage "monadlist")
        ] ++
    if useDevRepo
    then [ P.Package { P.name = "haskell-clckwrks-plugin-page"
                     , P.spec = Debianize (Patch (Cd "clckwrks-plugin-page" (Darcs repo)) $(embedFile "patches/clckwrks-plugin-page.diff"))
                     , P.flags = [P.BuildDep "haskell-hsx-utils"] } ]
    else []

happstack release =
    let privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String in
    P.Packages (singleton "happstack")
    [ plugins
    -- , debianize (darcs "haskell-debian-packaging" (repo </> "debian-packaging"))
    , darcs "haskell-seereason-base" (repo ++ "/seereason-base")
    , debianize (hackage "happstack")
    , debianize (hackage "happstack-fay"
                   `patch` $(embedFile "patches/happstack-fay.diff")
                )
    , debianize (hackage "cryptohash-cryptoapi")
    , debianize (hackage "happstack-fay-ajax")
    , debianize (hackage "hsx2hs" `flag` P.CabalDebian ["--executable", "hsx2hs"])
    , debianize (hackage "fay-hsx")
    , debianize (hackage "fay")
    , debianize (hackage "fay-base")
    , debianize (git "haskell-fay-jquery" "https://github.com/faylang/fay-jquery")
    , debianize (darcs "mastermind" "http://hub.darcs.net/stepcut/mastermind"
                   `patch` $(embedFile "patches/mastermind.diff")
                   `flag` P.CabalDebian ["--build-dep=hsx2hs",
                                         "--build-dep=haskell-fay-utils",
                                         "--build-dep=haskell-fay-base-utils",
                                         "--build-dep=haskell-fay-hsx-utils",
                                         "--build-dep=haskell-fay-jquery-utils",
                                         "--build-dep=haskell-happstack-fay-ajax-utils"])
    , darcs "haskell-happstack-extra" (repo ++ "/happstack-extra")
{-  , debianize (git "haskell-haskell-names" "https://github.com/haskell-suite/haskell-names")
    , debianize (git "haskell-haskell-packages" "https://github.com/haskell-suite/haskell-packages")
    , debianize (git "haskell-hse-cpp" "https://github.com/haskell-suite/hse-cpp")
      -- These will break everything
    , debianize (git "haskell-haskell-src-exts" "https://github.com/haskell-suite/haskell-src-exts")
    , debianize (git "haskell-cabal" "https://github.com/haskell/cabal" `cd` "Cabal") -}
    -- , debianize (hackage "cabal-install" `patch` $(embedFile "patches/cabal-install.diff"))
    , debianize (hackage "EitherT")
    , debianize (hackage "type-eq")
    , debianize (hackage "traverse-with-class")
    , debianize (hackage "happstack-hsp"
                   -- `patch` $(embedFile "patches/happstack-hsp.diff")
                   `flag` P.BuildDep "haskell-hsx-utils")
    , debianize (hackage "happstack-jmacro")
    , debianize (hackage "jmacro-rpc-happstack")
    , debianize (hackage "jmacro-rpc")
    , darcs "haskell-happstack-search" (repo ++ "/happstack-search")
    , debianize (hackage "happstack-server")
    , debianize (hackage "happstack-lite")
    , debianize (hackage "happstack-server-tls")
    , debianize (hackage "time-compat")
    , debianize (hackage "base64-bytestring")
    , debianize (hackage "threads")
    , debianize (hackage "list-tries")
    , debianize (hackage "happstack-static-routing" `flag` P.DebVersion "0.3.1-1~hackage1")
    , debianize (hackage "happstack-util"
                   `patch` $(embedFile "patches/happstack-util-ghc76.diff")
                   `flag` P.DebVersion "6.0.3-1")
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize (hackage "hsp"
                   `patch` $(embedFile "patches/hsp.diff")
                   `flag` P.BuildDep "haskell-hsx-utils"
                   `flag` CabalPin "0.7.1"
                   `mflag` (rel release (Just (P.DebVersion "0.7.1-1~hackage1")) Nothing))
    , debianize (hackage "hsx"
                   `flag` P.DebVersion "0.10.4-1~hackage1"
                   -- Putting trhsx into a package called trhsx is problematic,
                   -- because the official debian package puts it into libghc-hsx-utils.
                   -- `flag` P.CabalDebian ["--executable", "trhsx"]
                )
    , debianize (hackage "pandoc"
                   `patch` $(embedFile "patches/pandoc.diff")
                   `flag` P.RelaxDep "libghc-pandoc-doc")
    , debianize (hackage "markdown" `rename` "markdown")
    , debianize (hackage "highlighting-kate")
    , debianize (hackage "web-routes")
    , debianize (hackage "web-routes-boomerang" `flag` P.DebVersion "0.27.0-1~hackage1")
    , debianize (hackage "web-routes-happstack")
    , debianize (hackage "web-routes-hsp" `flag` P.DebVersion "0.23.0-1~hackage1")
    , debianize (hackage "web-routes-mtl" `flag` P.DebVersion "0.20.1-1~hackage1")
    , debianize (hackage "web-routes-th" `flag` P.DebVersion "0.22.1-1~hackage1")
    , darcs "haskell-formlets-hsp" (repo ++ "/formlets-hsp")
    , darcs "haskell-happstack-scaffolding" (repo ++ "/happstack-scaffolding") -- Don't use Debianize here, it restores the doc package which crashes the build
    , debianize (hackage "HJScript")
    , debianize (darcs "reform" (darcsHub ++ "/reform")
                   `cd` "reform"
                   `flag` P.DebVersion "0.1.2-1~hackage1")
    , debianize (darcs "reform-blaze" (darcsHub ++ "/reform") `cd` "reform-blaze")
    , debianize (darcs "reform-happstack" (darcsHub ++ "/reform") `cd` "reform-happstack")
    -- , debianize (darcs "reform-heist" (darcsHub ++ "/reform") `cd` "reform-heist")
    , debianize (darcs "reform-hsp" (darcsHub ++ "/reform") `cd` "reform-hsp")
    , debianize (hackage "blaze-markup")
    , apt (rel release "wheezy" "quantal") "haskell-blaze-builder"
    , debianize (hackage "blaze-builder-enumerator" `flag` P.DebVersion "0.2.0.5-1~hackage1")
    , debianize (hackage "blaze-from-html")
    , debianize (hackage "blaze-html")
    , debianize (hackage "blaze-textual")
    , debianize (hackage "blaze-textual-native"
                   `patch` $(embedFile "patches/blaze-textual-native.diff")
                   `flag` P.Revision "")
    , debianize (darcs "clckwrks-theme-happstack" (repo ++ "/happstack-clckwrks")
                   `cd` "clckwrks-theme-happstack"
                   -- `patch` $(embedFile "patches/clckwrks-theme-happstack.diff")
                   `flag` P.BuildDep "haskell-hsx-utils")
    , debianize (darcs "happstack-dot-com" (repo ++ "/happstack-clckwrks")
                   `cd` "happstack-dot-com"
                   `patch` $(embedFile "patches/happstack-dot-com.diff"))
    , debianize (hackage "acid-state")
    ]

-- | Packages pinned pending update of happstack-authenticate (in one possible build order.)
authenticate _home release =
  P.Packages (singleton "authenticate") $
    [ conduit
    , debianize (hackage "pureMD5")
    , debianize (hackage "monadcryptorandom")
    , debianize (hackage "RSA")
    , debianize (hackage "resourcet")
    , debianize (hackage "mmorph")
    , debianize (hackage "void")
    -- Version 1.3.1 may be too new for tls 0.9.11
    , debianize (hackage "certificate"
                   -- `patch` $(embedFile "patches/certificate.diff")
                )
    , debianize (hackage "pem")
    , debianize (hackage "zlib-bindings")
    , debianize (hackage "tls")
    , debianize (hackage "tls-extra")
    , debianize (hackage "cipher-rc4")
    , debianize (hackage "crypto-pubkey")
    , debianize (hackage "crypto-numbers")
    , debianize (hackage "authenticate")
    , debianize (hackage "zlib-enum" `flag` P.DebVersion "0.2.3-1~hackage1")
    , debianize (darcs "haskell-happstack-authenticate" (darcsHub ++ "/happstack-authenticate"))
    , digestiveFunctors
    , debianize (hackage "fb")
    , debianize (hackage "monad-logger")
    , debianize (hackage "fast-logger")
    , debianize (hackage "date-cache")
    , debianize (hackage "unix-time")
    ]

digestiveFunctors =
    P.Packages (singleton "digestive-functors")
    [ debianize (hackage "digestive-functors" `flag` P.CabalPin "0.2.1.0")  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
    -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
    , debianize (hackage "digestive-functors-happstack"
                   `patch` $(embedFile "patches/digestive-functors-happstack.diff")
                   `flag` P.CabalPin "0.1.1.5"
                   `flag` P.DebVersion "0.1.1.5-1~hackage1")
    , debianize (darcs "haskell-digestive-functors-hsp" (repo ++ "/digestive-functors-hsp")
                   `flag` P.DebVersion "0.5.0-1~hackage1") ]

-- | We need new releases of all the conduit packages before we can move
-- from conduit 0.4.2 to 0.5.
conduit =
  P.Packages (singleton "conduit")
    [ debianize (hackage "conduit")
    , debianize (hackage "attoparsec-conduit")
    , debianize (hackage "blaze-builder-conduit")
    , debianize (hackage "http-conduit")
    , debianize (hackage "zlib-conduit")
    , debianize (hackage "xml-conduit")
    , debianize (hackage "mime-types")
    ]

-- ircbot needs a dependency on containers
happstackdotcom _home =
    P.Packages (singleton "happstackdotcom") $
    [ debianize (hackage "ircbot")
    , darcs "haskell-happstackdotcom-doc" (repo </> "happstackDotCom-doc") ]

-- May work with these added dependencies (statevar thru openglraw)
opengl release = P.Packages (singleton "opengl") $
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    [ debianize (hackage "OpenGL"
                   `flag` P.DevelDep "libglu1-mesa-dev")
{-  , P.Package { P.name = "haskell-vacuum-opengl"
                , P.spec =  Debianize (Patch (Hackage "vacuum-opengl"))
                                 (unlines
                                  [ "--- old/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:26:14.000000000 -0700"
                                  , "+++ new/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:32:17.027953252 -0700"
                                  , "@@ -34,7 +34,7 @@"
                                  , " "
                                  , " import Network"
                                  , " "
                                  , "-import Foreign"
                                  , "+import Foreign (shiftR)"
                                  , " import Foreign.C"
                                  , " "
                                  , " --------------------------------------------------------------------------------" ])
                , P.flags = [ P.DebVersion "0.0.3-1~hackage2" ] } -}
    , debianize (hackage "bitmap-opengl"
                   `flag` P.DevelDep "libglu1-mesa-dev")
    , debianize (hackage "GLUT"
                   `flag` P.DevelDep "freeglut3-dev")
    , debianize (hackage "StateVar" `pflag` P.DebVersion "1.0.0.0-2build1" `qflag` P.DebVersion "1.0.0.0-2build3")
    , debianize (hackage "Tensor")
    , debianize (hackage "GLURaw")
    , debianize (hackage "ObjectName")
    , debianize (hackage "monad-task")
    , darcs "haskell-glfw" ("http://src.seereason.com/haskell-GLFW")
--    , debianize (hackage "GLFW")
    , debianize (hackage "GLFW-b")
    , debianize (hackage "GLFW-b-demo")
    , debianize (hackage "GLFW-task")
    , debianize (hackage "OpenGLRaw"
                   `flag` P.DevelDep "libgl1-mesa-dev")
    ]

--  Using pkg-config version 0.25 found on system at: /usr/bin/ 2>
--  <interactive>:2:1:
--      Failed to load interface for `Directory'
--      It is a member of the hidden package `haskell98-2.0.0.1'.
--      Use -v to see a list of the files searched for.
--
--  src/System/Plugins/Utils.hs:21:8:
--      Warning: In the use of `catch'
--               (imported from Prelude, but defined in System.IO.Error):
--               Deprecated: "Please use the new exceptions variant, Control.Exception.catch"
--
--  src/System/Plugins/Load.hs:91:35:
--      Module `GHC.Exts' does not export `addrToHValue#'
--  make: *** [build-ghc-stamp] Error 1
plugins = P.Packages (singleton "plugins") $
    [ debianize (hackage "plugins")
    , debianize (hackage "plugins-auto" `patch` $(embedFile "patches/plugins-auto.diff"))
    , debianize (hackage "happstack-plugins" `patch` $(embedFile "patches/happstack-plugins.diff"))
    , debianize (darcs "haskell-web-plugins" "http://hub.darcs.net/stepcut/web-plugins" `cd` "web-plugins")
    ]

algebra release =
    let qflag = case release of "quantal-seereason" -> flag; _ -> \ p _ -> p
        pflag = case release of "precise-seereason" -> flag; _ -> \ p _ -> p in
    P.Packages (singleton "algebra")
    [ debianize (hackage "data-lens")
    , debianize (hackage "data-lens-template")
    , debianize (hackage "adjunctions")
    , debianize (hackage "algebra")
    , debianize (hackage "bifunctors")
    , debianize (hackage "categories")
    , debianize (hackage "comonad")
    , debianize (hackage "comonads-fd")
    , debianize (hackage "comonad-transformers")
    , debianize (hackage "control-monad-free")
    , debianize (hackage "transformers-free")
    , debianize (hackage "contravariant"
                             `patch` $(embedFile "patches/contravariant.diff")
                             `qflag` P.DebVersion "0.2.0.2-1build2")
    , debianize (hackage "distributive"
                             `patch` $(embedFile "patches/distributive.diff")
                             `qflag` P.DebVersion "0.2.2-1build2")
    -- This package fails to build in several different ways because it has no modules.
    -- I am just going to patch the packages that use it to require transformers >= 0.3.
    -- Specifically, distributive and lens.
    -- , debianize (hackage "transformers-compat" {-`flag` P.NoDoc-})
    , debianize (hackage "profunctor-extras")
    , debianize (hackage "semigroupoid-extras")
    , debianize (hackage "groupoids")
    , debianize (hackage "profunctors")
    , debianize (hackage "reflection")
    , debianize (hackage "free")
    , debianize (hackage "keys")
    , debianize (hackage "intervals")
    , debianize (hackage "numeric-extras")
    , debianize (hackage "lens" `patch` $(embedFile "patches/lens.diff"))
    , debianize (hackage "lens-family-core")
    , debianize (hackage "lens-family")
    , debianize (hackage "lens-family-th" `patch` $(embedFile "patches/lens-family-th.diff"))
    , debianize (hackage "linear")
    , debianize (hackage "representable-functors")
    , debianize (hackage "representable-tries")
    , debianize (hackage "semigroupoids")
    , debianize (hackage "spine") ]
