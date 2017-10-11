{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.CommonTargets (commonTargets) where

import Control.Lens (use, view)
import Data.FileEmbed (embedFile)
import Data.Map as Map (elems)
import Data.Set as Set (fromList, member, Set)
import Debian.AutoBuilder.Details.Common (broken, broken2, ghcjs_also, ghcjs, git', gitrepo, gitrepo2, hack, noTests, noDoc, noProf, putSrcPkgName, Reason(Reason), replacement, skip, skip2, tflag, qflag, pflag, substitute, wflag, wskip, TSt)
import Debian.AutoBuilder.Types.Packages as P (PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           NoDoc, UDeb, OmitLTDeps, SkipVersion), packageMap,
                                               pid, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri, GroupName, inGroups, createPackage)
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage))

commonTargets :: Monad m => TSt m ()
commonTargets = do
  --------------------------------------------------
  -- INDIVIDUAL PACKAGES (alphabetized by symbol) --
  --------------------------------------------------
  _abstract_deque <-  hackage (Just "0.3") "abstract-deque" >>= flag (P.DebVersion "0.3-5") >>= debianize [] {->>= setDebVersion-}
  _abstract_par <- hackage (Just "0.3.3") "abstract-par" >>= flag (P.DebVersion "0.3.3-5") >>= debianize []
  _acid_state <- git "https://github.com/acid-state/acid-state" [{-Branch "log-inspection"-}] >>=
                 debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _adjunctions <- hackage (Just "4.3") "adjunctions" >>= debianize [] >>= ghcjs_also
  _aeson <- hackage (Just "0.11.2.0") "aeson" >>=
            patch $(embedFile "patches/aeson.diff") >>=
            flag (P.CabalPin "0.11.2.0") >>=
            -- flag (P.CabalDebian ["--missing-dependency", "libghc-fail-doc"]) >>=
            debianize [] >>=
            inGroups ["important"]
  _aeson_pretty <- hackage (Just "0.8.5") "aeson-pretty" >>= debianize []
  _aeson_qq <-  hackage (Just "0.8.2") "aeson-qq" >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _agi <- darcs ("https://github.com/ddssff/haskell-agi") >>= skip (Reason "No instance for (Applicative (AGIT m))")
  -- No Debian build trees found in /home/dsf/.autobuilder/hackage/allocated-processor-0.0.2
  -- _allocated_processor <- hackage Nothing "allocated-processor"
  _annotated_wl_pprint <- hack (Just "0.7.0") "annotated-wl-pprint"
  _ansi_terminal <- hackage (Just "0.6.2.3") "ansi-terminal" >>= flag (P.DebVersion "0.6.2.3-1") >>= debianize [] >>= ghcjs_also
  _ansi_wl_pprint <- hackage (Just "0.6.7.3") "ansi-wl-pprint" >>= flag (P.DebVersion "0.6.7.3-1") >>= debianize [] >>= ghcjs_also
  _appar <- hackage Nothing "appar" >>= flag (P.DebVersion "1.1.4-1") >>= debianize []
  _applicative_extras <- hackage (Just "0.1.8") "applicative-extras" >>= flag (P.DebVersion "0.1.8-1") >>= debianize [] >>= ghcjs_also
  _archive <- git "https://github.com/seereason/archive" []
             >>= flag (P.CabalDebian ["--default-package", "archive"])
             >>= inGroups ["autobuilder-group", "important"] >>= debianize []
  _asn1_data <- hackage (Just "0.7.2") "asn1-data" >>= debianize []
  _asn1_encoding <-  (hackage (Just "0.9.4") "asn1-encoding") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _asn1_parse <-  (hackage (Just "0.9.4") "asn1-parse") >>= flag (P.DebVersion "0.9.4-1build1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _asn1_types <-  (hackage (Just "0.3.2") "asn1-types") >>= flag (P.DebVersion "0.3.2-1") >>= debianize [] >>= ghcjs_also
  _async <-  (hackage (Just "2.1.0") "async") >>= debianize []
  _atomic_primops <-  (hackage (Just "0.8.0.4") "atomic-primops") >>= debianize []
  _atp_haskell <-  (git "https://github.com/seereason/atp-haskell" []) >>= debianize [] >>= inGroups ["seereason", "th-path", "important"] >>= ghcjs_also
  _attempt <-  (hackage (Just "0.4.0.1") "attempt") >>= debianize []
  _attoparsec <-  (hackage (Just "0.13.0.2") "attoparsec") >>= debianize [] >>= inGroups ["important"]
  _attoparsec_enumerator <- hackage (Just "0.3.4") "attoparsec-enumerator" >>= flag (P.DebVersion "0.3.4-3build1") >>= debianize []
               -- This was merged into attoparsec
               -- ,  (hackage "attoparsec-text" `patch` $(embedFile "patches/attoparsec-text.diff") >>= flag (P.Revision "")) >>= debianize []
               -- Deprecated
               -- ,  (hackage "attoparsec-text-enumerator") >>= debianize []
  _authenticate <-  (hackage (Just "1.3.3.2") "authenticate") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _autobuilder <-  (git "https://github.com/ddssff/autobuilder" []) >>= debianize []
                  >>= flag (P.CabalDebian [ "--source-package-name", "autobuilder" ])
                  >>= inGroups ["autobuilder-group", "important"]
  _autobuilder_seereason <-  (git "https://github.com/ddssff/autobuilder-seereason" []) >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _auto_update <-  (hackage (Just "0.1.4") "auto-update") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _base_compat <-  (hackage (Just "0.9.1") "base-compat") >>= debianize [] >>= ghcjs_also
  _base_orphans <-  (hackage (Just "0.5.4") "base-orphans") >>= debianize [] >>= ghcjs_also
  _base16_bytestring <-  (hackage (Just "0.1.1.6") "base16-bytestring") >>= flag (P.DebVersion "0.1.1.6-5") >>= debianize [] >>= ghcjs_also
  _base64_bytestring <- hackage (Just "1.0.0.1") "base64-bytestring" >>= flag (P.DebVersion "1.0.0.1-6") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _base_unicode_symbols <- hackage (Just "0.2.2.4") "base-unicode-symbols" >>= flag (P.DebVersion "0.2.2.4-7") >>= debianize []
  _bifunctors <-  (hackage (Just "5.4.1") "bifunctors") >>= debianize [] >>= ghcjs_also
  _bimap <-  (hackage (Just "0.3.2") "bimap") >>= debianize []
  -- _binary_tagged <- hackage Nothing "binary-tagged" >>= debianize []
  _bindings_dSL <-  (hackage (Just "1.0.23") "bindings-DSL") >>= flag (P.DebVersion "1.0.23-1") >>= debianize []
  _bindings_gLFW <-  (hackage (Just "3.1.2.1") "bindings-GLFW" >>= inGroups ["gl"]
                               -- `patch` $(embedFile "patches/bindings-GLFW.diff")
                               -- >>= flag (P.DevelDep "libxrandr2")
                               >>= flag (P.DevelDep "libx11-dev")
                               >>= flag (P.DevelDep "libgl1-mesa-dev")
                               >>= flag (P.DevelDep "libxi-dev")
                               >>= flag (P.DevelDep "libxxf86vm-dev")) >>= debianize []
  _bitmap <-  (hackage (Just "0.0.2") "bitmap") >>= debianize []
  _bitmap_opengl <-  (hackage (Just "0.0.1.5") "bitmap-opengl"
                     >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize [] >>= skip (Reason "Waiting for newer opengl")
  _bits_atomic <-  (hackage (Just "0.1.3") "bits-atomic") >>= debianize []
  _bitset <-  (hackage (Just "1.4.8") "bitset") >>= debianize [] >>= skip (Reason "Waiting for version compatible with base-4.8")
  _blaze_builder <-  (hackage (Just "0.4.0.2") "blaze-builder") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _blaze_from_html <-  (hackage (Just "0.3.2.1") "blaze-from-html") >>= debianize [] >>= inGroups ["happstack", "important"]
  _blaze_html <-  (hackage (Just "0.8.1.2") "blaze-html") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _blaze_markup <-  (hackage (Just "0.7.1.0") "blaze-markup") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _blaze_textual <-  (hackage (Just "0.2.1.0") "blaze-textual") >>= flag (P.DebVersion "0.2.1.0-3build1") >>= debianize [] >>= inGroups ["happstack", "important"]
  _blaze_textual_native <-  (hackage (Just "0.2.1.1") "blaze-textual-native"
                             >>= patch $(embedFile "patches/blaze-textual-native.diff")
                             >>= flag (P.Revision "")) >>= debianize [] >>= inGroups ["happstack", "important"]
  _boolean <-  (hackage (Just "0.2.3") "Boolean") >>= flag (P.DebVersion "0.2.3-4") >>= debianize []
  _boomerang <-  (hackage (Just "1.4.5.2") "boomerang") >>= debianize [] >>= ghcjs_also
  -- _bugzilla <- broken <$> apt "squeeze" "bugzilla" -- requires python-central (>= 0.5)
  _byteable <- hackage (Just "0.1.1") "byteable" >>= flag (P.DebVersion "0.1.1-5") >>= debianize [] >>= ghcjs_also
  _byteorder <- hackage (Just "1.0.4") "byteorder" >>= flag (P.DebVersion "1.0.4-5") >>= debianize []
  _bytes <-  (hackage (Just "0.15.0.1") "bytes") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-cereal-dev (<< 0.5)")
  -- bytestring-builder is now part of bytestring, but some packages (fast-logger) still depend on it (now patched)
  _bytestring_builder <- hackage (Just "0.10.8.1.0") "bytestring-builder" >>= flag P.NoDoc >>= debianize [] >>= ghcjs_also
  _bytestring_conversion <- hackage (Just "0.3.1") "bytestring-conversion" >>= debianize [] >>= inGroups ["servant"]
  _bytestring_nums <-  (hackage (Just "0.3.6") "bytestring-nums") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-bytestring-nums"
  _bytestring_trie <-  (hackage (Just "0.2.4.1") "bytestring-trie") >>= debianize []
  _bzlib <- hackage (Just "0.5.0.5") "bzlib" >>= flag (P.DebVersion "0.5.0.5-4") >>= flag (P.DevelDep "libbz2-dev") >>= debianize [] >>= ghcjs_also
               -- ,  (hackage "cairo-pdf") >>= debianize []
  _cabal_debian <- git "https://github.com/ddssff/cabal-debian" [] >>=
                   -- If we run cabal-debian it will find debian/Debianize.hs, and then
                   -- it will try to import src/Debian/Debianize/Optparse.hs, which has
                   -- #if directives that will fail.  Just use the debianization in git.
                   -- debianize ["--native", "--executable", "cabal-debian"] >>=
                   inGroups ["autobuilder-group", "important"]
  -- This is provided by ghc.  1.24 is for ghc-8.  Why am I even
  -- building this?  Maybe because packages are failing with 1.22.4.0?
  -- _cabal <- hackage (Just "1.24.0.0") "Cabal" >>= debianize []

  -- Build ghc debs for the version of Cabal shipped with our ghcjs.
  -- These debs need special names (e.g. libghc-cabal1228-dev) to
  -- avoid conflicts with the virtual package provided by ghc.
  _cabal_macosx <- hackage (Just "0.2.3.4") "cabal-macosx" {-`patch` $(embedFile "patches/cabal-macosx.diff")-} >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>= debianize []
  _c_ares <- apt "sid" "c-ares" >>= skip (Reason "Use standard")
  _cairo <- hackage (Just "0.13.1.1") "cairo" >>= flag (P.BuildDep "gtk2hs-buildtools") >>= debianize [] >>= skip (Reason "Setup.hs:5:8: Could not find module ‘Gtk2HsSetup’")
  _case_insensitive <-  (hackage (Just "1.2.0.7") "case-insensitive") >>= debianize [] >>= inGroups ["important"]
               -- Here is an example of creating a debian/Debianize.hs file with an
               -- autobuilder patch.  The autobuilder then automatically runs this
               -- script to create the debianization.
  _categories <- hackage (Just "1.0.6") "categories" >>= tflag (P.DebVersion "1.0.6-1") >>= debianize [] >>= broken
      -- comonad now includes comonad-transformers and comonads-fd
  _cautious_file <-  (hackage (Just "1.0.2") "cautious-file" >>= tflag (P.DebVersion "1.0.2-2")) >>= debianize [] >>= skip (Reason "requires filepath < 1.4")
  _cc_delcont <-  (hackage (Just "0.2") "CC-delcont" >>= flag (P.DebVersion "0.2-1~hackage1")) >>= debianize [] >>= skip (Reason "Missing applicative instances in 0.2")
               -- , apt (rel release "wheezy" "quantal") "haskell-cereal"
  _cereal <-  (hackage (Just "0.5.3.0") "cereal" {->>= flag (P.CabalPin "0.4.1.1")-}) >>= debianize [] >>= ghcjs_also -- Concerns about migration in 0.5
  _certificate <- hackage (Just "1.3.9") "certificate" >>= patch $(embedFile "patches/certificate.diff") >>= tflag (P.DebVersion "1.3.9-1build4") >>= debianize [] >>= skip (Reason "base < 4.8")
  _cgi <- (hackage (Just "3001.2.2.2") "cgi" {- `patch` $(embedFile "patches/cgi.diff") -}) >>= debianize [] >>= inGroups ["platform"] >>= skip (Reason "Depends on exceptions < 0.7")
  _charset <-  (hackage (Just "0.3.7.1") "charset") >>= flag (P.DebVersion "0.3.7.1-4build1") >>= debianize []
  _charsetdetect_ae <-  (hackage (Just "1.1.0.1") "charsetdetect-ae") >>= flag (P.DebVersion "1.1.0.1-1") >>= debianize []
  _cheapskate <- git "https://github.com/seereason/cheapskate" [] {-hackage (Just "0.1.0.3") "cheapskate"-} >>= debianize [] >>= skip (Reason "data default dependency")
  _chili <-  gitrepo2 "chili"  >>= debianize [] >>= ghcjs_also
  _cipher_aes128 <-  (hackage (Just "0.7.0.1") "cipher-aes128") >>= flag (P.DebVersion "0.7.0.1-2") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _cipher_aes <- hackage (Just "0.2.11") "cipher-aes" >>= flag (P.DebVersion "0.2.11-3build1") >>= debianize []
  _cipher_des <- hackage (Just "0.0.6") "cipher-des" >>= flag (P.DebVersion "0.0.6-5build1") >>= debianize []
  _cipher_rc4 <- hackage (Just "0.1.4") "cipher-rc4" >>= flag (P.DebVersion "0.1.4-5build1") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _citeproc_hs <- hackage (Just "0.3.10") "citeproc-hs" >>= debianize [] >>= skip (Reason "Non type-variable argument\nthe constraint: MonadState EvalState m\n(Use FlexibleContexts to permit this)")
  _clckwrks_cli <-  (gitrepo "clckwrks-cli") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_dot_com <- gitrepo "clckwrks-dot-com" >>=
                                 -- This is a change that only relates to the autobuilder
                                 patch $(embedFile "patches/clckwrks-dot-com.diff") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_bugs <-  (gitrepo2 "clckwrks-plugin-bugs"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_ircbot <-  (gitrepo "clckwrks-plugin-ircbot"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_media <-  (gitrepo "clckwrks-plugin-media"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_page <-  (gitrepo "clckwrks-plugin-page"
                             -- `patch` $(embedFile "patches/clckwrks-plugin-page.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks <-
    createPackage (Debianize'' (Patch (DataFiles (DataFiles
                                                  ({-Git "https://github.com/clckwrks/clckwrks" []-} Hackage "clckwrks")
                                                  (Uri "https://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                       "e211065e573ea0239d6449882c9d860d")
                                                  "jstree")
                                                 (Uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                                                      "c88c72230de1fa3c701187b8afba5e52" {-previouly: "5eecb009ae16dc54f261f31da01dbbac", then "a6d5fdbbcb076dd9385dd2135dbfb589"-})
                                                 "json2")
                                      $(embedFile "patches/clckwrks.diff"))
                               Nothing)
                  [P.BuildDep "hsx2hs"]
                  [] >>= inGroups ["clckwrks", "important", "testtarget"]
{-
  _clckwrks <- do
      cw <- git "https://github.com/clckwrks/clckwrks" []
      jstree <- uri "https://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip" "e211065e573ea0239d6449882c9d860d"
      p <- datafiles' cw jstree "jstree"
      json2 <- uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                   "a6d5fdbbcb076dd9385dd2135dbfb589" {-previouly: "5eecb009ae16dc54f261f31da01dbbac"-}
      datafiles' p json2 "json2" >>= patch $(embedFile "patches/clckwrks.diff")
                                 >>= flag (P.BuildDep "hsx2hs")
                                 >>= flip inGroup "clckwrks", "important"
                                 >>= debianize []
-}
{-
  _clckwrks <- pure (P.Package { P._spec = Debianize'' (Patch
                                                (DataFiles
                                                 (DataFiles
                                                  (Git "https://github.com/clckwrks/clckwrks" [])
                                                  (Uri "https://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                       "e211065e573ea0239d6449882c9d860d")
                                                  "jstree")
                                                 (Uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                                                      "a6d5fdbbcb076dd9385dd2135dbfb589" {-previouly: "5eecb009ae16dc54f261f31da01dbbac"-})
                                                 "json2")
                                                $(embedFile "patches/clckwrks.diff")) Nothing
                             , P._flags = [P.BuildDep "hsx2hs"]
                             , P._post = [] }) >>= inGroups ["clckwrks", "important"] :: Monad m => TSt m m Package
-}
  _clckwrks_theme_bootstrap <-  (gitrepo "clckwrks-theme-bootstrap" >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clckwrks_theme_clckwrks <-  (gitrepo "clckwrks-theme-clckwrks" >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _clock <-  (hackage (Just "0.7.2") "clock") >>= debianize [] >>= ghcjs_also
  -- _closure_compiler <- apt "sid" "closure-compiler"
  _cmark <-  (hackage (Just "0.5.3.1") "cmark") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _cmdargs <-  (hackage (Just "0.10.14") "cmdargs") >>= debianize [] >>= ghcjs_also
  _colour <-  hackage (Just "2.3.3") "colour" >>= flag (P.DebVersion "2.3.3-8") >>= debianize []
               -- , apt "wheezy" "haskell-configfile"
  _comfort_graph <- hackage (Just "0.0.1") "comfort-graph" >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "transformers dependency too old")
  _comonad <- hackage (Just "5") "comonad" >>=
                     apply (replacement "comonad" "comonad-transformers") >>=
                     apply (replacement "comonad" "comonad-fd") >>= debianize [] >>= ghcjs_also
  _concatenative <-  (hackage (Just "1.0.1") "concatenative") >>= debianize []
  _concrete_typerep <-  hackage (Just "0.1.0.2") "concrete-typerep" >>=
                        tflag (P.DebVersion "0.1.0.2-2build3") >>=
                        debianize [] >>=
                        skip (Reason "Constructor ‘TypeRep’ should have 4 arguments, but has been given 3")
  _cond <-  (hackage (Just "0.4.1.1") "cond") >>= flag (P.DebVersion "0.4.1.1-4") >>= debianize []
  _conduit <-  (hackage (Just "1.2.6.6") "conduit") >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _conduit_extra <-  (hackage (Just "1.1.13.2") "conduit-extra") >>= debianize [] >>= inGroups ["conduit", "important", "servant"] >>= ghcjs_also
  _configurator <- hackage (Just "0.3.0.0") "configurator" >>= flag (P.DebVersion "0.3.0.0-3build1") >>= debianize []
  _configFile <-  (hackage (Just "1.1.4") "ConfigFile") >>= flag (P.DebVersion "1.1.4-2build1") >>= debianize []
  _connection <-  (hackage (Just "0.2.5") "connection") >>= flag (P.DebVersion "0.2.5-2build1") >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _constrained_normal <-  (hackage (Just "1.0.2") "constrained-normal") >>= debianize []
  _constraints <- hackage (Just "0.8") "constraints" >>= debianize []
  _consumer <- darcs ("http://src.seereason.com/haskell-consumer") >>= skip (Reason "build failure")
  _contravariant <-  (hackage (Just "1.4") "contravariant") >>= debianize [] >>= ghcjs_also
  _control_monad_free <-  (hackage (Just "0.6.1") "control-monad-free") >>= flag (P.DebVersion "0.6.1-3") >>= debianize []
  _cookie <-  (hackage (Just "0.4.2.1") "cookie") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _cpphs <-  (hackage (Just "1.20.1") "cpphs") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  -- apt "sid" "debian-keyring=2014.03.03" -- The current version (2014.04.25) seems to be missing some keys that we need
  _cprng_aes <-  (hackage (Just "0.6.1") "cprng-aes") >>= flag (P.DebVersion "0.6.1-3build1") >>= debianize []
  _cpu <- hackage (Just "0.1.2") "cpu" >>= flag (P.DebVersion "0.1.2-5") >>= debianize []
  -- A bunch of missing dependencies - glob, cassava, hastache, statistics
  -- _criterion <- hackage (Just "1.1.1.0") "criterion" >>= debianize []
  _crypto_api <- hackage (Just "0.13.2") "crypto-api" >>= flag (P.DebVersion "0.13.2-5") >>= debianize [] >>= ghcjs_also
               -- The certificate package may need to be updated for version 0.4
  _crypto_cipher_types <- hackage (Just "0.0.9") "crypto-cipher-types" >>= flag (P.DebVersion "0.0.9-5build1") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto <-  (hackage (Just "4.2.5.1") "Crypto") >>= flag (P.DebVersion "4.2.5.1-5build1") >>= debianize []
  _cryptohash_conduit <- hackage (Just "0.1.1") "cryptohash-conduit" >>= flag (P.DebVersion "0.1.1-6build1") >>= debianize [] >>= inGroups ["servant"] >>= ghcjs_also
  _cryptohash_cryptoapi <-  (hackage (Just "0.1.4") "cryptohash-cryptoapi") >>= debianize [] >>= inGroups ["happstack", "important"]
  _cryptohash_sha256 <- hackage (Just "0.11.100.1") "cryptohash-sha256" >>= debianize []
  _cryptohash <-  (hackage (Just "0.11.9") "cryptohash") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _cryptonite <-  (hackage (Just "0.17") "cryptonite") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _crypto_numbers <-  (hackage (Just "0.2.7") "crypto-numbers") >>= flag (P.DebVersion "0.2.7-5build1") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey <-  (hackage (Just "0.2.8") "crypto-pubkey") >>= flag (P.DebVersion "0.2.8-5build1") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey_types <-  (hackage (Just "0.4.3") "crypto-pubkey-types") >>= flag (P.DebVersion "0.4.3-5build1") >>= debianize []
               -- crypto-pubkey-types-0.3.2 depends on older asn1-types
  _crypto_random_api <- hackage (Just "0.2.0") "crypto-random-api" >>= flag (P.DebVersion "0.2.0-6") >>= debianize []
  _crypto_random <-  (hackage (Just "0.0.9") "crypto-random") >>= flag (P.DebVersion "0.0.9-4build1") >>= debianize []
  _css <-  (hackage (Just "0.2") "css") >>= debianize [] >>= skip (Reason "No instance for (Applicative (CSSM x))")
  _css_text <-  (hackage (Just "0.1.2.2") "css-text") >>= debianize [] >>= inGroups ["important"]
  _csv <- hackage (Just "0.1.2") "csv" >>= flag (P.DebVersion "0.1.2-9build1") >>= debianize []
  _curl <- hackage (Just "1.3.8") "curl" >>= flag (P.DebVersion "1.3.8-7") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-curl"
  _currency <-  (hackage (Just "0.2.0.0") "currency") >>= debianize []
  _data_accessor <-  (hackage (Just "0.2.2.7") "data-accessor") >>= debianize []
  _data_accessor_template <-  (hackage (Just "0.2.1.13") "data-accessor-template") >>= debianize []
  _data_binary_ieee754 <- hack (Just "0.4.4") "data-binary-ieee754" >>= flag (P.DebVersion "0.4.4-5")
  _data_default_class <- hackage (Just "0.1.2.0") "data-default-class" >>= debianize [] >>= ghcjs_also
  _data_default <- hackage (Just "0.7.1.1") "data-default" >>= debianize [] >>= ghcjs_also
  _data_default_instances_base <- hackage (Just "0.1.0.1") "data-default-instances-base" >>= debianize [] >>= ghcjs_also
  _data_default_instances_containers <-  (hackage (Just "0.0.1") "data-default-instances-containers") >>= flag (P.DebVersion "0.0.1-5") >>= debianize [] >>= ghcjs_also
  _data_default_instances_dlist <-  (hackage (Just "0.0.1") "data-default-instances-dlist") >>= flag (P.DebVersion "0.0.1-5") >>= debianize [] >>= ghcjs_also
  _data_default_instances_old_locale <-  (hackage (Just "0.0.1") "data-default-instances-old-locale") >>= flag (P.DebVersion "0.0.1-5") >>= debianize [] >>= ghcjs_also
  _data_lens <- hackage (Just "2.10.7") "data-lens" {-`patch` $(embedFile "patches/data-lens.diff")-} >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "Unmet build dependencies: libghcjs-comonad-dev (<< 4.3) libghcjs-semigroupoids-dev (<< 5.1)")
  _data_lens_light <- hackage (Just "0.1.2.2") "data-lens-light" >>= debianize []
  -- _data_lens_template <-  (hackage (Just "2.1.9") "data-lens-template") >>= debianize []
  _data_object <-  (hackage (Just "0.3.1.9") "data-object" >>= patch $(embedFile "patches/data-object.diff")) >>= debianize []
  _data_ordlist <-  (hackage (Just "0.4.7.0") "data-ordlist") >>= flag (P.DebVersion "0.4.7.0-3") >>= debianize []
  _data_reify <-  (hackage (Just "0.6.1") "data-reify") >>= debianize []
  _data_r_tree <-  (hackage (Just "0.0.5.0") "data-r-tree") >>= debianize []
  _data_stringmap <-  (hackage (Just "1.0.1.1") "data-stringmap") >>= debianize []
  _date_cache <-  (hackage (Just "0.3.0") "date-cache" >>= tflag (P.DebVersion "0.3.0-3")) >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _datetime <- hackage (Just "0.3.1") "datetime" {->>= pflag (P.DebVersion "0.2.1-2") >>= tflag (P.DebVersion "0.2.1-5build1")-} >>= debianize []
  _debhelper <- apt "wheezy" "debhelper" >>= patch $(embedFile "patches/debhelper.diff") >>= skip (Reason "Use standard")
  _debian_haskell <- git "https://github.com/ddssff/debian-haskell" [] >>= flag (P.RelaxDep "cabal-debian") >>= inGroups ["autobuilder-group", "important"]
  _debian_repo <- git "https://github.com/ddssff/debian-repo" [] >>= inGroups ["autobuilder-group", "important"]
  _debootstrap <- apt "sid" "debootstrap" >>= flag (P.UDeb "debootstrap-udeb")
               -- Build fails due to some debianization issue
               -- , apt "wheezy" "geneweb"
  _decimal <-  (hackage (Just "0.4.2") "Decimal") >>= flag (P.DebVersion "0.4.2-4") >>= debianize [] -- for hledger
  _deepseq_generics <- hackage (Just "0.2.0.0") "deepseq-generics" >>= {-patch $(embedFile "patches/deepseq-generics.diff") >>=-} debianize [] >>= ghcjs_also
  _derive <-  (hackage (Just "2.6.3") "derive") >>= debianize [] >>= flag (P.CabalDebian ["--executable", "derive"])
  --_diff <- hackage (Just "0.3.4") "Diff" >>= debianize [] >>= inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _diff <- git "https://github.com/seereason/Diff" [] >>= debianize [] >>= inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _digest <- hackage (Just "0.0.1.2") "digest" >>= flag (P.DebVersion "0.0.1.2-5") >>= debianize [] >>= ghcjs_also
  _digestive_functors <-  (hackage (Just "0.2.1.0") "digestive-functors" >>= flag (P.CabalPin "0.2.1.0")) >>= debianize [] >>= inGroups ["seereason", "important"]  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
      -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
  _digestive_functors_happstack <-  (git "https://github.com/seereason/digestive-functors" []
                                            >>= cd "digestive-functors-happstack"
                                            >>= flag (P.DebVersion "0.1.1.5-2")) >>= debianize [] >>= inGroups ["digestive-functors", "appraisalscribe", "important"]
  _digestive_functors_hsp <-  (darcs ("http://src.seereason.com/digestive-functors-hsp") >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["seereason", "important"]
  _directory_tree <-  (hackage (Just "0.12.0") "directory-tree") >>= flag (P.DebVersion "0.12.0-3") >>= debianize []
  _distributive <-  (hackage (Just "0.5.0.2") "distributive") >>= debianize [] >>= ghcjs_also
  _dlist <-  (hackage (Just "0.8.0.1") "dlist") >>= debianize []
               -- Natty only(?)
  _doctemplates <- hackage (Just "0.1.0.2") "doctemplates" >>= debianize [] >>= ghcjs_also
  _doctest <-  (hackage (Just "0.11.0") "doctest") >>= debianize [] {->>= ghcjs_also-}
      -- This package fails to build in several different ways because it has no modules.
      -- I am just going to patch the packages that use it to require transformers >= 0.3.
      -- Specifically, distributive and lens.
  _double_conversion <-  (hackage (Just "2.0.1.0") "double-conversion") >>= debianize [] >>= ghcjs_also
  _dpkg <- apt "wheezy" "dpkg" >>= patch $(embedFile "patches/dpkg.diff") >>= skip (Reason "use standard")
  _drbg <-  (hackage (Just "0.5.4") "DRBG") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= skip (Reason "requires update to use current cereal")
  -- Depends on several old packages
  -- _dropbox_sdk <-  (hackage (Just "0.3.1") "dropbox-sdk") >>= debianize [] >>= patch $(embedFile "patches/dropbox-sdk.diff")
  _dynamic_state <-  (hackage (Just "0.2.2.0") "dynamic-state") >>= debianize []
  _dyre <-  (hackage (Just "0.8.12") "dyre") >>= flag (P.DebVersion "0.8.12-1") >>= debianize []
  _easy_file <-  (hackage (Just "0.2.1") "easy-file") >>= flag (P.DebVersion "0.2.1-3") >>= debianize [] >>= ghcjs_also
  _ed25519 <- hackage (Just "0.0.5.0") "ed25519" >>= flag (P.DebVersion "0.0.5.0-1") >>= debianize []
  _edisonAPI <- hackage (Just "1.3.1") "EdisonAPI" >>= debianize []
  _edisonCore <- ( (hackage (Just "1.3.1.1") "EdisonCore" >>= qflag (P.DebVersion "1.2.1.3-9build2")) >>= debianize [])
  _ekg_core <-  (hackage (Just "0.1.1.1") "ekg-core") >>= debianize []
  _email_validate <-  (hackage (Just "2.2.0") "email-validate") >>= debianize [] >>= inGroups ["important"]
  _enclosed_exceptions <-  (hackage (Just "1.0.2") "enclosed-exceptions") >>= debianize [] >>= inGroups ["ghcjs-comp"] >>= ghcjs_also
  _entropy <- hackage (Just "0.3.7") "entropy" >>= flag (P.DebVersion "0.3.7-3") >>= debianize [] >>= ghcjs_also
  _enumerator <- hackage (Just "0.4.20") "enumerator" >>= flag (P.DebVersion "0.4.20-4build1") >>= debianize []
  _erf <- hackage (Just "2.0.0.0") "erf" >>= flag (P.DebVersion "2.0.0.0-9") >>= debianize []
  _errors <-  (hackage (Just "2.1.2") "errors") >>= debianize [] >>= ghcjs_also
  _exceptions <-  (hackage (Just "0.8.3") "exceptions") >>= debianize [] >>= ghcjs_also
  _expiring_cache_map <-  (hackage (Just "0.0.5.4") "expiring-cache-map") >>= debianize []
  _executable_path <- hackage (Just "0.0.3") "executable-path" >>= flag (P.DebVersion "0.0.3-7") >>= debianize []
               -- , apt (rel release "wheezy" "quantal") "haskell-digest"
               -- , apt (rel release "wheezy" "quantal") "haskell-dlist"
  _extensible_exceptions <- hackage (Just "0.1.1.4") "extensible-exceptions" >>= flag (P.DebVersion "0.1.1.4-6") >>= debianize []
  _extra <-  (hackage (Just "1.5") "extra") >>= debianize [] >>= ghcjs_also
  -- In ghc-8 this has no modules, so use NoDoc to avoid a haddock error
  _fail <- hackage (Just "4.9.0.0") "fail" >>= flag (P.BuildDep "hscolour") >>= debianize [] >>= flag P.NoDoc >>= ghcjs_also
  _failure <- hackage (Just "0.2.0.3") "failure" >>= flag (P.DebVersion "0.2.0.3-5") >>= debianize []
  -- Patch removes dependency on bytestring-builder, now part of bytestring.
  _fast_logger <- hackage (Just "2.4.6") "fast-logger">>= patch $(embedFile "patches/fast-logger.diff") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _fay_base <-  (hackage (Just "0.20.0.1") "fay-base") >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _fay <-  (hackage (Just "0.23.1.16") "fay" {- >>= patch $(embedFile "patches/fay.diff") -}) >>= debianize [] >>= flag (P.CabalDebian [ "--depends", "haskell-fay-utils:cpphs" ]) >>= skip (Reason "too old for current syb and optparse-applicative")
  _fay_jquery <-  (git "https://github.com/faylang/fay-jquery" []) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _fay_text <-  (hackage (Just "0.3.2.2") "fay-text") >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _fb <- git "https://github.com/ddssff/fb.git" [] >>= flag (P.DebVersion "1.0.13-1") >>= debianize [] >>= inGroups [ "authenticate", "appraisalscribe", "important"]
  _feed <- hackage (Just "0.3.12.0") "feed" >>= {-flag (P.DebVersion "0.3.10.4-1build1") >>=-} debianize []
  _fgl <-  (hackage (Just "5.5.3.0") "fgl") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _file_embed <-  (hackage (Just "0.0.10") "file-embed") >>= debianize [] >>= ghcjs_also
  _file_location <- hackage (Just "0.4.9.1") "file-location" >>= {-flag (P.CabalDebian [ "--source-package-name", "file-location" ]) >>=-} debianize []
  _filelock <- hackage (Just "0.1.1.2") "filelock" >>= debianize []
  _filemanip <- git "https://github.com/ddssff/filemanip" [] >>= debianize [] >>= ghcjs_also
  _filemanip_extra <- git "https://github.com/seereason/filemanip-extra" [] >>= debianize [] >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _fingertree <- hack (Just "0.1.1.0") "fingertree" >>= flag (P.DebVersion "0.1.1.0-3")
  _fixed <- hackage (Just "0.2.1.1") "fixed" >>= debianize []
  _flock <- hackage (Just "0.3.1.8") "flock" >>= debianize []
  _fmlist <-  (hackage (Just "0.9") "fmlist") >>= flag (P.DebVersion "0.9-4") >>= debianize [] >>= ghcjs_also
  _foreign_var <-  (hackage (Just "0.1") "foreign-var") >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "dependencies")
  _formlets <-  (hackage (Just "0.8") "formlets"
                               >>= patch $(embedFile "patches/formlets.diff")
                               >>= flag (P.DebVersion "0.8-1~hackage1")) >>= debianize []
  _free <-  (hackage (Just "4.12.4") "free") >>= debianize [] >>= ghcjs_also
  _frquotes <-  (hackage (Just "0.2.1") "frquotes") >>= debianize []
               -- Usable versions of this package are available in some dists -
               -- e.g. trusty and wheezy.
               -- , apt "trusty" "foo2zjs"
  _fsnotify <-  (hackage (Just "0.2.1") "fsnotify") >>= flag (P.DebVersion "0.2.1-2build1") >>= debianize []
  _ftgl <-  (hackage (Just "2.1") "FTGL"
                     -- >>= patch $(embedFile "patches/FTGL.diff")
                     >>= flag (P.DevelDep "libftgl-dev")
                     >>= flag (P.DevelDep "libfreetype6-dev")) >>= debianize []
  _gd <- hackage (Just "3000.7.3") "gd" >>= patch $(embedFile "patches/gd.diff")
                       >>= flag (P.DebVersion "3000.7.3-8")
                       >>= flag (P.DevelDep "libgd-dev")
                       >>= flag (P.DevelDep "libc6-dev")
                       >>= flag (P.DevelDep "libfreetype6-dev") >>= debianize []
               -- ,  (flags [P.BuildDep "libm-dev", P.BuildDep "libfreetype-dev"] (hackage (Just "3000.7.3") "gd")) >>= debianize []
  _gdiff <-  (hackage (Just "1.1") "gdiff") >>= debianize []
  _gdiff_th <- git "https://github.com/ddssff/gdiff-th" [] >>= debianize []
  _generic_deriving <-  (hackage (Just "1.10.7") "generic-deriving") >>= debianize [] >>= ghcjs_also
  _genI <- darcs "http://hub.darcs.net/kowey/GenI" >>= patch $(embedFile "patches/GenI.diff") >>= debianize [] >>= inGroups ["GenI"]
  _ghc_boot <- hackage (Just "8.0.1") "ghc-boot" >>= debianize [] >>= skip (Reason "Encountered missing dependencies: 2> binary ==0.8.*")
  _ghc_boot_th <- hackage (Just "8.0.2") "ghc-boot-th" >>= debianize []
  _ghc_exactprint <- git "https://github.com/alanz/ghc-exactprint" [] >>= debianize []
  _terminal_size <- hackage (Just "0.3.2.1") "terminal-size"  >>= flag (P.DebVersion "0.3.2.1-2") >>= debianize [] >>= inGroups ["ghcid"]
  _ghcid <- hackage (Just "0.6.4") "ghcid" >>= debianize [] >>= inGroups ["ghcid"]
  _ghcjs_base <- git "https://github.com/ghcjs/ghcjs-base" [] >>= debianize [] >>= ghcjs
  _ghcjs_jquery <-  git "https://github.com/ghcjs/ghcjs-jquery" [] >>=
                    debianize [] {-`putSrcPkgName` "ghcjs-ghcjs-jquery"-} >>=
                    patch $(embedFile "patches/ghcjs-jquery.diff") >>=
                    ghcjs
  -- ghcjs_vdom = ghcjs_flags ( (git "https://github.com/seereason/ghcjs-vdom" [Branch "base48"]) >>= debianize `putSrcPkgName` "ghcjs-ghcjs-vdom")
  _ghcjs_ffiqq <- git "https://github.com/ghcjs/ghcjs-ffiqq" [] >>= putSrcPkgName "ghcjs-ghcjs-ffiqq" >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "[libghc-ghcjs-base-doc] -> []")
  _ghcjs_dom <- hackage (Just "0.2.4.0" {-"0.7.0.4"-} {-"0.4.0.0"-}) "ghcjs-dom" >>=
                flag (P.CabalPin "0.2.4.0") >>=
                debianize [] >>=
                inGroups ["glib"] >>= ghcjs
  -- _ghcjs_dom_jsffi <- hackage (Just "0.7.0.4") "ghcjs-dom-jsffi" >>= debianize [] >>= ghcjs_also
  _ghcjs_dom_hello <- hackage (Just "3.0.0.0") "ghcjs-dom-hello" >>=
                      patch $(embedFile "patches/ghcjs-dom-hello.diff") >>=
                      flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"]) >>=
                      debianize [] >>=
                      inGroups ["glib"] >>= ghcjs
  -- In a hurry right now
  _ghcjs_prim <- git "https://github.com/ghcjs/ghcjs-prim" [] >>= debianize [] >>= inGroups ["ghcjs-comp", "glib"] >>= ghcjs
  _ghc_mtl <- (hackage (Just "1.2.1.0") "ghc-mtl") >>= flag (P.DebVersion "1.2.1.0-4build3") >>= debianize [] {- >>= skip (Reason "No instance for (MonadIO GHC.Ghc)") -}
  _ghc_paths <- hackage (Just "0.1.0.9") "ghc-paths" >>= flag (P.DebVersion "0.1.0.9-7") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-ghc-paths" -- for leksah
               -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
               -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
               --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
               -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
               -- Errors were encountered while processing:
               --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
               -- E: Sub-process /usr/bin/dpkg returned an error code (1)
  _ghc_simple <- (hackage (Just "0.4") "ghc-simple") >>= debianize [] >>= skip (Reason "Requires directory==1.2, also we do not use")
  _gio <- hackage (Just "0.13.1.1") "gio" >>= patch $(embedFile "patches/gio.diff") >>= inGroups ["glib"] >>= skip (Reason "see glib")
  _glfw <-  (hackage (Just "0.5.2.4") "GLFW" >>= inGroups ["gl"] >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize [] >>= skip (Reason "Waiting for newer opengl")
      -- ,  (hackage (Just "1.4.6") "GLFW-b") >>= debianize []
      -- ,  (hackage "GLFW-b-demo" >>= flag (P.SkipPackage {- >>= patch $(embedFile "patches/GLFW-b-demo.diff") -})) >>= debianize []
  _glfw_task <-  (hackage (Just "0.2.0") "GLFW-task" >>= inGroups ["gl"]) >>= debianize [] >>= skip (Reason "Waiting for newer opengl")
  _glib <- hackage (Just "0.13.2.2") "glib" >>=
           flag (P.BuildDep "gtk2hs-buildtools") >>=
           flag (P.BuildDep "libpango1.0-dev") >>=
           flag (P.BuildDep "libgtk2.0-dev") >>=
           flag (P.BuildDep "libglib2.0-dev") >>=
           debianize [] >>= inGroups ["glib"] >>= skip (Reason "Could not find module Gtk2HsSetup")
  _gluRaw <- (hackage (Just "2.0.0.2") "GLURaw" >>= inGroups ["gl"]) >>= debianize []
  _glut <-  (hackage (Just "2.7.0.3") "GLUT" >>= inGroups ["gl"]
                     >>= flag (P.DevelDep "freeglut3-dev")) >>= debianize [] >>= skip (Reason "Waiting for newer opengl")
  _groom <-  (hackage (Just "0.1.2") "groom") >>= debianize [] >>= ghcjs_also
               -- Retired
               -- , apt "wheezy" "haskell-dummy"
               -- Need this when we upgrade blaze-textual to 0.2.0.0
               -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
  _gtk2hs_buildtools <- hackage (Just "0.13.0.5" {-"0.13.2.1"-}) "gtk2hs-buildtools"
                              >>= flag (P.CabalPin "0.13.0.5") -- >= 0.13.1 requires Cabal-1.24
                              >>= flag (P.CabalDebian ["--default-package", "gtk2hs-buildtools",
                                                       "--build-dep", "alex",
                                                       "--build-dep", "happy",
                                                       "--revision", ""])
                              >>= debianize []
                              >>= inGroups ["glib"]
               -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
  _gtk3 <- hackage (Just "0.13.9") "gtk3" >>= flag (P.CabalPin "0.13.9") >>= flag (P.BuildDep "libgtk-3-dev") >>= debianize [] >>= inGroups ["glib"] >>= skip (Reason "see cairo and glib")
  _gyp <- apt "sid" "gyp" >>= skip (Reason "Use standard")
  _hackage_security <- hackage (Just "0.5.2.2") "hackage-security" >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>= debianize []
  _half <-  (hackage (Just "0.2.2.3") "half" >>= inGroups ["gl"]) >>= debianize []
  _hamlet <-  (hackage (Just "1.2.0") "hamlet") >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "No input files to haddock?")
  -- seereason still depends on this
  _happstack_authenticate_0 <-  (git "https://github.com/Happstack/happstack-authenticate-0.git" []
                             >>= flag (P.CabalDebian [ "--debian-name-base", "happstack-authenticate-0",
                                                    "--cabal-flags", "migrate",
                                                    "--executable", "happstack-authenticate-migrate" ])) >>= debianize [] >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_authenticate <- git "https://github.com/Happstack/happstack-authenticate.git" [] >>= debianize [] >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_clckwrks <-  (git ("https://github.com/Happstack/happstack-clckwrks") [] >>=
                             cd "clckwrks-theme-happstack"
                             -- >>= patch $(embedFile "patches/clckwrks-theme-happstack.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _happstack_dot_com <-  (git ("https://github.com/Happstack/happstack-clckwrks") []
                                   >>= cd "happstack-dot-com"
                                   -- This is a change that only relates to the autobuilder
                                   >>= patch $(embedFile "patches/happstack-dot-com.diff")) >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _happstackDotCom_doc <- darcs ("http://src.seereason.com/happstackDotCom-doc") >>= inGroups ["happstack", "important"]
  _happstack_extra <-  (git "https://github.com/seereason/happstack-extra.git" []) >>= debianize []
  _happstack_fay_ajax <-  (hackage (Just "0.2.0") "happstack-fay-ajax" >>= patch $(embedFile "patches/happstack-fay-ajax.diff")) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
      -- ,  (hackage "fay-hsx" >>= patch $(embedFile "patches/fay-hsx.diff")) >>= debianize []
  _happstack_fay <-  (hackage (Just "0.2.0") "happstack-fay" >>= patch $(embedFile "patches/happstack-fay.diff")) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _happstack_foundation <-  (git "https://github.com/Happstack/happstack-foundation.git" []) >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_foundation_example <-
       (git "https://github.com/Happstack/happstack-foundation.git" []
                                   >>= cd "examples/ControlVAuth"
                                   >>= flag (P.CabalDebian ["--source-package-name", "happstack-foundation-example",
                                                         "--default-package", "happstack-foundation-example"])) >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_hsp <-  (git "https://github.com/Happstack/happstack-hsp.git" []) >>= debianize [] >>= inGroups ["happstack", "lens", "important"]
  _happstack_jmacro <-  (git "https://github.com/Happstack/happstack-jmacro.git" []) >>= debianize [] >>= inGroups ["happstack", "lens", "important"]
  _happstack_lite <- hackage (Just "7.3.6") "happstack-lite" >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_plugins <-  (hackage (Just "7.0.2") "happstack-plugins" >>= patch $(embedFile "patches/happstack-plugins.diff")) >>= debianize [] >>= skip (Reason "Needs plugins-auto")
  _happstack_scaffolding <-  (git "https://github.com/seereason/happstack-scaffolding" [] >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["seereason", "important"]
  _happstack_search <- darcs ("http://src.seereason.com/happstack-search") >>= inGroups ["happstack", "important"]
              -- ,  (hackage (Just "7.4.6.2") "happstack-server") >>= debianize []
  _happstack_servant <- git "https://github.com/Happstack/servant-happstack" [] >>= debianize []
  _happstack_server <- hackage (Just "7.5.0") "happstack-server" >>=
                       debianize [] >>=
                       inGroups ["happstack", "important"]
  _happstack_server_tls <- hackage (Just "7.1.6.3") "happstack-server-tls" >>= debianize [] >>= inGroups ["happstack", "important"]
  _happstack_static_routing <-  (hackage (Just "0.4.2") "happstack-static-routing") >>= debianize [] {->>= inGroups ["happstack", "important"]-} >>= skip (Reason "compile error")
  _happstack_util <- git "https://github.com/seereason/happstack-util" [] >>=
                     flag (P.DebVersion "6.0.3-1") >>=
                     debianize [] >>=
                     inGroups ["happstack", "important"]
  _happstack_websockets <- git "https://github.com/seereason/happstack-websockets" [] >>= debianize []
  _harp <-  (git "https://github.com/seereason/harp" []) >>= debianize []
  _hashable <-  (hackage (Just "1.2.4.0") "hashable") >>= debianize []
  _hashed_storage <-  (hackage (Just "0.5.11") "hashed-storage") >>= debianize [] >>= skip (Reason "Non type-variable argument in the constraint: MonadState (TreeState m_aFTg) (t m_aFTg)")
               -- Built into ghc-7.8.3
  _hashtables <- hackage (Just "1.2.1.1") "hashtables" >>= debianize []
  -- _haskeline <- hackage (Just "0.7.2.3") "haskeline" >>= debianize []
  _haskell_darcs <-  (darcs "http://darcs.net/reviewed"
                     >>= flag (P.CabalDebian ["--source-package-name", "darcs"])
                     >>= flag (P.CabalDebian ["--default-package", "darcs"])
                     >>= flag (P.CabalDebian ["--cabal-flags", "-http"]) -- the http flag forces network < 2.5
                     -- >>= patch $(embedFile "patches/darcs.diff")
                    ) >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-vector-dev (<< 0.11)")
  _haskell_either <-  (hackage (Just "4.4.1.1") "either") >>= debianize []
  _sr_extra <-  (git ("https://github.com/seereason/sr-extra") []
                              -- Don't push out libghc-extra-dev, it now comes from Neil Mitchell's repo
                              {- `apply` (replacement "sr-extra" "Extra") -}
                       ) >>= debianize [] >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _sr_order <- git "https://github.com/seereason/sr-order" [] >>= debianize [] >>= ghcjs_also
  _haskell_help <- git ("https://github.com/seereason/sr-help") [] >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _haskell_lexer <-  (hackage (Just "1.0.1") "haskell-lexer") >>= debianize []
  _haskell_list <-  (hackage (Just "0.5.2") "List") >>= debianize []
  -- _haskell_mode <- apt "jessie" "haskell-mode" >>= patch $(embedFile "patches/haskell-mode.diff")
  _haskell_names <- hackage (Just "0.8.0") "haskell-names" >>= debianize []
  _haskell_newtype <- hackage (Just "0.2") "newtype" >>= flag (P.DebVersion "0.2-7") >>= debianize []
  _haskell_packages <-  (hackage (Just "0.3") "haskell-packages" {->>= patch $(embedFile "patches/haskell-packages.diff")-}) >>= debianize [] >>= inGroups ["happstack", "important"] >>= skip (Reason "duplicate FromJSON instances")
  _sr_revision <-  (git ("https://github.com/seereason/sr-revision") []) >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also >>= skip2 (Reason "Not used")
  _haskell_src <-  (hackage (Just "1.0.2.0") "haskell-src" >>= flag (P.BuildDep "happy")) >>= flag (P.DebVersion "1.0.2.0-4build1") >>= debianize [] >>= inGroups ["platform"]
  -- The source package name is set to haskell-src-exts by the
  -- cabal-debian package, Debian.Debianize.Details.debianDefaults.
  -- But where does that leave ghcjs-haskell-src-exts?
  _haskell_src_exts <- hackage (Just "1.18.2") "haskell-src-exts" >>=
                       debianize [] >>=
                       flag (P.BuildDep "happy") >>=
                       inGroups ["important"] >>= ghcjs_also
  -- This goes with haskell-src-exts-1.18.*
  -- _haskell_src_exts_simple <- hackage "haskell-src-exts-simple" >>= debianize []
  _haskell_src_meta <-
      -- git "https://github.com/ddssff/haskell-src-meta" [] >>=
      hackage (Just "0.7.0") "haskell-src-meta" >>=
      debianize [] >>=
      inGroups ["happstack", "important"] >>= ghcjs_also
  -- _hastache <- hackage Nothing "hastache" >>= debianize []
  _haste_compiler <- hack (Just "0.5.5.0") "haste-compiler" >>= flag (P.CabalDebian ["--default-package", "haste-compiler"]) >>= skip (Reason "Unmet build dependencies: libghc-shellmate-dev (<< 0.3) libghc-shellmate-prof (<< 0.3)")
  _haste_ffi_parser <- git' "https://github.com/RudolfVonKrugstein/haste-ffi-parser" []
  _haTeX <- hackage Nothing "HaTeX" >>=
            -- git "https://github.com/seereason/HaTeX" [Branch "linebreak"] >>= -- adds mapLaTeXT, fix \\ format
            -- patch $(embedFile "patches/HaTeX-texty.diff") >>=
            -- patch $(embedFile "patches/HaTeX-doc.diff") >>=
            debianize [] >>= ghcjs_also
  _haXml <- hackage (Just "1.25.4") "HaXml" >>= debianize [] >>= inGroups ["pretty"] >>= ghcjs_also
  _hclip <- hackage (Just "3.0.0.4") "Hclip" >>= debianize []
  _hdaemonize <-  (git "https://github.com/madhadron/hdaemonize" []) >>= debianize [] >>= skip (Reason "Module ‘System.Posix.Syslog’ does not export ‘syslog’")
  _heap <-  (hackage (Just "1.0.3") "heap") >>= debianize []
               -- ,  (hackage (Just "0.13.1.2") "heist" >>= patch $(embedFile "patches/heist.diff")) >>= debianize []
  _hex <-  (hackage (Just "0.1.2") "hex") >>= flag (P.DebVersion "0.1.2-2") >>= debianize []
  _hexpat <- hackage (Just "0.20.9") "hexpat" >>= debianize []
  _highlighting_kate <-  (hackage (Just "0.6.2.1") "highlighting-kate") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _hinotify <-  (hackage (Just "0.3.8.1") "hinotify") >>= flag (P.DebVersion "0.3.8.1-3") >>= debianize []
  _hint <-  (hackage (Just "0.6.0") "hint") >>= debianize [] {- >>= skip (Reason "requires ghc-mtl") -}
  -- _hit <- hackage Nothing "hit" >>= debianize []
  _hJavaScript <- hackage (Just "0.4.7") "HJavaScript"
                  >>= patch $(embedFile "patches/HJavaScript.diff")
                  -- >>= tflag (P.DebVersion "0.4.7-6")
                  >>= debianize []
               -- Not used, and not building.
               -- ,  (hackage (Just "0.3.5") "hoauth") >>= debianize []
  _hJScript <- hackage (Just "0.7.0") "HJScript" >>= debianize [] >>= inGroups ["happstack", "important"]
  _hledger <- git "https://github.com/simonmichael/hledger" [] >>= cd "hledger-lib" >>= debianize [] >>= skip (Reason "requires mtl-compat")
           {-
               -- Needs a build dependency on libXrandr-dev and the cabal package x11.
               , P.Package { P.spec =  (Hackage "xmobar") >>= Debianize []
                           , P.flags = [] }
           -}
  _hlint <-  (hackage (Just "1.8.53") "hlint") >>= debianize [] >>= {- ghcjs_also >>= -} skip (Reason "[libghc-refact-doc] -> []")

  _hostname <- hackage (Just "1.0") "hostname" >>= flag (P.DebVersion "1.0-10") >>= debianize [] >>= ghcjs_also
               -- The Sid package has no profiling libraries, so dependent packages
               -- won't build.  Use our debianization instead.  This means keeping
               -- up with sid's version.
  _hourglass <-  (hackage (Just "0.2.10") "hourglass") >>= debianize [] >>= ghcjs_also
  -- _hpack <- hackage Nothing "hpack" >>= debianize []
  _hpdf <-  (hackage (Just "1.4.10") "HPDF") >>= debianize []
  _hs_bibutils <-  (hackage (Just "5.5") "hs-bibutils") >>= flag (P.DebVersion "5.5-3build1") >>= debianize []
  _hscolour <-  (hackage (Just "1.24.1") "hscolour") >>= debianize [] >>= flag (P.RelaxDep "hscolour") >>= ghcjs_also
  _hse_cpp <-  (hackage (Just "0.2") "hse-cpp") >>= debianize [] {->>= patch $(embedFile "patches/hse-cpp.diff")-} >>= inGroups ["happstack", "important"]
  -- _hse_cpp <- git "https://github.com/haskell-suite/hse-cpp" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _hsemail <-  (hackage (Just "1.7.7") "hsemail") >>= flag (P.DebVersion "1.7.7-3build1") >>= debianize [] -- (rel release [] [P.DebVersion "1.7.1-2build2"])
  _hslogger <-  (hackage (Just "1.2.10") "hslogger") >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _hslua <-  (hackage (Just "0.4.1") "hslua") >>= flag (P.DebVersion "0.4.1-7") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _hsOpenSSL <-  (hackage (Just "0.11.3.2") "HsOpenSSL"
                              >>= flag (P.DevelDep "libssl-dev")
                              >>= flag (P.DevelDep "libcrypto++-dev")) >>= debianize []
  _hsp <- hackage (Just "0.10.0") "hsp" >>= flag (P.DebVersion "0.10.0-3build1") >>= flag (P.BuildDep "hsx2hs") >>= debianize [] >>= inGroups ["happstack", "important"]
  _hspec <-  (hackage (Just "2.2.3") "hspec") >>= debianize [] >>= ghcjs_also
  _hspec_core <-  (hackage (Just "2.2.3") "hspec-core") >>= debianize [] >>= ghcjs_also
  _hspec_discover <-  (hackage (Just "2.2.3") "hspec-discover") >>= debianize [] >>= ghcjs_also
  _hspec_expectations <-  (hackage (Just "0.7.2") "hspec-expectations") >>= flag (P.DebVersion "0.7.2-1build1") >>= debianize [] >>= ghcjs_also
  _hspec_meta <-  (hackage (Just "2.2.1") "hspec-meta") >>= debianize []
  _hsSyck <-  (hackage (Just "0.53") "HsSyck") >>= debianize []
  _hStringTemplate <- hackage (Just "0.8.5") "HStringTemplate" >>= debianize []
  (_hsx2hs, _) <- hackage (Just "0.14.0") "hsx2hs" >>=
             patch $(embedFile "patches/hsx2hs.diff") >>=
             debianize [] >>=
             inGroups ["happstack", "lens", "important"] >>= ghcjs_also
  flag (P.CabalDebian ["--executable", "hsx2hs"]) _hsx2hs
  _hsx_jmacro <-  (git "https://github.com/Happstack/hsx-jmacro.git" []) >>= debianize [] >>= inGroups ["happstack", "lens", "important"]
  _hsyslog <-  (hackage (Just "4") "hsyslog") >>= debianize []
  _htf <-  (hackage (Just "0.13.1.0") "HTF" >>= flag (P.BuildDep "cpphs")) >>= debianize []
  _html <- hackage (Just "1.0.1.2") "html" >>= flag (P.DebVersion "1.0.1.2-11") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _html_entities <- darcs ("http://src.seereason.com/html-entities")
  _html_xml_utils <- apt "sid" "html-xml-utils"
  _http_api_data <- hackage (Just "0.2.4") "http-api-data" >>= debianize []
  _http_client <-  (hackage (Just "0.4.30" {-"0.5.4"-}) "http-client") >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _http_client_tls <- hackage (Just "0.2.4.1" {-"0.3.3"-}) "http-client-tls" >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
      -- Deprecated in favor of http-conduit
      -- ,  (hackage (Just "0.2.0.1") "http-client-conduit") >>= debianize []
      -- Deprecated in favor of conduit-extra
      -- ,  (hackage (Just "1.0.1.2") "attoparsec-conduit") >>= debianize []
      -- ,  (hackage (Just "1.0.0") "blaze-builder-conduit") >>= debianize []
      -- ,  (hackage (Just "1.0.0") "zlib-conduit") >>= debianize []
  _http_common <-  (hackage (Just "0.8.2.0") "http-common") >>= flag (P.DebVersion "0.8.2.0-2build1") >>= debianize [] >>= inGroups ["platform", "happstack", "important"]
  _http_conduit <-  (hackage (Just "2.1.11" {-"2.2.2.1"-}) "http-conduit") >>= debianize [] >>= inGroups ["conduit", "important"] -- fb isn't ready for 2.2
  _http_date <-  (hackage (Just "0.0.6.1") "http-date") >>= flag (P.DebVersion "0.0.6.1-3build1") >>= debianize []
  _http_media <-  (hackage (Just "0.6.4") "http-media") >>= debianize [] >>= inGroups ["servant"]
  _http <-  (hackage Nothing "HTTP") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _http2 <-  (hackage (Just "1.6.1") "http2") >>= debianize []
  _http_streams <-  (hackage (Just "0.8.4.0") "http-streams") >>= debianize [] >>= inGroups ["platform", "appraisalscribe", "important"]
  _http_types <-  (hackage (Just "0.8.6") "http-types" >>= flag (P.CabalPin "0.8.6")) >>= flag (P.DebVersion "0.9-2") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also -- web-routes specifies << 0.9
  _hUnit <-  (hackage (Just "1.3.1.1") "HUnit") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _hunt <-  (git "https://github.com/hunt-framework/hunt.git" [] >>= cd "hunt-searchengine" ) >>= debianize [] >>= skip (Reason "No instance for (Foldable t0) arising from a use of ‘elem’")
  _hxt_charproperties <-  (hackage (Just "9.2.0.1") "hxt-charproperties") >>= flag (P.DebVersion "9.2.0.1-4") >>= debianize [] >>= ghcjs_also
  _hxt <- hackage (Just "9.3.1.15") "hxt" >>= flag (P.CabalDebian ["--cabal-flags", "network-uri"]) >>= flag (P.DebVersion "9.3.1.15-4build1") >>= debianize [] >>= ghcjs_also
  _hxt_regex_xmlschema <-  (hackage (Just "9.2.0.2") "hxt-regex-xmlschema") >>= flag (P.DebVersion "9.2.0.2-4build1") >>= debianize [] >>= ghcjs_also
  _hxt_unicode <- hackage (Just "9.0.2.4") "hxt-unicode" >>= flag (P.DebVersion "9.0.2.4-4") >>= debianize [] >>= ghcjs_also
               -- ,  (darcs "haskell-tiny-server" ("http://src.seereason.com/tiny-server") >>= flag (P.BuildDep "hsx2hs")
               --                >>= flag (P.SkipPackage {- has a "derives SafeCopy" -})) >>= debianize []
  _i18n <-  (hackage (Just "0.3") "i18n" >>= flag (P.DebVersion "0.3-1~hackage1")) >>= debianize [] >>= skip (Reason "Could not find module ‘System.IO.UTF8’")
  _iconv <-  (hackage (Just "0.4.1.3") "iconv") >>= flag (P.DebVersion "0.4.1.3-3") >>= debianize []
  _idris <-  hackage (Just "0.9.15.1") "idris" >>=
             -- patch $(embedFile "patches/idris.diff") -- adds *.idr to extra-source-files >>=
             flag (P.BuildDep "libgc-dev") >>=
             flag (P.CabalDebian ["--default-package", "idris"]) >>=
             debianize [] >>=
             skip (Reason "Unmet build dependencies: libghc-optparse-applicative-dev (<< 0.12)")
  -- incremental_sat_solver = pure $ P.Package { P.spec = DebDir (Hackage (Just "0.2.0") "incremental-sat-solver") (Darcs ("http://src.seereason.com/haskell-incremental-sat-solver-debian")) , P.flags = [] }
  _incremental_sat_solver <-  (git "https://github.com/seereason/incremental-sat-solver" []) >>= debianize []
  _indents <-  (hackage (Just "0.3.3") "indents") >>= debianize []
  _instant_generics <- hackage (Just "0.6") "instant-generics" >>= flag (P.SkipVersion "0.3.7") >>= debianize [] >>= broken
  _integer_logarithms <- hackage (Just "1.0.1") "integer-logarithms" >>= debianize [] >>= ghcjs_also
  _intervals <-  (hackage (Just "0.7.2") "intervals") >>= debianize []
  _ioRefCAS <- (hackage (Just "0.2.0.1") "IORefCAS") >>= debianize [] >>= skip (Reason "Version 0.2.0.1 build fails")
  _io_storage <- hackage (Just "0.3") "io-storage" >>= flag (P.DebVersion "0.3-9") >>= debianize []
  -- _io_streams <- git "https://github.com/snapframework/io-streams" [] >>= debianize [] >>= inGroups ["important"] -- pull request to allow atto-parsec-0.13
  _io_streams <- hackage (Just "1.3.6.1") "io-streams" >>= debianize [] >>= patch $(embedFile "patches/io-streams.diff") >>= inGroups ["important"] -- http-streams-0.8.4.0 requires io-streams < 1.4
  _iproute <-  (hackage (Just "1.7.0") "iproute") >>= flag (P.DebVersion "1.7.0-1") >>= debianize []
  _ircbot <- hackage (Just "0.6.5.1") "ircbot" >>= debianize [] >>= inGroups ["happstack", "important"]
  _irc <- hackage (Just "0.6.1.0") "irc" >>= flag (P.DebVersion "0.6.1.0-5build1") >>= debianize [] >>= inGroups ["important"]
  _iso3166_country_codes <-  (hackage (Just "0.20140203.7") "iso3166-country-codes") >>= debianize []
  _ixset <-  (git "https://github.com/Happstack/ixset.git" []) >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also -- ,  (hackage (Just "1.0.7") "ixset") >>= debianize []
  _ixset_typed <-  (hackage (Just "0.3.1") "ixset-typed") >>= debianize [] >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _jmacro <-  (hackage (Just "0.6.14") "jmacro") >>= debianize [] >>= inGroups ["happstack", "lens", "th-path", "important"] >>= ghcjs_also
  _jmacro_rpc <- hackage (Just "0.3.2") "jmacro-rpc" >>= inGroups ["happstack", "important"] >>= debianize [] >>= broken
  _jmacro_rpc_happstack <- hackage (Just "0.3.2") "jmacro-rpc-happstack" >>= flag (P.SkipVersion "0.2.1") >>= debianize [] >>= broken -- Really just waiting for jmacro-rpc
  -- _jquery <- apt "sid" "jquery" >>= skip (Reason "Missing dependency node-source-map") {- >>= patch $(embedFile "patches/jquery.diff") -} -- Revert to version 1.7.2+dfsg-3, version 1.7.2+dfsg-3.2 gives us a nearly empty jquery.min.js 
  _jquery <- apt "jessie" "jquery" >>= patch $(embedFile "patches/jquery.diff")
  _jquery_goodies <- apt "sid" "jquery-goodies"
                     -- >>= patch $(embedFile "patches/jquery-goodies.diff")
               -- We want to stick with jqueryui-1.8 for now, so create
               -- packages with the version number embedded in the name.
  _jqueryui18 <- darcs ("http://src.seereason.com/jqueryui18")
  _js_flot <-  (hackage (Just "0.8.3") "js-flot") >>= flag (P.DebVersion "0.8.3-4") >>= debianize [] >>= ghcjs_also
  _js_jquery <-  (hackage (Just "3.1.0") "js-jquery") >>= debianize [] >>= ghcjs_also
  _jsaddle <- hackage (Just "0.9.3.0") "jsaddle" {-git "https://github.com/ghcjs/jsaddle" []-} >>= debianize [] >>= ghcjs_also
  _json <-  (hackage (Just "0.9.1") "json") >>= flag (P.DebVersion "0.9.1-3build1") >>= debianize [] >>= inGroups ["seereason", "important"] >>= ghcjs_also -- darcs "haskell-json" (repo ++ "/haskell-json")
  _juicyPixels <- hackage (Just "3.2.8") "JuicyPixels" >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _jwt <-  (hackage (Just "0.7.2") "jwt") >>= debianize [] >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _kan_extensions <- hackage (Just "5.0.1") "kan-extensions" {->>= flag (P.CabalPin "4.2.3")-} >>= debianize [] >>= ghcjs_also
  _keys <-  (hackage (Just "3.11") "keys") >>= debianize []
  _language_css <-  (hackage Nothing "language-css" >>= flag (P.DebVersion "0.0.4.1-1~hackage1")) >>= debianize []
  _language_ecmascript <-  (hackage (Just "0.17.0.1") "language-ecmascript") >>= debianize [] >>= skip (Reason "relax data-default dependency")
  _language_haskell_extract <-  (hackage (Just "0.2.4") "language-haskell-extract") >>= flag (P.DebVersion "0.2.4-5") >>= debianize []
  _language_java <-  (hackage (Just "0.2.8") "language-java" >>= flag (P.BuildDep "alex")) >>= debianize []
  _language_javascript <-  (hackage (Just "0.6.0.8") "language-javascript"
                              >>= flag (P.BuildDep "happy")
                              >>= flag (P.BuildDep "alex")
                           ) >>= debianize []
  _largeword <-  (hackage (Just "1.2.5") "largeword") >>= debianize []
               -- No cabal file
               -- ,  (git "haskell-logic-hs" "https://github.com/smichal/hs-logic") >>= debianize []
           {-  , apt "wheezy" "haskell-leksah"
               , apt "wheezy" "haskell-leksah-server" -- for leksah -}
  _latex <-  (hackage (Just "0.1.0.3") "latex") >>= debianize []
  -- _lattices <- hackage (Just "1.6.0") "lattices" >>= debianize []
  -- This commit adds the sequence numbers we need to generated function parameters
  _lens <- hackage (Just "4.15.1") "lens" >>=
           -- git "https://github.com/ekmett/lens" [Commit "950eb5be34fb40bf0111ded6bc91c1ffcd2a786b"] >>=
           apply (replacement "lens" "microlens-compat") >>=
           debianize [] >>= inGroups ["lens", "important"] >>= ghcjs_also
  -- _lens <-  (hackage (Just "4.14") "lens") >>= debianize [] >>= ghcjs_also
  _lens_compat <-  (git "https://github.com/ddssff/lens-compat" []) >>= debianize [] >>= inGroups ["lens"] >>= skip (Reason "Module ‘Control.Lens.Internal.Zoom’ does not export ‘Zoomed’")
  _lens_family_core <-  (hackage (Just "1.2.1") "lens-family-core") >>= debianize []
  _lens_family <-  (hackage (Just "1.2.1") "lens-family") >>= debianize []
  _lens_family_th <-  (hackage (Just "0.4.1.0") "lens-family-th") >>= debianize [] >>= skip (Reason "Encountered missing dependencies: 2> base ==4.9.*, template-haskell ==2.11.*")
      -- These five fail because representable-functors fails, it wasn't updated
      -- for the consolidation of comonad
      {-
      ,  (hackage (Just "3.2.0.2") "representable-functors" {- >>= patch $(embedFile "patches/representable-functors.diff") -}) >>= debianize []
      ,  (hackage (Just "3.0.2") "representable-tries") >>= debianize []
      ,  (hackage (Just "3.1") "algebra") >>= debianize []
      ,  (hackage (Just "0.4.0.4") "universe" {- >>= patch $(embedFile "patches/universe.diff") -}) >>= debianize []
      -}
{-
  _libjs_jcrop <- apt "trusty" "libjs-jcrop" >>= \p ->
                 (baseRelease . view release <$> get) >>= \ r ->
                 case r of
                   Precise -> proc p
                   _ -> skip (Reason "Trusty has libjs-jcrop") p
-}
           {-
               , P.Package { P.spec = DebDir (Uri ("http://src.seereason.com/jcrop/Jcrop.tar.gz") "028feeb9b6415af3b7fd7d9471c92469") (Darcs ("http://src.seereason.com/jcrop-debian"))
                           , P.flags = [] }
           -}
  _libv8 <- apt "sid" "libv8-3.14" >>= skip (Reason "Use standard")
  _lifted_async <-  (hackage (Just "0.9.0") "lifted-async") >>= debianize [] >>= inGroups ["ghcjs-comp"]
  _lifted_base <-  (hackage (Just "0.2.3.8") "lifted-base") >>= debianize [] >>= ghcjs_also
  _linear <-  (hackage (Just "1.20.2") "linear") >>= debianize [] >>= skip (Reason "Requires bytes")
  _list_extras <-  (hackage (Just "0.4.1.4") "list-extras") >>= debianize []
  _listLike <- git "https://github.com/JohnLato/ListLike" [] >>= flag (P.CabalDebian ["--cabal-flags", "safe"]) >>= debianize [] >>= inGroups ["pretty", "autobuilder-group"] >>= ghcjs_also
  _list_tries <-  (hackage (Just "0.6.3") "list-tries" {- >>= patch $(embedFile "patches/list-tries.diff") -}) >>= debianize [] >>= inGroups ["happstack", "important"] -- version 0.5.2 depends on dlist << 0.7
  _loch_th <-  (hackage (Just "0.2.1") "loch-th") >>= debianize []
  _logging <- hackage (Just "3.0.4") "logging" >>= debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _logic_classes <-  (git "https://github.com/seereason/logic-classes" []) >>= debianize [] >>= inGroups ["seereason", "important"]
  _logic_TPTP <-  (hackage (Just "0.4.4.0") "logic-TPTP") >>= debianize []
                 >>= patch $(embedFile "patches/logic-TPTP.diff")
                 >>= flag (P.BuildDep "alex")
                 >>= flag (P.BuildDep "happy")
  -- logic_TPTP = pure $ P.Package { P.spec = Debianize'' (Patch (Hackage (Just "0.4.4.0") "logic-TPTP") $(embedFile "patches/logic-TPTP.diff")) Nothing, P._flags = [ P.BuildDep "alex", P.BuildDep "happy" ] }
               -- , apt "sid" "haskell-maybet"
  _logict <- createPackage (Debianize'' (Hackage "logict") Nothing) mempty [] >>= flag (P.DebVersion "0.6.0.2-5") >>= ghcjs_also
  _loop <- hackage (Just "0.3.0") "loop" >>= debianize [] >>= ghcjs_also
  _lucid <-  (hackage (Just "2.9.5") "lucid") >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _maccatcher <-  (hackage (Just "2.1.5") "maccatcher"
                              >>= pflag (P.DebVersion "2.1.5-3")
                              >>= tflag (P.DebVersion "2.1.5-5build1")) >>= debianize [] >>= ghcjs_also
  _machines <- hackage Nothing "machines" >>= debianize [] >>= ghcjs_also
  _magic <- hackage (Just "1.1") "magic" >>= flag (P.DebVersion "1.1-4") >>= flag (P.DevelDep "libmagic-dev") >>= debianize []
           {-  , P.Package { P._spec = Quilt (Apt "wheezy" "magic-haskell") (Darcs ("http://src.seereason.com/magic-quilt"))
                           , P._flags = mempty } -}
  _mainland_pretty <-  (hackage (Just "0.4.1.4") "mainland-pretty") >>= debianize []
  _makedev <- apt "wheezy" "makedev" >>= skip (Reason "Use standard")
  _markdown <-  (hackage (Just "0.1.14") "markdown" {- >>= patch $(embedFile "patches/markdown.diff") -}) >>= debianize [] >>= inGroups ["happstack", "important"]
  _markdown_unlit <-  (hackage (Just "0.4.0") "markdown-unlit" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize []
  _matrix <-  (hackage (Just "0.3.5.0") "matrix") >>= debianize [] >>= ghcjs_also
               -- ,  (hackage (Just "0.3") "hlatex") >>= debianize []
  _maybeT <-  (hackage (Just "1.2") "MaybeT" >>= flag (P.DebVersion "1.2-6")) >>= debianize [] >>= skip (Reason "Could not deduce (Applicative (MaybeT m))")
  _memoize <-  (hackage (Just "0.8.1") "memoize") >>= debianize []
  _memory <-  (hackage (Just "0.13") "memory") >>= debianize [] >>= ghcjs_also
  _mime <- git ("https://github.com/seereason/haskell-mime") [] >>= debianize [] >>= inGroups ["autobuilder-group"]
  _mime_mail <-  (git "https://github.com/snoyberg/mime-mail.git" [] >>= cd "mime-mail") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _mime_types <-  (hackage (Just "0.1.0.7") "mime-types") >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _mirror <-  (git "https://github.com/seereason/mirror" []
                        >>= flag (P.CabalDebian ["--executable", "debian-mirror"])) >>= debianize [] >>= inGroups ["autobuilder-group", "important"]
  _missingH <-  (hackage (Just "1.4.0.1") "MissingH") >>= debianize []
  _mmap <-  (hackage (Just "0.5.9") "mmap") >>= flag (P.DebVersion "0.5.9-3") >>= debianize []
  _mmorph <-  (hackage (Just "1.0.6") "mmorph") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _module_management <-  (git "https://github.com/seereason/module-management" [] >>= flag (P.BuildDep "rsync")) >>= debianize []
  _monadCatchIO_mtl <-  (hackage (Just "0.3.1.0") "MonadCatchIO-mtl" >>= patch $(embedFile "patches/monadcatchio-mtl.diff")) >>= debianize []
  _monadCatchIO_transformers <- hackage (Just "0.3.1.3") "MonadCatchIO-transformers" >>=
                                flag (P.DebVersion "0.3.1.3-4") >>=
                                patch $(embedFile "patches/MonadCatchIO-transformers.diff") >>=
                                debianize []
  _monad_control <-  (hackage (Just "1.0.1.0") "monad-control") >>= debianize [] >>= ghcjs_also
  _monadcryptorandom <- hackage (Just "0.7.0") "monadcryptorandom" >>= flag (P.DebVersion "0.7.0-1") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _monadLib <-  (hackage (Just "3.7.3") "monadLib") >>= flag (P.DebVersion "3.7.3-1") >>= debianize []
               -- Putting this in our repo can cause problems, because when it is
               -- installed some packages can't compile unless you add package
               -- qualifiers to their imports.  For this reason, when we run the
               -- autobuilder with the --lax flag we usually get a failure from
               -- some package that builds after monads-tf got installed.  On the
               -- other hand, without monads-tf we lose this dependency chain:
               -- monads-tf <- options <- fay.
  _monadlist <-  (hackage (Just "0.0.2") "monadlist") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _monad_logger <-  (hackage (Just "0.3.24") "monad-logger") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _monad_loops <- hackage (Just "0.4.3") "monad-loops" >>= flag (P.DebVersion "0.4.3-3") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _monad_parallel <-  (hackage (Just "0.7.2.2") "monad-parallel") >>= debianize []
  _monad_par <-  (hackage (Just "0.3.4.8") "monad-par") >>= debianize []
  _monad_par_extras <-  (hackage (Just "0.3.3") "monad-par-extras") >>= flag (P.DebVersion "0.3.3-5") >>= debianize []
  _monadRandom <-  (hackage (Just "0.4.2.3") "MonadRandom") >>= debianize []
  _monads_tf <-  (hackage (Just "0.1.0.3") "monads-tf") >>= debianize []
  _monad_task <- hackage (Just "0.1.0") "monad-task" >>= debianize [] >>= skip (Reason "0.1.0 requires transformers<4")
  _mono_traversable <- hackage (Just "1.0.0.1") "mono-traversable" >>= debianize [] >>= ghcjs_also
  _monoid_transformer <-  (hackage (Just "0.0.3") "monoid-transformer") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-monoid-transformer"
  _mtl <- hackage (Just "2.2.1") "mtl" >>= flag (P.DebVersion "2.2.1-2") >>= debianize [] >>= inGroups ["platform"]
  _mtl_compat <-  (hackage (Just "0.2.1.3") "mtl-compat") >>= debianize [] >>= skip (Reason "build failure")
  _mtl_unleashed <-  (git "https://github.com/seereason/mtl-unleashed" []) >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _mtlparse <-  (hackage (Just "0.1.4.0") "mtlparse") >>= flag (P.DebVersion "0.1.4.0-4") >>= debianize []
  _multimap <-  (hackage (Just "1.2.1") "multimap") >>= debianize []
  _multipart <-  (hackage (Just "0.1.2") "multipart") >>= flag (P.DebVersion "0.1.2-3build1") >>= debianize [] >>= inGroups ["platform"]
  _multiset <-  (hackage (Just "0.3.3") "multiset") >>= debianize []
  _murmur_hash <-  (hackage (Just "0.1.0.9") "murmur-hash") >>= debianize []
  _mwc_random <-  (hackage (Just "0.13.4.0") "mwc-random") >>= debianize [] >>= ghcjs_also
  _mysql <- hackage (Just "0.1.1.8") "mysql" >>= flag (P.BuildDep "libmysqlclient-dev") >>= debianize [] >>= skip (Reason "dependencies")
  _mysql_simple <- hackage (Just "0.2.2.5") "mysql-simple" >>= flag (P.BuildDep "libmysqlclient-dev") >>= debianize [] >>= skip (Reason "dependencies")
  _nano_hmac <- hackage Nothing "nano-hmac" >>= patch $(embedFile "patches/nano-hmac.diff") >>= flag (P.DebVersion "0.2.0ubuntu1") >>= debianize []
  _nanospec <-  (hackage (Just "0.2.1") "nanospec" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize [] -- avoid circular dependency nanospec <-> silently
  -- Empty as of ghc-7.10
  _nats <-  (hackage (Just "1.1.1") "nats") >>= debianize [] >>= flag P.NoDoc >>= ghcjs_also
  -- Deprecated in favor if conduit-extra
  -- _network_conduit <- hackage (Just "1.1.0") "network-conduit" >>= debianize []
  _network <- hackage (Just "2.6.3.1") "network" >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _network_info <-  (hackage (Just "0.2.0.8") "network-info") >>= flag (P.DebVersion "0.2.0.8-1") >>= debianize [] >>= ghcjs_also
  _network_uri <-  (hackage (Just "2.6.1.0") "network-uri") >>= debianize [] >>= ghcjs_also
  _newtype_generics <-  (hackage (Just "0.5") "newtype-generics") >>= debianize [] >>= inGroups ["autobuilder-group"]
  -- _nodejs <- uri "https://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz" "5523ec4347d7fe6b0f6dda1d1c7799d5" >>=
  --            debdir (Git "https://github.com/seereason/nodejs-debian" []) >>= inGroups ["ghcjs-comp"]
  _nodejs <- uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5.orig.tar.gz" "a2a820b797fb69ffb259b479c7f5df32" >>=
            debdir (Uri "https://deb.nodesource.com/node_6.x/pool/main/n/nodejs/nodejs_6.9.5-1nodesource1~xenial1.debian.tar.xz" "0083c158831134295e719a524d9c8513") >>=
            flag (P.RelaxDep "libssl-dev") >>=
            inGroups ["ghcjs-comp"]
  _numeric_extras <-  (hackage (Just "0.1") "numeric-extras") >>= flag (P.DebVersion "0.1-1") >>= debianize []
  _numInstances <-  (hackage (Just "1.4") "NumInstances") >>= flag (P.DebVersion "1.4-4") >>= debianize []
  _objectName <-  (hackage (Just "1.1.0.1") "ObjectName") >>= flag (P.DebVersion "1.1.0.1-1") >>= debianize []
  _old_locale <- hackage (Just "1.0.0.7") "old-locale" >>= flag (P.DebVersion "1.0.0.7-2") >>= debianize []
  _old_time <- hackage (Just "1.1.0.3") "old-time" >>= flag (P.DebVersion "1.1.0.3-2") >>= debianize []
  _oo_prototypes <-  (hackage (Just "0.1.0.0") "oo-prototypes") >>= flag (P.DebVersion "0.1.0.0-3") >>= debianize []
  _openGL <-  (hackage (Just "2.13.1.0") "OpenGL" >>= inGroups ["gl"] >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize [] >>= skip (Reason "too old for openglraw")
  _openGLRaw <-  (hackage (Just "3.2.1.0") "OpenGLRaw" >>= inGroups ["gl"]
                     >>= flag (P.DevelDep "libgl1-mesa-dev")) >>= debianize []
  _openid <-  (hackage (Just "0.2.0.2") "openid" >>= patch $(embedFile "patches/openid.diff")) >>= debianize [] >>= skip (Reason "No instance for (Applicative (Assoc m))")
           {-  , P.Package { P.spec =  (Patch (Hackage (Just "0.2.0.2") "openid") $(embedFile "patches/openid-ghc76.diff")) >>= Debianize []
                           , P.flags = [] } -}
  _openssl_streams <-  (hackage (Just "1.2.1.1") "openssl-streams") >>= debianize [] >>= inGroups ["important", "platform"]
  _operational <- hackage (Just "0.2.3.3") "operational" >>= flag P.OmitLTDeps >>= debianize []
  _optparse_applicative <-  (hackage (Just "0.12.1.0") "optparse-applicative") >>= debianize [] >>= ghcjs_also
  _ordered <-  (hackage (Just "0.1") "ordered") >>= debianize []
  _pandoc <- hackage (Just "1.19.1") "pandoc" >>=
             patch $(embedFile "patches/pandoc.diff") >>=
             flag (P.BuildDep "alex") >>=
             flag (P.BuildDep "happy") >>=
             debianize [] >>=
             inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _pandoc_types <- hackage (Just "1.17.0.3") "pandoc-types" >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _pango <-  (hackage (Just "0.13.1.1") "pango") >>= debianize [] >>= skip (Reason "see cairo")
  _parallel <- hackage (Just "3.2.1.0") "parallel" >>= flag (P.DebVersion "3.2.1.0-1") >>= debianize [] >>= inGroups ["platform"]
  _parseargs <-  (hackage (Just "0.2.0.7") "parseargs") >>= debianize [] >>= ghcjs_also
               -- , apt (rel release "wheezy" "quantal") "haskell-parsec2" >>= patch $(embedFile "patches/parsec2.diff")
  _parsec <-  (hackage (Just "3.1.11") "parsec" >>= apply (substitute "parsec2" "parsec3")) >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _parse_dimacs <-  (hackage (Just "1.3") "parse-dimacs") >>= debianize []
  _parsers <- hackage (Just "0.12.4") "parsers" >>= debianize []
  _pbkdf2 <-  (hackage (Just "0.3.1.5") "PBKDF2") >>= debianize [] >>= skip (Reason "[libghc-multiset-dev (<< 0.3)] -> []")
               -- , apt (rel release "wheezy" "quantal") "haskell-pcre-light"
  _pcre_light <- hackage (Just "0.4.0.4") "pcre-light" >>=
                 -- Tell it that build tool libpcre means deb libpcre3-dev, and tell
                 -- it to install libpcre3-dev.
                 flag (P.CabalDebian ["--exec-map", "libpcre:libpcre3-dev"]) >>=
                 flag (P.DevelDep "libpcre3-dev") >>=
                 flag (P.DebVersion "0.4.0.4-1") >>= debianize [] >>=
                 ghcjs_also
  _pem <-  (hackage (Just "0.2.2") "pem") >>= flag (P.DebVersion "0.2.2-5") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _permutation <-  (hackage (Just "0.5.0.5") "permutation") >>= debianize []
  _pipes <-  (hackage (Just "4.2.0") "pipes") >>= debianize []
  _placeholders <-  (hackage (Just "0.1") "placeholders") >>= debianize []
  _plugins_auto <-  (hackage (Just "0.0.4") "plugins-auto" >>= patch $(embedFile "patches/plugins-auto.diff")) >>= debianize [] >>= skip (Reason "Couldn't match expected type ‘Int#’ with actual type ‘Int’")
  _plugins <- git "https://github.com/stepcut/plugins" [] >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>= debianize [] >>= skip (Reason "obsolete")
  _plugins_ng <-  (git "https://github.com/ddssff/plugins-ng" []) >>= debianize [] >>= skip (Reason "needs fsnotify << 0.2")
  _po4a <- apt "wheezy" "po4a" >>= skip (Reason "use standard trusty version")
  _pointed <- git "https://github.com/ekmett/pointed" [] >>= debianize [] >>= ghcjs_also
  _pointedlist <-  (hackage (Just "0.6.1") "pointedlist") >>= flag (P.DebVersion "0.6.1-4") >>= debianize []
  _polyparse <-  (hackage (Just "1.12") "polyparse") >>= debianize [] >>= ghcjs_also
  _prelude_extras <-  (hackage (Just "0.4.0.3") "prelude-extras") >>= debianize [] >>= ghcjs_also
  -- We can't put upgraded versions of pretty in the repo because the template haskell
  -- version (which is bundled with ghc) conflicts, in particular via th-typegraph.
  -- _pretty <- git "https://github.com/ddssff/pretty" [] >>= debianize [] >>= inGroups ["pretty"] >>= ghcjs_also
  _pretty_show <- hackage (Just "1.6.12") "pretty-show" >>= flag (P.BuildDep "happy") >>= debianize []
  _primitive <-
      -- 0.6.1.0 depends on base<4.9, ghc-prim<0.5, transformers<0.5, so for ghc8 we probably need 0.6.2.0
      -- hackage (Just "0.6.1.0") "primitive" >>=
      hackage (Just "0.6.2.0") "primitive" >>=
      debianize []
  _process_extras <-
       (git "https://github.com/seereason/process-extras" []) >>= debianize []
                   >>= apply (substitute "process-extras" "process-listlike")
                   >>= inGroups ["autobuilder-group"] >>= ghcjs_also
  _processing <-  (hackage (Just "1.2.0.2") "processing") >>= debianize [] >>= skip (Reason "[libghc-multiset-prof (<< 0.3)] -> []")
  _profunctors <-  (hackage (Just "5.2") "profunctors"
                     >>= apply (replacement "profunctors" "profunctors-extras")) >>= debianize []
                     >>= ghcjs_also
  _propLogic <-  (git "https://github.com/ddssff/PropLogic" []) >>= debianize []
  _pseudomacros <-  (hackage (Just "0.0.2") "pseudomacros") >>= debianize []
  _psQueue <- hackage (Just "1.1") "PSQueue" >>= flag (P.DebVersion "1.1-8") >>= debianize [] >>= wskip
  _psqueues <- hackage (Just "0.2.2.2") "psqueues" >>= debianize []
  _publicsuffixlist <- hackage (Just "0.1") "publicsuffixlist" >>= flag (P.DebVersion "0.1-7build1") >>= debianize [] >>= inGroups ["platform"]
  _pureMD5 <- hackage (Just "2.1.3") "pureMD5" >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _pwstore_purehaskell <-  (hackage (Just "2.1.4") "pwstore-purehaskell"
                              >>= flag (P.SkipVersion "2.1.2")
                              -- >>= patch $(embedFile "patches/pwstore-purehaskell.diff")
                              -- >>= flag (P.DebVersion "2.1-1~hackage1")
                           ) >>= debianize []
               -- Retired
               -- , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
  _quickCheck <- hackage (Just "2.9.2") "QuickCheck" >>= flag (P.BuildDep "libghc-random-prof") {->>= flag (P.CabalDebian ["--no-tests"])-} >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _quickcheck_gent <-  (hackage Nothing "QuickCheck-GenT") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-quickcheck2-dev (<< 2.7) libghc-quickcheck2-prof (<< 2.7)")
  -- _quickcheck_instances <-  (hackage (Just "0.3.12") "quickcheck-instances") >>= debianize []
  _quickcheck_io <-  (hackage (Just "0.1.3") "quickcheck-io") >>= debianize [] >>= ghcjs_also
  -- quickCheck1 =  (hackage "QuickCheck" >>= flag (P.CabalPin "1.2.0.1") >>= flag (P.DebVersion "1.2.0.1-2") >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize []
  _random <- hackage (Just "1.1") "random" >>= flag (P.DebVersion "1.1-3") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also -- 1.1.0.3 fixes the build for ghc-7.4.2 / base < 4.6
  _reducers <- hack (Just "3.12.1") "reducers" >>= flag (P.DebVersion "3.12.1-1build1")
  _ref_tf <- hackage (Just "0.4.0.1") "ref-tf" >>= debianize [] >>= ghcjs_also
  _reflection <-  (hackage (Just "2.1.2") "reflection") >>= debianize [] >>= ghcjs_also -- avoid rebuild
  _reform_blaze <- git "https://github.com/Happstack/reform-blaze.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform <- git "https://github.com/Happstack/reform.git" [] >>= flag (P.DebVersion "0.2.7.1-1build1") >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform_hamlet <- git "https://github.com/Happstack/reform-hamlet.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform_happstack <- git "https://github.com/Happstack/reform-happstack.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform_hsp <- git "https://github.com/Happstack/reform-hsp.git" [] >>= flag (P.BuildDep "hsx2hs") >>= debianize [] >>= inGroups ["happstack", "important"]
  _regex_applicative <-  (hackage (Just "0.3.3") "regex-applicative") >>= flag (P.DebVersion "0.3.3-1") >>= debianize []
  _regex_base <- hackage (Just "0.93.2") "regex-base" >>= flag (P.DebVersion "0.93.2-8") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _regex_compat <- hackage (Just "0.95.1") "regex-compat" >>= flag (P.DebVersion "0.95.1-8") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _regex_compat_tdfa <- hackage (Just "0.95.1.4") "regex-compat-tdfa" >>= flag (P.DebVersion "0.95.1.4-3build2") >>= debianize []
  _regex_pcre_builtin <-  (hackage (Just "0.94.4.8.8.35") "regex-pcre-builtin"
                              -- Need to email Audrey Tang <audreyt@audreyt.org> about this.
                              >>= patch $(embedFile "patches/regex-pcre-builtin.diff")
                              >>= flag (P.DevelDep "libpcre3-dev")) >>= debianize [] >>= ghcjs_also
  _regex_posix <- hackage (Just "0.95.2") "regex-posix" >>= flag (P.DebVersion "0.95.2-7") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _regexpr <- hackage (Just "0.5.4") "regexpr" >>= flag (P.DebVersion "0.5.4-9build1") >>= debianize []
  _regex_tdfa <-  (hackage (Just "1.2.2") "regex-tdfa"
                          -- Although it might be nice to start using regex-tdfa-rc everywhere
                          -- we are using regex-tdfa, the cabal package names are different so
                          -- packages can't automatically start using regex-tdfa-rc.
                          >>= apply (substitute "regex-tdfa" "regex-tdfa-rc")) >>= debianize [] >>= ghcjs_also
  _regex_tdfa_rc <-  (hackage (Just "1.1.8.3") "regex-tdfa-rc"
                              >>= apply (substitute "regex-tdfa-rc" "regex-tdfa")) >>= debianize [] >>= ghcjs_also
  _regex_tdfa_text <- hackage (Just "1.0.0.3") "regex-tdfa-text" >>= debianize [] >>= ghcjs_also
  -- reified_records =  (hackage (Just "0.2.2") "reified-records" >>= patch $(embedFile "patches/reified-records.diff")) >>= debianize []
  _reified_records <-  (hg "https://bitbucket.org/ddssff/reified-records") >>= debianize []
  _resource_pool <- hackage (Just "0.2.3.2") "resource-pool" >>= flag (P.DebVersion "0.2.3.2-4build1") >>= debianize []
  _resourcet <-  (hackage (Just "1.1.7.4") "resourcet") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _rJson <-  hackage (Just "0.3.7") "RJson" >>=
             patch $(embedFile "patches/RJson.diff") >>=
             wflag (P.DebVersion "0.3.7-1~hackage1") >>=
             debianize [] >>=
             skip (Reason "Ambiguous occurrence ‘escape’")
  _rsa <-hackage (Just "2.2.0") "RSA" >>= flag (P.DebVersion "2.2.0-1") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _rss <-  (hackage (Just "3000.2.0.5") "rss" {- >>= patch $(embedFile "patches/rss.diff") -}) >>= debianize [] >>= skip (Reason "time dependency")
  _safecopy <- git "https://github.com/acid-state/safecopy" [] >>= debianize [] >>= ghcjs_also
  _safe <- hackage (Just "0.3.9") "safe" >>= flag (P.DebVersion "0.3.9-3") >>= debianize [] >>= ghcjs_also
  _safeSemaphore <-  (hackage (Just "0.10.1") "SafeSemaphore") >>= flag (P.DebVersion "0.10.1-5build1") >>= debianize [] >>= inGroups ["happstack", "important"]
  _sandi <-  (hackage (Just "0.4.0") "sandi") >>= debianize [] -- replaces dataenc
  _sat <-  (hackage (Just "1.1.1") "sat"
                              >>= patch $(embedFile "patches/sat.diff")
                              >>= flag (P.DebVersion "1.1.1-1~hackage1")) >>= debianize []
  _scientific <- hackage (Just "0.3.4.11") "scientific" >>= debianize [] >>= ghcjs_also
               -- ,  (hackage (Just "0.4.1.1") "arithmoi" >>= flag (P.BuildDep "llvm-dev")) >>= debianize []
  _scotty <- hackage (Just "0.10.2") "scotty" {- >>= patch $(embedFile "patches/scotty.diff") -} >>= debianize [] >>= skip (Reason "data-default dependency")
  _seclib <-  (darcs ("http://src.seereason.com/seclib")) >>= debianize [] >>= skip (Reason "No instance for (Applicative (Sec s))")
  _securemem <-  (hackage (Just "0.1.9") "securemem") >>= flag (P.DebVersion "0.1.9-3build1") >>= debianize []
  _seereason_base <- git "https://github.com/seereason/seereason-base" [] >>= debianize [] >>= inGroups ["seereason", "important"]
  _seereason_keyring <- darcs ("http://src.seereason.com/seereason-keyring") >>= flag (P.UDeb "seereason-keyring-udeb")
  _seereason_ports <-  (git "https://github.com/seereason/seereason-ports" []) >>= debianize []
  _semigroupoids <-  (hackage (Just "5.1") "semigroupoids"
                     >>= apply (replacement "semigroupoids" "semigroupoid-extras")) >>= debianize [] >>= ghcjs_also
  _semigroups <-  (hackage (Just "0.18.2") "semigroups") >>= debianize [] >>= inGroups ["important"] >>= flag (P.CabalPin "0.18.2") >>= ghcjs_also -- avoid rebuilds
  _sendfile <- hackage (Just "0.7.9") "sendfile" >>= flag (P.DebVersion "0.7.9-6") >>= debianize [] >>= ghcjs_also
  _servant <- hackage (Just "0.8") "servant" >>= debianize [] >>= inGroups ["servant"]
  _servant_server <- hackage (Just "0.8") "servant-server" >>= debianize [] >>= inGroups ["servant-server"] >>= inGroups ["servant"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _setenv <- hackage (Just "0.1.1.3") "setenv" >>= flag (P.DebVersion "0.1.1.3-4") >>= debianize [] >>= ghcjs_also
  _set_extra <-  (darcs ("http://src.seereason.com/set-extra")) >>= flag (P.DebVersion "1.3.2-2build1") >>= debianize [] >>= ghcjs_also
               -- I don't think we use this any more
               -- ,  (darcs "haskell-old-exception" ("http://src.seereason.com/old-exception")) >>= debianize []
  _set_monad <-  (hackage (Just "0.2.0.0") "set-monad") >>= debianize []
  _sha <- hackage (Just "1.6.4.2") "SHA" >>= flag (P.DebVersion "1.6.4.2-4") >>= debianize [] >>= ghcjs_also -- apt (rel release "wheezy" "quantal") "haskell-sha"
  _shake <-  (hackage (Just "0.15.10") "shake") >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "dependencies")
  _shakespeare <-  (hackage (Just "2.0.10") "shakespeare") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _shakespeare_js <-  (hackage (Just "1.3.0") "shakespeare-js") >>= debianize [] >>= skip (Reason "No input files to haddock?")
  _shellmate <- hack (Just "0.3.3") "shellmate" >>= skip (Reason "directory dependency")
  _shelly <- hackage (Just "1.6.8.4") "shelly" >>= debianize [] >>= inGroups ["ghcjs-comp"]
  _show_please <- hackage Nothing "show-please" >>= debianize [] >>= ghcjs_also
  _silently <-  (hackage (Just "1.2.5") "silently") >>= flag (P.DebVersion "1.2.5-3") >>= debianize []
  _simple_reflect <-  (hackage (Just "0.3.2") "simple-reflect") >>= flag (P.DebVersion "0.3.2-5") >>= debianize []
  _simple_sendfile <-  (hackage (Just "0.2.25") "simple-sendfile") >>= debianize []
  _singletons <- hackage (Just "2.1") "singletons" >>= flag (P.CabalPin "2.1") >>= debianize [] -- 2.2 requires base-4.9
  _smallcheck <-  (hackage (Just "1.1.1") "smallcheck") >>= flag (P.DebVersion "1.1.1-5") >>= debianize [] >>= ghcjs_also
  _smtpClient <-  (hackage (Just "1.1.0") "SMTPClient") >>= debianize []
  _snap_core <- hackage (Just "0.9.5.0") "snap-core" >>= debianize [] >>= skip (Reason "glib")
  _snap_server <- hackage (Just "0.9.3.4") "snap-server" >>= debianize [] >>= skip (Reason "snap-core, glib")
  _socks <-  (hackage (Just "0.5.5") "socks") >>= debianize [] >>= ghcjs_also
  _sodium <- hackage (Just "0.11.0.3") "sodium" >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "dependencies")
  _sourcemap <-  (hackage (Just "0.1.6") "sourcemap") >>= debianize [] >>= inGroups ["happstack", "important"]
  _spine <-  (hackage (Just "0.1") "spine") >>= debianize []
  _split <- hackage (Just "0.2.3.1") "split" >>= {-patch $(embedFile "patches/split.diff") >>= tflag (P.DebVersion "0.2.2-1") >>=-} debianize [] >>= ghcjs_also
  _spoon <-  hackage (Just "0.3.1") "spoon" >>= debianize []
  _srcloc <-  (hackage (Just "0.5.1.0") "srcloc") >>= debianize []
  -- _stack <- hackage Nothing "stack" >>= debianize []
  _stateVar <-  (hackage (Just "1.1.0.4") "StateVar") >>= debianize [] >>= ghcjs_also
  _stb_image <-  (hackage (Just "0.2.1") "stb-image") >>= debianize []
  _stm_chans <-  (hackage (Just "3.0.0.4") "stm-chans") >>= flag (P.DebVersion "3.0.0.4-3build1") >>= debianize [] >>= inGroups ["platform"]
  _stm <- hackage (Just "2.4.4.1") "stm" >>= flag (P.DebVersion "2.4.4.1-1") >>= debianize [] >>= inGroups ["platform"]
  _streaming_commons <-  (hackage (Just "0.1.15.5") "streaming-commons") >>= debianize [] >>= inGroups ["conduit", "important"] >>= ghcjs_also
  _strict <- hackage (Just "0.3.2") "strict" >>= flag (P.DebVersion "0.3.2-11") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-strict" -- for leksah
               -- ,  (hackage (Just "0.2.4.1") "strict-concurrency" >>= wflag (P.DebVersion "0.2.4.1-2")) >>= debianize []
  _strict_io <-  (hackage (Just "0.2.1") "strict-io") >>= debianize [] >>= inGroups ["GenI"] >>= skip (Reason "dependencies are missing: deepseq >=1.1 && <1.4")
  _stringable <-  (hackage (Just "0.1.3") "stringable") >>= debianize [] -- this can be done with listlike-instances
  _string_conversions <- hackage (Just "0.4") "string-conversions" >>= debianize [] >>= inGroups ["servant"]
  _stringbuilder <-  (hackage (Just "0.5.0") "stringbuilder") >>= flag (P.DebVersion "0.5.0-4") >>= debianize []
  _stringsearch <-  (hackage (Just "0.3.6.6") "stringsearch") >>= flag (P.DebVersion "0.3.6.6-4") >>= debianize []
  _sunroof_compiler <-  (git "http://github.com/ku-fpg/sunroof-compiler" [] >>= patch $(embedFile "patches/sunroof-compiler.diff")) >>= debianize [] >>= skip (Reason "Setup.hs:3:1: parse error on input ‘import’")
  _syb <- hackage (Just "0.6") "syb" >>= flag (P.DebVersion "0.6-1") >>= debianize [] >>= inGroups ["platform"] -- haskell-src-meta requres syb<0.6
  _syb_with_class <- git "http://github.com/Happstack/syb-with-class" [] >>= debianize [] >>= ghcjs_also
  _syb_with_class_instances_text <-
                  (hackage (Just "0.0.1") "syb-with-class-instances-text"
                              >>= pflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.SkipVersion "0.0.1-3")
                              >>= tflag (P.DebVersion "0.0.1-6build1")) >>= debianize [] >>= ghcjs_also >>= broken2
  _system_fileio <-  (hackage (Just "0.3.16.3") "system-fileio") >>= flag (P.DebVersion "0.3.16.3-3build1") >>= debianize []
  _system_filepath <-  (hackage (Just "0.4.13.4") "system-filepath") >>= flag (P.DebVersion "0.4.13.4-3build1") >>= debianize [] >>= ghcjs_also
               -- , P.Package { P.spec = Debianize'' (Patch (Hackage (Just "0.4.4.1") "xml-enumerator") $(embedFile "patches/xml-enumerator.diff")) Nothing , P.flags = [] }
  _tagged <-  (hackage (Just "0.8.5") "tagged") >>= debianize [] >>= ghcjs_also
  _tagshare <-  (hackage (Just "0.0") "tagshare") >>= debianize []
  _tagsoup <-  (hackage (Just "0.14") "tagsoup") >>= debianize [] >>= ghcjs_also
  _tagstream_conduit <- hackage (Just "0.5.5.3") "tagstream-conduit" >>= flag (P.DebVersion "0.5.5.3-5build1") >>= debianize [] >>= inGroups ["conduit", "authenticate", "important"]
  _tar <-  (hackage (Just "0.5.0.3") "tar" >>= flag (P.CabalDebian ["--cabal-flags", "-old-time"])) >>= debianize []
           {-  -- This is built into ghc-7.8.3
               ,  (hackage (Just "0.3.2.5") "terminfo"
                                        >>= flag (P.DevelDep "libncurses5-dev")
                                        >>= flag (P.DevelDep "libncursesw5-dev")) >>= debianize -}
  _tasty <-  (hackage (Just "0.11.0.3") "tasty") >>= debianize [] >>= ghcjs_also
  _tasty_hunit <- hackage (Just "0.9.2") "tasty-hunit" >>= flag (P.DebVersion "0.9.2-5build2") >>= debianize []
  _tasty_golden <- hackage (Just "2.3.1") "tasty-golden" >>= flag (P.DebVersion "2.3.1-1build2") >>= debianize []
  _tasty_quickcheck <- hackage (Just "0.8.4") "tasty-quickcheck" >>= flag (P.DebVersion "0.8.4-1build1") >>= debianize []
  _tasty_smallcheck <- hackage (Just "0.8.1") "tasty-smallcheck" >>= flag (P.DebVersion "0.8.1-1build1") >>= debianize [] >>= ghcjs_also
  _template_default <- hackage (Just "0.1.1") "template-default" >>= patch $(embedFile "patches/template-default.diff") >>= debianize [] >>= skip (Reason "Not in scope: data constructor ‘ClassP’")
  _temporary <-  (hackage (Just "1.2.0.4") "temporary") >>= debianize [] >>= ghcjs_also
  _tensor <- hackage (Just "1.0.0.1") "Tensor" >>= tflag (P.DebVersion "1.0.0.1-2") >>= debianize [] >>= broken
  _test_framework <- hackage (Just "0.8.1.1") "test-framework" >>= flag (P.DebVersion "0.8.1.1-4build1") >>= debianize [] >>= ghcjs_also
  _test_framework_hunit <-  (hackage (Just "0.3.0.2") "test-framework-hunit") >>= flag (P.DebVersion "0.3.0.2-1build1") >>= debianize [] >>= ghcjs_also
  _test_framework_quickcheck2 <- git "https://github.com/seereason/test-framework" [] >>= flag (P.DebVersion "0.3.0.3-6build1") >>= patch $(embedFile "patches/test-framework-quickcheck2.diff") >>= cd "quickcheck2" >>= debianize [] >>= ghcjs_also
  _test_framework_smallcheck <-  (hackage (Just "0.2") "test-framework-smallcheck") >>= debianize []
  _test_framework_th <- hackage (Just "0.2.4") "test-framework-th" >>= flag (P.DebVersion "0.2.4-7build1") >>= debianize []
  _testing_feat <- hackage (Just "0.4.0.3") "testing-feat" >>= {-patch $(embedFile "patches/testing-feat.diff") >>=-} debianize []
  _texmath <- hackage (Just "0.9") "texmath" >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _text_binary <- hackage (Just "0.2.1") "text-binary" >>= flag (P.DebVersion "0.2.1-1build1") >>= debianize [] >>= ghcjs_also
  _text <-  (hackage (Just "1.2.2.1") "text" >>= flag (P.CabalDebian ["--cabal-flags", "-integer-simple"]) >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize [] >>= inGroups ["platform", "test8"]
  _text_icu <-  (hackage (Just "0.7.0.1") "text-icu" >>= flag (P.DevelDep "libicu-dev")) >>= flag (P.DebVersion "0.7.0.1-4build1") >>= debianize []
  _text_show <-  (hackage (Just "3.3") "text-show") >>= debianize []
  _text_stream_decode <- hackage (Just "0.1.0.5") "text-stream-decode" >>= patch $(embedFile "patches/text-stream-decode.diff") >>= debianize [] >>= inGroups ["conduit", "important"] >>= skip (Reason "depends on older text")
  _tf_random <-  (hackage (Just "0.5") "tf-random") >>= flag (P.DebVersion "0.5-5") >>= debianize [] >>= inGroups ["platform"] >>= ghcjs_also
  _th_alpha <-  (git "https://github.com/ddssff/th-alpha.git" []) >>= debianize []
  _th_abstraction <- hackage Nothing "th-abstraction" >>= debianize []
  _th_context <-  (git "http://github.com/seereason/th-context" []) >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_desugar <- {-(git "http://github.com/goldfirere/th-desugar" [])-} hackage (Just "1.6") "th-desugar" >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_expand_syns <-  (hackage (Just "0.4.0.0") "th-expand-syns") >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  -- th_instance_reification =  (git "https://github.com/seereason/th-instance-reification.git" []) >>= debianize []
  _th_kinds_fork <-  (git "http://github.com/ddssff/th-kinds-fork" []) >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_lift <- hackage (Just "0.7.6") "th-lift" >>= debianize [] >>= ghcjs_also
  _th_lift_instances <-
      -- hackage (Just "0.1.10") "th-lift-instances" >>=
      git "https://github.com/ddssff/th-lift-instances" [] >>=
      debianize [] >>= inGroups ["important"] >>= ghcjs_also
  _th_orphans <-  (hackage Nothing "th-orphans") >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _th_typegraph <- git "http://github.com/seereason/th-typegraph" [] >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _threads <-  (hackage (Just "0.5.1.4") "threads") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _th_reify_many <-  (hackage (Just "0.1.6") "th-reify-many") >>= debianize [] >>= inGroups ["th-path", "important"] >>= ghcjs_also
  _time_compat <-  (hackage (Just "0.1.0.3") "time-compat") >>= flag (P.DebVersion "0.1.0.3-5") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _time_locale_compat <-  (hackage (Just "0.1.1.3") "time-locale-compat") >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _tinymce <- apt "wheezy" "tinymce"
  _tls <-  (hackage (Just "1.3.8") "tls") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
              -- tls-extra deprecated in favor of tls
              -- ,  (hackage (Just "0.6.6") "tls-extra" >>= patch $(embedFile "patches/tls-extra.diff")) >>= debianize []
  _transformers_base <- hackage (Just "0.4.4") "transformers-base" >>= flag (P.DebVersion "0.4.4-4build1") >>= debianize [] >>= ghcjs_also
  _transformers_compat <- hackage (Just "0.5.1.4") "transformers-compat" >>=
                          -- flag (P.CabalPin "0.4.0.4") >>= -- waiting for newer monad-control and monad-parallel
                          {-patch $(embedFile "patches/transformers-compat.diff") >>=-}
                          debianize [] >>= ghcjs_also
  _transformers_free <-  (hackage (Just "1.0.1") "transformers-free") >>= debianize []
  _traverse_with_class <- hackage (Just "0.2.0.4") "traverse-with-class" >>= debianize [] >>= inGroups ["happstack", "important"]
  _trifecta <-  (hackage (Just "1.5.2") "trifecta" {->>= patch $(embedFile "patches/trifecta.diff")-}) >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-comonad-dev (<< 5) libghc-comonad-prof (<< 5)")
  _tyb <- hackage (Just "0.2.3") "TYB" >>= debianize [] >>= skip (Reason "Needs update for current template-haskell")
  _type_eq <- hackage (Just "0.5") "type-eq" >>= flag (P.BuildDep "cpphs") >>= debianize [] >>= skip (Reason "dependencies")
  _uglymemo <-  (hackage (Just "0.1.0.1") "uglymemo") >>= debianize []
  _unbounded_delays <- hackage (Just "0.1.0.9") "unbounded-delays" >>= flag (P.DebVersion "0.1.0.9-4") >>= debianize [] >>= ghcjs_also
  _unexceptionalio <- hackage (Just "0.3.0") "unexceptionalio" >>= debianize [] >>= ghcjs_also
  _unicode_names <-  (git "https://github.com/seereason/unicode-names" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1")) >>= debianize []
  _unicode_properties <-  (git "https://github.com/seereason/unicode-properties" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1")) >>= debianize []
  _unification_fd <-  (hackage (Just "0.10.0.1") "unification-fd" >>= flag (P.SkipVersion "0.8.0")) >>= debianize []
  _union_find <-  (hackage (Just "0.2") "union-find") >>= debianize []
               -- ,  (hackage "Elm") >>= debianize []
               -- ,  (hackage "elm-server" {- >>= patch $(embedFile "patches/elm-server.diff") -}) >>= debianize []
  _uniplate <- hackage (Just "1.6.12") "uniplate" >>= flag (P.DebVersion "1.6.12-4build1") >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _units <-  (hackage (Just "2.4") "units") >>= debianize [] >>= skip (Reason "[libghc-singletons-prof (<< 2)] -> []")
  _units_parser <-  (hackage (Just "0.1.0.0") "units-parser") >>= debianize []
  _universe_base <- hackage Nothing "universe-base" >>= flag (P.DebVersion "1.0.2.1-1") >>= debianize []
  _universe_instances_base <- hackage Nothing "universe-instances-base" >>= debianize []
  _universe_reverse_instances <- hackage Nothing "universe-reverse-instances" >>= debianize []
  _unix_compat <-  (hackage (Just "0.4.2.0") "unix-compat") >>= debianize [] >>= ghcjs_also
  _unix_time <-  (hackage (Just "0.3.6") "unix-time" {->>= flag (P.CabalDebian ["--no-run-tests"])-}) >>= flag (P.DebVersion "0.3.6-1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also -- doctest assumes cabal build dir is dist
  _unixutils <- git "https://github.com/seereason/haskell-unixutils" [] >>= flag (P.DebVersion "1.54.1-2build2") >>= debianize [] >>= inGroups ["autobuilder-group", "important"] >>= ghcjs_also
  _unixutils_shadow <-  (hackage (Just "1.0.0") "Unixutils-shadow") >>= debianize []
  _unordered_containers <-  (hackage (Just "0.2.7.1") "unordered-containers") >>= debianize []
               -- Obsolete after ghc-6.10
               -- ,  (hackage "utf8-prelude" >>= flag (P.DebVersion "0.1.6-1~hackage1")) >>= debianize []
               -- The GHC in wheezy conflicts with libghc-containers-dev, so we can't build this.
               -- , wonly $  (hackage "containers") >>= debianize []
  _urlencoded <-  (hackage (Just "0.4.1") "urlencoded" {->>= patch $(embedFile "patches/urlencoded.diff")-}) >>= debianize []
  _userid <- git "https://github.com/Happstack/userid" [] >>= debianize [] >>= inGroups ["authenticate", "happstack", "important"] >>= ghcjs_also
  _utf8_light <-  (hackage (Just "0.4.2") "utf8-light") >>= flag (P.DebVersion "0.4.2-4") >>= debianize []
  _utf8_string <- hackage (Just "1.0.1.1") "utf8-string" >>=
                  flag (P.DebVersion "1.0.1.1-1") >>=
                  flag (P.RelaxDep "hscolour") >>=
                  flag (P.RelaxDep "cpphs") >>=
                  debianize [] >>= ghcjs_also
  _utility_ht <- hackage (Just "0.0.11") "utility-ht" >>= flag (P.DebVersion "0.0.11-1") >>= debianize [] >>= ghcjs_also
  _uuid <- hackage (Just "1.3.12") "uuid" >>= debianize [] >>= ghcjs_also
  _uuid_orphans <- git "https://github.com/seereason/uuid-orphans" [] >>= debianize [] >>= inGroups ["clckwrks", "important"] >>= ghcjs_also
  _uuid_types <-  (hackage (Just "1.0.3") "uuid-types") >>= debianize [] >>= ghcjs_also
  _vacuum <-  (hackage (Just "2.2.0.0") "vacuum" >>= flag (P.SkipVersion "2.1.0.1")) >>= debianize [] >>= skip (Reason "#error Unsupported GHC version in ClosureTypes.hs!")
  _validation <-  (hackage (Just "0.2.0") "Validation" >>= patch $(embedFile "patches/validation.diff")) >>= debianize []
  _value_supply <-  (hackage (Just "0.6") "value-supply") >>= debianize [] >>= ghcjs_also
  _vault <-  (hackage (Just "0.3.0.6") "vault") >>= debianize []
  _vc_darcs <- darcs ("http://src.seereason.com/vc-darcs")
  _vc_git_dired <- git "https://github.com/ddssff/vc-git-dired" []
  _vector <- hackage (Just "0.12.0.0") "vector" >>= debianize []
  _vector_algorithms <- hackage (Just "0.7.0.1") "vector-algorithms" >>=
                        flag (P.DebVersion "0.7.0.1-3build1") >>= debianize [] >>=
                        ghcjs_also
  _vector_binary_instances <- hackage (Just "0.2.3.4") "vector-binary-instances" >>= debianize [] >>= skip (Reason "vector dependency")
  _virthualenv <-  (hackage (Just "0.2.2") "virthualenv" >>= patch $(embedFile "patches/virthualenv.diff")) >>= debianize [] >>= skip (Reason "dependencies are missing: filepath >=1.1.0.3 && <1.4")
{-
  _virthualenv <- pure $ P.Package { P._spec = Debianize'' (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff")) Nothing
                                 , P._flags =  mempty
                                 , P._post = [] } :: TSt Package
-}
  _void <-  (hackage (Just "0.7.1") "void") >>= flag (P.DebVersion "0.7.1-1build1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _wai_app_static <-  (hackage (Just "3.1.5") "wai-app-static") >>= debianize [] >>= inGroups ["servant"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _wai <- hackage (Just "3.2.1.1") "wai" {- >>= patch $(embedFile "patches/wai.diff") -} >>= debianize [] >>= inGroups ["happstack", "important"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _wai_extra <-  (hackage (Just "3.0.16.1") "wai-extra") >>= debianize [] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _wai_logger <-  (hackage (Just "2.3.0") "wai-logger") >>= debianize [] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _wai_middleware_static <-  (hackage (Just "0.8.0") "wai-middleware-static") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-wai-dev (<< 3.1) libghc-wai-prof (<< 3.1)")
  _warp <-  (hackage (Just "3.2.7") "warp") >>= debianize []
  _webdriver <-  (git "https://github.com/kallisti-dev/hs-webdriver.git" [{-Commit "0251579c4dd5aebc26a7ac5b300190f3370dbf9d"-} {- avoid rebuild -}]) >>= debianize []
  _web_encodings <-  (hackage (Just "0.3.0.9") "web-encodings" >>= patch $(embedFile "patches/web-encodings.diff")) >>= debianize [] >>= skip (Reason "Deprecated in hackage")
{-
  _web_encodings <- pure $ P.Package { P._spec = Debianize'' (Patch (Hackage "web-encodings") $(embedFile "patches/web-encodings.diff")) Nothing
                                   , P._flags = mempty
                                   , P._post = [] } :: TSt Package
-}
  _web_plugins <-  (git "http://github.com/clckwrks/web-plugins" []) >>= debianize []
  _web_routes_boomerang <- git "https://github.com/Happstack/web-routes-boomerang.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _web_routes <- git "https://github.com/Happstack/web-routes.git" [] >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
  _web_routes_happstack <- git "https://github.com/Happstack/web-routes-happstack.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _web_routes_hsp <- git "https://github.com/Happstack/web-routes-hsp.git" [] >>= flag (P.DebVersion "0.24.6.1-1build1") >>= debianize [] >>= inGroups ["happstack", "important"]
{-
  _web_routes_mtl <- git "https://github.com/Happstack/web-routes-mtl.git" [] >>= flag (P.DebVersion "0.20.1-1~hackage1") >>= debianize [] >>= inGroups ["happstack", "important"]
-}
  _web_routes_th <- git "https://github.com/Happstack/web-routes-th.git" [] >>= debianize [] >>= inGroups ["happstack", "important"] >>= ghcjs_also
              -- web_routes_transformers =  (git "https://github.com/Happstack/web-routes.git" [] >>= cd "web-routes-transformers") >>= debianize [] -- requires transformers ==0.2.*
  _web_routes_wai <- git "https://github.com/Happstack/web-routes-wai.git" [] >>= debianize [] >>= inGroups ["happstack", "important"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _webkit_sodium <- git "https://github.com/ghcjs/ghcjs-examples" [] >>= cd "webkit-sodium" >>= debianize [] >>= ghcjs_also >>= skip2 (Reason "skipped jsaddle")
  _webkitgtk3 <- hackage (Just "0.13.1.3") "webkitgtk3" >>= flag (P.CabalPin "0.13.1.3") >>= flag (P.BuildDep "libwebkitgtk-3.0-dev") >>= debianize [] >>= inGroups ["glib"] >>= skip (Reason "see cairo and glib")
  _webkitgtk3_javascriptcore <- hackage (Just "0.13.1.1") "webkitgtk3-javascriptcore" >>= debianize [] >>= skip (Reason "Could not find module Gtk2HsSetup")
  _websockets <- hackage (Just "0.12.2.0") "websockets" >>= debianize [] >>= ghcjs_also
  _wl_pprint <- hackage (Just "1.2") "wl-pprint" >>= debianize []
  _wl_pprint_extras <- hackage (Just "3.5.0.5") "wl-pprint-extras" >>=
                       patch $(embedFile "patches/wl-pprint-extras.diff") >>=
                       flag (P.DebVersion "3.5.0.5-3build1") >>= debianize [] >>=
                       ghcjs_also
  _wl_pprint_text <-  (hackage (Just "1.1.0.4") "wl-pprint-text") >>= flag (P.DebVersion "1.1.0.4-4build1") >>= debianize [] >>= ghcjs_also
               -- Our applicative-extras repository has several important patches.
  _word8 <-  (hackage (Just "0.1.2") "word8") >>= flag (P.DebVersion "0.1.2-4") >>= debianize []
  _word_trie <-  (hackage (Just "0.3.0") "word-trie") >>= flag (P.DebVersion "0.3.0-4") >>= debianize []
  _x509 <- hackage (Just "1.6.3") "x509" >>= flag (P.DebVersion "1.6.3-1build1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_store <- hackage (Just "1.6.1") "x509-store" >>= flag (P.DebVersion "1.6.1-1build1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_system <-  (hackage (Just "1.6.3") "x509-system") >>= flag (P.DebVersion "1.6.3-1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _x509_validation <- hackage (Just "1.6.3") "x509-validation" >>= flag (P.DebVersion "1.6.3-1build1") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= ghcjs_also
  _xdg_basedir <- hackage (Just "0.2.2") "xdg-basedir" >>= flag (P.DebVersion "0.2.2-6") >>= debianize []
  _xhtml <-  (hackage (Just "3000.2.1") "xhtml" >>= wflag (P.DebVersion "3000.2.1-1") >>= qflag (P.DebVersion "3000.2.1-1build2") >>= tflag (P.DebVersion "3000.2.1-4")) >>= debianize [] >>= ghcjs_also
  _xml_conduit <-  (hackage (Just "1.3.5") "xml-conduit") >>= debianize [] >>= inGroups ["conduit", "authenticate", "important"]
  _xml <-  (hackage (Just "1.3.14") "xml") >>= flag (P.DebVersion "1.3.14-4build1") >>= debianize [] >>= ghcjs_also -- apt (rel release "wheezy" "quantal") "haskell-xml"
  _xmlgen <-  (hackage (Just "0.6.2.1") "xmlgen") >>= debianize []
  _xmlhtml <-  (hackage (Just "0.2.3.4") "xmlhtml") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-blaze-builder-dev (<< 0.4)")
  _xml_types <-  (hackage (Just "0.3.6") "xml-types") >>= flag (P.DebVersion "0.3.6-3build1") >>= debianize []
  _xss_sanitize <-  (hackage (Just "0.3.5.7") "xss-sanitize" >>= qflag (P.DebVersion "0.3.2-1build1")) >>= debianize [] >>= inGroups ["important"]
  _yaml <-  (hackage (Just "0.8.18.1") "yaml") >>= debianize [] >>= inGroups ["appraisalscribe", "important"] >>= ghcjs_also
  _yaml_light <-  (hackage (Just "0.1.4") "yaml-light"
                              >>= wflag (P.DebVersion "0.1.4-2")
                              >>= pflag (P.DebVersion "0.1.4-2")
                              >>= qflag (P.DebVersion "0.1.4-2build1")
                              >>= tflag (P.DebVersion "0.1.4-5build1")) >>= debianize []
  -- _yi <-  (hackage (Just "0.12.6") "yi") >>= debianize [] {- >>= skip (Reason "requires hint") -}
  _yi_rope <- hackage (Just "0.7.0.1") "yi-rope" >>= flag (P.DebVersion "0.7.0.1-3build1") >>= debianize []
  _zip_archive <-  (hackage (Just "0.3.0.4") "zip-archive") >>= flag (P.BuildDep "zip") >>= debianize [] >>= ghcjs_also
  _zenc <- hackage (Just "0.1.1") "zenc" >>= debianize [] >>= ghcjs_also
  _zlib_bindings <-  (hackage (Just "0.1.1.5") "zlib-bindings") >>= flag (P.DebVersion "0.1.1.5-5build1") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  -- Cabal-1.22.6.0 is not ready for zlib-0.6
  _zlib_enum <- hackage (Just "0.2.3.1") "zlib-enum" >>= flag (P.DebVersion "0.2.3.1-5build1") >>= debianize [] >>= flag (P.CabalDebian ["--no-tests"]) >>= inGroups [ "authenticate", "important"] >>= skip (Reason "needs older transformer")

#if 0
  -- Specify suspected dependencies
  _asn1_types `depends` [_hourglass]
  _authenticate `depends` [_tagstream_conduit, _xml_conduit, _http_conduit]
  _connection `depends` [_x509_system, _socks]
  _happstack_authenticate `depends` [_authenticate, _happstack_hsp, _happstack_jmacro, _shakespeare, _web_routes_happstack]
  _happstack_authenticate `depends` [_userid]
  _happstack_hsp `depends` [_happstack_server]
  _happstack_jmacro `depends` [_happstack_server]
  _happstack_scaffolding `depends` [_userid]
  _haTeX `depends` [_quickCheck, _wl_pprint_extras, _matrix]
  _ixset `depends` [_safecopy]
  _jmacro `depends` [_parseargs, _wl_pprint_text, _haskell_src_meta]
  _matrix `depends` [_loop]
  _pandoc `depends` [_juicyPixels, _pandoc_types, _yaml]
  _seereason_base `depends` [_happstack_scaffolding]
  _shakespeare `depends` [_blaze_html, _blaze_markup]
  _sr_extra `depends` [_quickCheck]
  _th_desugar `depends` [_th_reify_many, _syb]
  -- _th_typegraph `depends` [_set_extra, _th_desugar, _th_orphans]
  _web_routes_happstack `depends` [_happstack_server]
  _x509 `depends` [_pem, _asn1_parse]
  _x509_validation `depends` [_x509_store]
#endif

  -- Create the ghcjs library package targets
  -- findGroup "ghcjs-libs" >>= mapM_ ghcjs_flags
  noTests -- Some package test suites fail, some hang, especially with ghcjs
  -- noDoc
  -- noProf

  return ()

findGroup :: Monad m => GroupName -> TSt m (Set P.PackageId)
findGroup name =
  (Set.fromList . map (view pid) . filter (Set.member name . view groups) . Map.elems) <$> use packageMap
