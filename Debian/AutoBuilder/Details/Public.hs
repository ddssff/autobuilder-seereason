{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Public ( buildTargets ) where

import Control.Lens (use, view, (%=))
import Data.FileEmbed (embedFile)
import Data.Map as Map (elems, keys)
import Data.Set as Set (fromList, insert, member, Set)
import Data.Text as Text (unlines)
import Debian.AutoBuilder.Details.Common -- (named, ghcjs_flags, putSrcPkgName)
import Debian.AutoBuilder.Types.Packages as P (TSt, depends,
                                               PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           UDeb, OmitLTDeps, SkipVersion), packageMap,
                                               pid, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, debdir, uri,
                                               GroupName, inGroups, createPackage)
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..))
import Debian.Repo.Fingerprint (RetrieveMethod(Uri, DataFiles, Patch, Debianize'', Hackage, Git), GitSpec(Commit, Branch))
--import Debug.Trace (trace)

-- Group descriptions:
--    ghcjs-libs - all the libraries we build ghcjs packages for
--    th-path - libraries required by th-path, an appraisalscribe dependency in private
--    happstack -- stuff required to build happstack-server
--    appraisalscribe - libraries that are dependencies for the appraisalscribe server and its private dependencies

findGroup :: GroupName -> TSt (Set P.PackageId)
findGroup name =
  (Set.fromList . map (view pid) . filter (Set.member name . view groups) . Map.elems) <$> use packageMap

buildTargets :: TSt ()
buildTargets = do
  --------------------------------------------------
  -- INDIVIDUAL PACKAGES (alphabetized by symbol) --
  --------------------------------------------------
  _abstract_deque <-  hackage "abstract-deque" >>= debianize
  _abstract_par <- hackage "abstract-par" >>= debianize
  _acid_state <- git "https://github.com/seereason/acid-state" [Branch "log-inspection"] >>=
                 debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _adjunctions <- hackage "adjunctions" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _aeson <- hackage "aeson" >>= debianize >>= flag (P.CabalPin "0.9.0.1") >>= inGroups ["ghcjs-libs", "ghc-libs"] -- pandoc 1.15.0.6 will not build with aeson 0.10.  Also, fb, jmacro, happstack-authenticate need updates
  _aeson_pretty <- hackage "aeson-pretty" >>= debianize
  _aeson_qq <-  hackage "aeson-qq" >>= debianize >>= inGroups [ "authenticate", "important"]
  _agi <- darcs ("http://src.seereason.com/haskell-agi") >>= skip (Reason "No instance for (Applicative (AGIT m))")
  _alex <- hackage "alex" >>=
           debianize >>=
           patch $(embedFile "patches/alex.diff") >>=
           flag (P.RelaxDep "alex") >>=
           flag (P.BuildDep "happy") >>=
           inGroups ["platform"]
  _annotated_wl_pprint <- hack "annotated-wl-pprint"
  _ansi_terminal <- hackage "ansi-terminal" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _ansi_wl_pprint <- hackage "ansi-wl-pprint" >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= debianize
  _appar <- hackage "appar" >>= flag (P.DebVersion "1.1.4-1") >>= debianize
  _applicative_extras <- hackage "applicative-extras" >>= flag (P.DebVersion "0.1.8-1") >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= debianize
  _archive <- git "https://github.com/seereason/archive" []
             >>= flag (P.CabalDebian ["--default-package", "archive"])
             >>= inGroups ["autobuilder-group"] >>= debianize
  _asn1_data <- hackage "asn1-data" >>= debianize
  _asn1_encoding <-  (hackage "asn1-encoding") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _asn1_parse <-  (hackage "asn1-parse") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _asn1_types <-  (hackage "asn1-types") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _async <-  (hackage "async") >>= debianize
  _atomic_primops <-  (hackage "atomic-primops") >>= debianize
  _atp_haskell <-  (git "https://github.com/seereason/atp-haskell" []) >>= debianize >>= inGroups ["seereason", "th-path", "important", "ghcjs-libs"]
  _attempt <-  (hackage "attempt") >>= debianize
  _attoparsec <-  (hackage "attoparsec") >>= debianize
  _attoparsec_enumerator <-  (hackage "attoparsec-enumerator") >>= debianize
               -- This was merged into attoparsec
               -- ,  (hackage "attoparsec-text" `patch` $(embedFile "patches/attoparsec-text.diff") >>= flag (P.Revision "")) >>= debianize
               -- Deprecated
               -- ,  (hackage "attoparsec-text-enumerator") >>= debianize
  _authenticate <-  (hackage "authenticate") >>= debianize >>= inGroups ["ghc-libs", "authenticate", "important"]
  _autobuilder <-  (git "https://github.com/ddssff/autobuilder" []) >>= debianize
                  >>= flag (P.CabalDebian [ "--source-package-name", "autobuilder" ])
                  >>= inGroups ["autobuilder-group"]
  _autobuilder_seereason <-  (git "https://github.com/ddssff/autobuilder-seereason" []) >>= debianize >>= inGroups ["autobuilder-group"]
  _auto_update <-  (hackage "auto-update") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _base_compat <-  (hackage "base-compat") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _base_orphans <-  (hackage "base-orphans") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _base16_bytestring <-  (hackage "base16-bytestring") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _base64_bytestring <-  (hackage "base64-bytestring" >>= tflag (P.DebVersion "1.0.0.1-1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _base_unicode_symbols <-  (hackage "base-unicode-symbols" >>= tflag (P.DebVersion "0.2.2.4-3")) >>= debianize
  _bifunctors <-  (hackage "bifunctors") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _bimap <-  (hackage "bimap") >>= debianize
  _bindings_dSL <-  (hackage "bindings-DSL") >>= debianize
  _bindings_gLFW <-  (hackage "bindings-GLFW" >>= inGroups ["gl"]
                               -- `patch` $(embedFile "patches/bindings-GLFW.diff")
                               -- >>= flag (P.DevelDep "libxrandr2")
                               >>= flag (P.DevelDep "libx11-dev")
                               >>= flag (P.DevelDep "libgl1-mesa-dev")
                               >>= flag (P.DevelDep "libxi-dev")
                               >>= flag (P.DevelDep "libxxf86vm-dev")) >>= debianize
  _bitmap <-  (hackage "bitmap") >>= debianize
  _bitmap_opengl <-  (hackage "bitmap-opengl"
                     >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize >>= skip (Reason "Waiting for newer opengl")
  _bits_atomic <-  (hackage "bits-atomic") >>= debianize
  _bitset <-  (hackage "bitset") >>= debianize >>= skip (Reason "Waiting for version compatible with base-4.8")
  _blaze_builder <-  (hackage "blaze-builder") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _blaze_from_html <-  (hackage "blaze-from-html") >>= debianize >>= inGroups ["happstack", "important"]
  _blaze_html <-  (hackage "blaze-html") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _blaze_markup <-  (hackage "blaze-markup") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _blaze_textual <-  (hackage "blaze-textual") >>= debianize >>= inGroups ["happstack", "important"]
  _blaze_textual_native <-  (hackage "blaze-textual-native"
                             >>= patch $(embedFile "patches/blaze-textual-native.diff")
                             >>= flag (P.Revision "")) >>= debianize >>= inGroups ["happstack", "important"]
  _boolean <-  (hackage "Boolean") >>= debianize
  _boomerang <-  (hackage "boomerang") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- _bugzilla <- broken <$> apt "squeeze" "bugzilla" -- requires python-central (>= 0.5)
  _byteable <-  (hackage "byteable" >>= tflag (P.DebVersion "0.1.1-1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _byteorder <-  (hackage "byteorder" >>= tflag (P.DebVersion "1.0.4-1")) >>= debianize
  _bytes <-  (hackage "bytes") >>= debianize >>= skip (Reason "Unmet build dependencies: libghc-cereal-dev (<< 0.5)")
  _bytestring_builder <-  (hackage "bytestring-builder") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _bytestring_conversion <- hackage "bytestring-conversion" >>= debianize >>= inGroups ["servant"]
  _bytestring_nums <-  (hackage "bytestring-nums") >>= debianize -- apt (rel release "wheezy" "quantal") "haskell-bytestring-nums"
  _bytestring_trie <-  (hackage "bytestring-trie") >>= debianize
  _bzlib <-  (hackage "bzlib" >>= flag (P.DevelDep "libbz2-dev")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- ,  (hackage "cairo-pdf") >>= debianize
  _cabal_debian <- git "https://github.com/ddssff/cabal-debian" []
                  >>= flag (P.CabalDebian ["--cabal-flags", "-tests"]) -- turn off test suite
                  >>= inGroups ["autobuilder-group"]
  -- This is provided by ghc
  -- _cabal = hackage "Cabal" >>= debianize
  _cabal_install <- hackage "cabal-install" >>= flag (P.CabalPin "1.22.7.0") >>=
                    -- patch $(embedFile "patches/cabal-install.diff") >>= -- Remove bound on HTTP dependency
                    flag (P.CabalDebian ["--default-package", "cabal-install"]) >>=
                    debianize >>=
                    inGroups ["platform"]
  _cabal_macosx <-  (hackage "cabal-macosx" {-`patch` $(embedFile "patches/cabal-macosx.diff")-}) >>= debianize
  _c_ares <- apt "sid" "c-ares" >>= skip (Reason "Use standard")
  _cairo <- hackage "cairo" >>= flag (P.BuildDep "gtk2hs-buildtools") >>= debianize
  _case_insensitive <-  (hackage "case-insensitive") >>= debianize
               -- Here is an example of creating a debian/Debianize.hs file with an
               -- autobuilder patch.  The autobuilder then automatically runs this
               -- script to create the debianization.
  _categories <- hackage "categories" >>= tflag (P.DebVersion "1.0.6-1") >>= debianize >>= broken
      -- comonad now includes comonad-transformers and comonads-fd
  _cautious_file <-  (hackage "cautious-file" >>= tflag (P.DebVersion "1.0.2-2")) >>= debianize >>= skip (Reason "requires filepath < 1.4")
  _cc_delcont <-  (hackage "CC-delcont" >>= flag (P.DebVersion "0.2-1~hackage1")) >>= debianize >>= skip (Reason "Missing applicative instances in 0.2")
               -- , apt (rel release "wheezy" "quantal") "haskell-cereal"
  _cereal <-  (hackage "cereal" {->>= flag (P.CabalPin "0.4.1.1")-}) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] -- Concerns about migration in 0.5
  _certificate <- hackage "certificate" >>= patch $(embedFile "patches/certificate.diff") >>= tflag (P.DebVersion "1.3.9-1build4") >>= debianize
  _cgi <- (hackage "cgi" {- `patch` $(embedFile "patches/cgi.diff") -}) >>= debianize >>= inGroups ["platform"] >>= skip (Reason "Depends on exceptions < 0.7")
  _charset <-  (hackage "charset") >>= debianize
  _charsetdetect_ae <-  (hackage "charsetdetect-ae") >>= debianize
  _cheapskate <-  (git "https://github.com/seereason/cheapskate" [] {-hackage "cheapskate"-}) >>= debianize
  _cipher_aes128 <-  (hackage "cipher-aes128") >>= debianize >>= inGroups ["authenticate", "important"]
  _cipher_aes <-  (hackage "cipher-aes") >>= debianize
  _cipher_des <-  (hackage "cipher-des" >>= tflag (P.DebVersion "0.0.6-1")) >>= debianize
  _cipher_rc4 <-  (hackage "cipher-rc4" >>= tflag (P.DebVersion "0.1.4-1")) >>= debianize >>= inGroups [ "authenticate", "important"]
  _citeproc_hs <-  (hackage "citeproc-hs") >>= debianize >>= skip (Reason "Non type-variable argument\nthe constraint: MonadState EvalState m\n(Use FlexibleContexts to permit this)")
  _clckwrks_cli <-  (gitrepo "clckwrks-cli") >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_dot_com <- gitrepo "clckwrks-dot-com" >>=
                                 -- This is a change that only relates to the autobuilder
                                 patch $(embedFile "patches/clckwrks-dot-com.diff") >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_bugs <-  (gitrepo "clckwrks-plugin-bugs"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_ircbot <-  (gitrepo "clckwrks-plugin-ircbot"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_media <-  (gitrepo "clckwrks-plugin-media"
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_plugin_page <-  (gitrepo "clckwrks-plugin-page"
                             -- `patch` $(embedFile "patches/clckwrks-plugin-page.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks <-
    createPackage (Debianize'' (Patch (DataFiles (DataFiles
                                                  (Git "https://github.com/clckwrks/clckwrks" [])
                                                  (Uri "https://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                       "e211065e573ea0239d6449882c9d860d")
                                                  "jstree")
                                                 (Uri "https://raw.githubusercontent.com/douglascrockford/JSON-js/master/json2.js"
                                                      "a6d5fdbbcb076dd9385dd2135dbfb589" {-previouly: "5eecb009ae16dc54f261f31da01dbbac"-})
                                                 "json2")
                                      $(embedFile "patches/clckwrks.diff"))
                               Nothing)
                  [P.BuildDep "hsx2hs"]
                  [] >>= inGroups ["clckwrks", "important"]
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
                                 >>= debianize
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
                             , P._post = [] }) >>= inGroups ["clckwrks", "important"] :: TSt Package
-}
  _clckwrks_theme_bootstrap <-  (gitrepo "clckwrks-theme-bootstrap" >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clckwrks_theme_clckwrks <-  (gitrepo "clckwrks-theme-clckwrks" >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _clock <-  (hackage "clock") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _closure_compiler <- apt "sid" "closure-compiler"
  _cmark <-  (hackage "cmark") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _cmdargs <-  (hackage "cmdargs") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _colour <-  hackage "colour" >>= pflag (P.DebVersion "2.3.3-1build1")
                              >>= qflag (P.DebVersion "2.3.3-1build1")
                              >>= sflag (P.DebVersion "2.3.3-1")
                              >>= tflag (P.DebVersion "2.3.3-4")
                              >>= debianize
               -- , apt "wheezy" "haskell-configfile"
  _comonad <- hackage "comonad" >>=
                     flag (P.CabalPin "4.2.7.2") >>= -- version 5 is too new for semigroupoids 5.0.1
                     apply (replacement "comonad" "comonad-transformers") >>=
                     apply (replacement "comonad" "comonad-fd") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _concatenative <-  (hackage "concatenative") >>= debianize
  _concrete_typerep <-  hackage "concrete-typerep" >>=
                        tflag (P.DebVersion "0.1.0.2-2build3") >>=
                        debianize >>=
                        skip (Reason "Constructor ‘TypeRep’ should have 4 arguments, but has been given 3")
  _cond <-  (hackage "cond") >>= debianize
  _conduit <-  (hackage "conduit") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
  _conduit_extra <-  (hackage "conduit-extra") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important", "servant"]
  _configurator <- hackage "configurator" >>= debianize
  _configFile <-  (hackage "ConfigFile") >>= debianize
  _connection <-  (hackage "connection") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
  _constrained_normal <-  (hackage "constrained-normal") >>= debianize
  _constraints <-  (hackage "constraints" >>= flag (P.CabalPin "0.4.1.3")) >>= debianize -- 0.6 is too new for lifted-async
  _consumer <- darcs ("http://src.seereason.com/haskell-consumer") >>= skip (Reason "build failure")
  _contravariant <-  (hackage "contravariant") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _control_monad_free <-  (hackage "control-monad-free") >>= debianize
  _cookie <-  (hackage "cookie") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _cpphs <-  (hackage "cpphs") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- apt "sid" "debian-keyring=2014.03.03" -- The current version (2014.04.25) seems to be missing some keys that we need
  _cprng_aes <-  (hackage "cprng-aes") >>= debianize
  _cpu <-  (hackage "cpu" >>= tflag (P.DebVersion "0.1.2-1")) >>= debianize
  _crypto_api <-  (hackage "crypto-api" >>= qflag (P.DebVersion "0.10.2-1build3")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- The certificate package may need to be updated for version 0.4
  _crypto_cipher_types <-  (hackage "crypto-cipher-types" >>= tflag (P.DebVersion "0.0.9-1")) >>= debianize >>= inGroups [ "authenticate", "important"]
  _crypto <-  (hackage "Crypto") >>= debianize
  _cryptohash_conduit <-  (hackage "cryptohash-conduit") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "servant"]
  _cryptohash_cryptoapi <-  (hackage "cryptohash-cryptoapi") >>= debianize >>= inGroups ["happstack", "important"]
  _cryptohash <-  (hackage "cryptohash") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _cryptonite <-  (hackage "cryptonite") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _crypto_numbers <-  (hackage "crypto-numbers") >>= debianize >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey <-  (hackage "crypto-pubkey") >>= debianize >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey_types <-  (hackage "crypto-pubkey-types") >>= debianize
               -- crypto-pubkey-types-0.3.2 depends on older asn1-types
  _crypto_random_api <-  (hackage "crypto-random-api" >>= tflag (P.DebVersion "0.2.0-2")) >>= debianize
  _crypto_random <-  (hackage "crypto-random") >>= debianize
  _css <-  (hackage "css") >>= debianize >>= skip (Reason "No instance for (Applicative (CSSM x))")
  _css_text <-  (hackage "css-text") >>= debianize
  _csv <-  (hackage "csv" >>= pflag (P.DebVersion "0.1.2-2")
                              >>= tflag (P.DebVersion "0.1.2-5build1")) >>= debianize
  _curl <-  (hackage "curl" >>= tflag (P.DebVersion "1.3.8-2")) >>= debianize -- apt (rel release "wheezy" "quantal") "haskell-curl"
  _currency <-  (hackage "currency") >>= debianize
  _data_accessor <-  (hackage "data-accessor") >>= debianize
  _data_accessor_template <-  (hackage "data-accessor-template") >>= debianize
  _data_binary_ieee754 <- hack "data-binary-ieee754"
  _data_default_class <-  (hackage "data-default-class" >>= tflag (P.DebVersion "0.0.1-1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_default <-  (hackage "data-default") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_default_instances_base <-  (hackage "data-default-instances-base") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_default_instances_containers <-  (hackage "data-default-instances-containers") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_default_instances_dlist <-  (hackage "data-default-instances-dlist") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_default_instances_old_locale <-  (hackage "data-default-instances-old-locale") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _dataenc <-  (hackage "dataenc" >>= patch $(embedFile "patches/dataenc.diff")) >>= debianize
  _data_lens <-  (hackage "data-lens" {-`patch` $(embedFile "patches/data-lens.diff")-}) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _data_lens_light <- hackage "data-lens-light" >>= debianize >>= inGroups ["ghc-libs"]
  _data_lens_template <-  (hackage "data-lens-template") >>= debianize
  _data_object <-  (hackage "data-object" >>= patch $(embedFile "patches/data-object.diff")) >>= debianize
  _data_ordlist <-  (hackage "data-ordlist") >>= debianize
  _data_reify <-  (hackage "data-reify") >>= debianize
  _data_r_tree <-  (hackage "data-r-tree") >>= debianize
  _data_stringmap <-  (hackage "data-stringmap") >>= debianize
  _date_cache <-  (hackage "date-cache" >>= tflag (P.DebVersion "0.3.0-3")) >>= debianize >>= inGroups [ "authenticate", "important"]
  _datetime <- hackage "datetime" {->>= pflag (P.DebVersion "0.2.1-2") >>= tflag (P.DebVersion "0.2.1-5build1")-} >>= debianize
  _debhelper <- apt "wheezy" "debhelper" >>= patch $(embedFile "patches/debhelper.diff") >>= skip (Reason "Use standard")
  _debian_haskell <- git "https://github.com/ddssff/debian-haskell" [] >>= flag (P.RelaxDep "cabal-debian") >>= inGroups ["autobuilder-group"]
  _debian_repo <- git "https://github.com/ddssff/debian-repo" [] >>= inGroups ["autobuilder-group"]
  _debootstrap <- apt "sid" "debootstrap" >>= flag (P.UDeb "debootstrap-udeb")
               -- Build fails due to some debianization issue
               -- , apt "wheezy" "geneweb"
  _decimal <-  (hackage "Decimal") >>= debianize -- for hledger
  _deepseq_generics <- hackage "deepseq-generics" >>= {-patch $(embedFile "patches/deepseq-generics.diff") >>=-} debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _derive <-  (hackage "derive") >>= debianize >>= skip (Reason "[libghc-src-exts-prof (>= 1.17)] -> []")
  _diff <- hackage "Diff" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _digest <-  (hackage "digest") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _digestive_functors <-  (hackage "digestive-functors" >>= flag (P.CabalPin "0.2.1.0")) >>= debianize >>= inGroups ["seereason", "important"]  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
      -- , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1"]
  _digestive_functors_happstack <-  (git "https://github.com/seereason/digestive-functors" []
                                            >>= cd "digestive-functors-happstack"
                                            >>= flag (P.DebVersion "0.1.1.5-2")) >>= debianize >>= inGroups ["digestive-functors", "appraisalscribe", "important"]
  _digestive_functors_hsp <-  (darcs ("http://src.seereason.com/digestive-functors-hsp") >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["seereason", "important"]
  _directory_tree <-  (hackage "directory-tree") >>= debianize
  _distributive <-  (hackage "distributive") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _dlist <-  (hackage "dlist") >>= debianize
               -- Natty only(?)
  _doctest <-  (hackage "doctest") >>= debianize >>= inGroups [{-"ghcjs-libs",-} "ghc-libs"]
      -- This package fails to build in several different ways because it has no modules.
      -- I am just going to patch the packages that use it to require transformers >= 0.3.
      -- Specifically, distributive and lens.
  _double_conversion <-  (hackage "double-conversion") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _dpkg <- apt "wheezy" "dpkg" >>= patch $(embedFile "patches/dpkg.diff") >>= skip (Reason "use standard")
  _drbg <-  (hackage "DRBG") >>= debianize >>= inGroups ["authenticate", "important"] >>= skip (Reason "requires update to use current cereal")
  _dropbox_sdk <-  (hackage "dropbox-sdk") >>= debianize -- >>= patch $(embedFile "patches/dropbox-sdk.diff")
  _dynamic_state <-  (hackage "dynamic-state") >>= debianize
  _dyre <-  (hackage "dyre") >>= debianize
  _easy_file <-  (hackage "easy-file") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _edisonAPI <- hackage "EdisonAPI" >>= debianize
  _edisonCore <- ( (hackage "EdisonCore" >>= qflag (P.DebVersion "1.2.1.3-9build2")) >>= debianize)
  _ekg_core <-  (hackage "ekg-core") >>= debianize
  _email_validate <-  (hackage "email-validate") >>= debianize
  _enclosed_exceptions <-  (hackage "enclosed-exceptions") >>= debianize >>= inGroups ["ghcjs-libs", "ghcjs-comp"]
  _entropy <-  (hackage "entropy") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _enumerator <-  (hackage "enumerator" >>= qflag (P.DebVersion "0.4.19-1build2")) >>= debianize
  _erf <-  (hackage "erf"
                              >>= pflag (P.DebVersion "2.0.0.0-3")
                              >>= wflag (P.DebVersion "2.0.0.0-3")
                              >>= tflag (P.DebVersion "2.0.0.0-5")) >>= debianize
  _errors <-  (hackage "errors") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _exceptions <-  (hackage "exceptions") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _expiring_cache_map <-  (hackage "expiring-cache-map") >>= debianize
  _executable_path <-  (hackage "executable-path"
                              >>= pflag (P.DebVersion "0.0.3-1")
                              >>= tflag (P.DebVersion "0.0.3-3")) >>= debianize
               -- , apt (rel release "wheezy" "quantal") "haskell-digest"
               -- , apt (rel release "wheezy" "quantal") "haskell-dlist"
  _extensible_exceptions <-  (hackage "extensible-exceptions" -- required for ghc-7.6.  Conflicts with ghc-7.4 in wheezy.
                              >>= tflag (P.DebVersion "0.1.1.4-2")) >>= debianize
  _extra <-  (hackage "extra") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _failure <-  (hackage "failure") >>= debianize
  _fast_logger <-  (hackage "fast-logger") >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs", "authenticate", "important"]
  _fay_base <-  (hackage "fay-base") >>= debianize >>= skip (Reason "Waiting for newer fay")
  _fay <-  (hackage "fay" {- >>= patch $(embedFile "patches/fay.diff") -}) >>= debianize >>= flag (P.CabalDebian [ "--depends", "haskell-fay-utils:cpphs" ]) >>= skip (Reason "too old for current syb and optparse-applicative")
  _fay_jquery <-  (git "https://github.com/faylang/fay-jquery" []) >>= debianize >>= skip (Reason "Waiting for newer fay")
  _fay_text <-  (hackage "fay-text") >>= debianize >>= skip (Reason "Waiting for newer fay")
  _fb <-  (git "https://github.com/ddssff/fb.git" []) >>= debianize >>= inGroups [ "authenticate", "appraisalscribe", "important"]
  _feed <- git "https://github.com/seereason/feed" [] {-hackage "feed"-} >>= tflag (P.DebVersion "0.3.9.2-1") >>= debianize
  _fgl <-  (hackage "fgl") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _file_embed <-  (hackage "file-embed") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _file_location <-  (hackage "file-location" >>= flag (P.CabalDebian [ "--source-package-name", "file-location" ])) >>= debianize
  _filemanip <-  (hackage "filemanip") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _filemanip_extra <-  (git "https://github.com/seereason/filemanip-extra" []) >>= debianize >>= inGroups ["ghcjs-libs", "autobuilder-group"]
  _fingertree <- hack "fingertree"
  _fixed <- hackage "fixed" >>= debianize
  _flock <- hack "flock"
  _fmlist <-  (hackage "fmlist") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _foreign_var <-  (hackage "foreign-var") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _formlets <-  (hackage "formlets"
                               >>= patch $(embedFile "patches/formlets.diff")
                               >>= flag (P.DebVersion "0.8-1~hackage1")) >>= debianize
  _free <-  (hackage "free") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _frquotes <-  (hackage "frquotes") >>= debianize
               -- Usable versions of this package are available in some dists -
               -- e.g. trusty and wheezy.
               -- , apt "trusty" "foo2zjs"
  _fsnotify <-  (hackage "fsnotify") >>= debianize
  _ftgl <-  (hackage "FTGL"
                     -- >>= patch $(embedFile "patches/FTGL.diff")
                     >>= flag (P.DevelDep "libftgl-dev")
                     >>= flag (P.DevelDep "libfreetype6-dev")) >>= debianize
  _gd <-  (hackage "gd" >>= patch $(embedFile "patches/gd.diff")
                       >>= flag (P.DevelDep "libgd-dev")
                       >>= flag (P.DevelDep "libc6-dev")
                       >>= flag (P.DevelDep "libfreetype6-dev")
                       >>= wflag (P.DebVersion "3000.7.3-1")
                       >>= qflag (P.DebVersion "3000.7.3-1build2")
                       >>= tflag (P.DebVersion "3000.7.3-3")) >>= debianize
               -- ,  (flags [P.BuildDep "libm-dev", P.BuildDep "libfreetype-dev"] (hackage "gd")) >>= debianize
  _gdiff <-  (hackage "gdiff") >>= debianize
               -- ,  (hackage "hjsmin") >>= debianize
  _generic_deriving <-  (hackage "generic-deriving") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _genI <-  (darcs "http://hub.darcs.net/kowey/GenI" >>= patch $(embedFile "patches/GenI.diff")) >>= debianize >>= inGroups ["GenI"]
  -- ghc76 <- ghcFlags $ apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff")
  -- ghc78 <- ghcFlags $ apt "experimental" "ghc" >>= patch $(embedFile "patches/trac9262.diff")
  _ghc710 <- apt "experimental" "ghc" >>= ghcFlags
                        >>= patch $(embedFile "patches/ghc.diff")
                            >>= skip (Reason "stick with current, avoid huge rebuild")
  _terminal_size <- hackage "terminal-size"  >>= debianize  >>= inGroups ["ghcid"]
  _ghcid <- hackage "ghcid" >>= debianize  >>= inGroups ["ghcid"]
  _ghcjs_base <- git "https://github.com/ghcjs/ghcjs-base" [] >>= debianize >>= inGroups ["ghcjs-libs", "glib"]
  _ghcjs_jquery <-  (git "https://github.com/ghcjs/ghcjs-jquery" []) >>= debianize
                    {-`putSrcPkgName` "ghcjs-ghcjs-jquery"-}
                    >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- ghcjs_vdom = ghcjs_flags ( (git "https://github.com/seereason/ghcjs-vdom" [Branch "base48"]) >>= debianize `putSrcPkgName` "ghcjs-ghcjs-vdom")
  _ghcjs_ffiqq <- git "https://github.com/ghcjs/ghcjs-ffiqq" [] >>= putSrcPkgName "ghcjs-ghcjs-ffiqq" >>= debianize >>= inGroups ["ghcjs-libs"] >>= skip (Reason "[libghc-ghcjs-base-doc] -> []")
  _ghcjs_dom <- {-git "https://github.com/ghcjs/ghcjs-dom" []-}
                hackage "ghcjs-dom" >>= flag (P.CabalPin "0.2.3.1") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "glib"]
  _ghcjs_dom_hello <-  (hackage "ghcjs-dom-hello"
                                 >>= patch $(embedFile "patches/ghcjs-dom-hello.diff")
                                 >>= flag (P.CabalDebian ["--default-package", "ghcjs-dom-hello"])) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "glib"]
  _ghcjs <- git "https://github.com/ddssff/ghcjs-debian" [{-Branch "ghcjsi"-}] >>= relax "cabal-install" >>= inGroups ["ghcjs-comp"]
  -- _ghcjs_prim <- git "https://github.com/ghcjs/ghcjs-prim" [] >>= debianize >>= inGroups ["ghcjs-comp", "glib"]
  _ghc_mtl <- (hackage "ghc-mtl") >>= debianize >>= skip (Reason "No instance for (MonadIO GHC.Ghc)")
  _ghc_paths <-  (hackage "ghc-paths" >>= tflag (P.DebVersion "0.1.0.9-3")) >>= debianize -- apt (rel release "wheezy" "quantal") "haskell-ghc-paths" -- for leksah
               -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
               -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
               --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
               -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
               -- Errors were encountered while processing:
               --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
               -- E: Sub-process /usr/bin/dpkg returned an error code (1)
  _ghc_simple <- (hackage "ghc-simple") >>= debianize
  _gio <- hackage "gio" >>= patch $(embedFile "patches/gio.diff") >>= inGroups ["glib"]
  _glfw <-  (hackage "GLFW" >>= inGroups ["gl"] >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize >>= skip (Reason "Waiting for newer opengl")
      -- ,  (hackage "GLFW-b") >>= debianize
      -- ,  (hackage "GLFW-b-demo" >>= flag (P.SkipPackage {- >>= patch $(embedFile "patches/GLFW-b-demo.diff") -})) >>= debianize
  _glfw_task <-  (hackage "GLFW-task" >>= inGroups ["gl"]) >>= debianize >>= skip (Reason "Waiting for newer opengl")
  _glib <- hackage "glib" >>=
           flag (P.BuildDep "gtk2hs-buildtools") >>=
           flag (P.BuildDep "libpango1.0-dev") >>=
           flag (P.BuildDep "libgtk2.0-dev") >>=
           flag (P.BuildDep "libglib2.0-dev") >>=
           debianize >>= inGroups ["glib"]
  _gluRaw <- (hackage "GLURaw" >>= inGroups ["gl"]) >>= debianize
  _glut <-  (hackage "GLUT" >>= inGroups ["gl"]
                     >>= flag (P.DevelDep "freeglut3-dev")) >>= debianize >>= skip (Reason "Waiting for newer opengl")
  _groom <-  (hackage "groom") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- Retired
               -- , apt "wheezy" "haskell-dummy"
               -- Need this when we upgrade blaze-textual to 0.2.0.0
               -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
  _gtk2hs_buildtools <-  (hackage "gtk2hs-buildtools"
                              >>= flag (P.CabalDebian ["--default-package", "gtk2hs-buildtools",
                                                       "--build-dep", "alex",
                                                       "--build-dep", "happy",
                                                       "--revision", ""])) >>= debianize >>= inGroups ["glib"]
               -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
  _gtk3 <- hackage "gtk3" >>= flag (P.CabalPin "0.13.9") >>= flag (P.BuildDep "libgtk-3-dev") >>= debianize >>= inGroups ["glib"]
  _gyp <- apt "sid" "gyp" >>= skip (Reason "Use standard")
  _haddock_api <-  hackage "haddock-api"
                    >>= flag (P.CabalDebian ["--default-package", "haddock-api"])
                    -- FIXME - This cabal-debian stuff does nothing because this isn't a Debianize target
                    >>= apply (execCabalM $ (debInfo . rulesFragments) %= Set.insert (Text.unlines
                                                                                                   [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                                                   , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"]))
                    >>= debianize >>= inGroups ["ghcjs-comp"]
  haddock_library <-  (hackage "haddock-library") >>= debianize >>= inGroups ["ghcjs-libs"]
  _half <-  (hackage "half" >>= inGroups ["gl"]) >>= debianize
  _hamlet <-  (hackage "hamlet") >>= debianize >>= inGroups ["ghcjs-libs"]
  -- seereason still depends on this
  _happstack_authenticate_0 <-  (git "https://github.com/Happstack/happstack-authenticate-0.git" []
                             >>= flag (P.CabalDebian [ "--debian-name-base", "happstack-authenticate-0",
                                                    "--cabal-flags", "migrate",
                                                    "--executable", "happstack-authenticate-migrate" ])) >>= debianize >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_authenticate <-  (git "https://github.com/Happstack/happstack-authenticate.git" []) >>= debianize >>= inGroups [ "authenticate", "happstack", "lens", "important"]
  _happstack_clckwrks <-  (git ("https://github.com/Happstack/happstack-clckwrks") [] >>=
                             cd "clckwrks-theme-happstack"
                             -- >>= patch $(embedFile "patches/clckwrks-theme-happstack.diff")
                             >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _happstack_dot_com <-  (git ("https://github.com/Happstack/happstack-clckwrks") []
                                   >>= cd "happstack-dot-com"
                                   -- This is a change that only relates to the autobuilder
                                   >>= patch $(embedFile "patches/happstack-dot-com.diff")) >>= debianize >>= inGroups ["clckwrks", "important"]
  _happstackDotCom_doc <- darcs ("http://src.seereason.com/happstackDotCom-doc") >>= inGroups ["happstack", "important"]
  _happstack_extra <-  (git "https://github.com/seereason/happstack-extra.git" []) >>= debianize
  _happstack_fay_ajax <-  (hackage "happstack-fay-ajax" >>= patch $(embedFile "patches/happstack-fay-ajax.diff")) >>= debianize >>= skip (Reason "Waiting for newer fay")
      -- ,  (hackage "fay-hsx" >>= patch $(embedFile "patches/fay-hsx.diff")) >>= debianize
  _happstack_fay <-  (hackage "happstack-fay" >>= patch $(embedFile "patches/happstack-fay.diff")) >>= debianize >>= skip (Reason "Waiting for newer fay")
  _happstack_foundation <-  (git "https://github.com/Happstack/happstack-foundation.git" []) >>= debianize >>= inGroups ["happstack", "important"]
  _happstack_foundation_example <-
       (git "https://github.com/Happstack/happstack-foundation.git" []
                                   >>= cd "examples/ControlVAuth"
                                   >>= flag (P.CabalDebian ["--source-package-name", "happstack-foundation-example",
                                                         "--default-package", "happstack-foundation-example"])) >>= debianize >>= inGroups ["happstack", "important"]
  _happstack_hsp <-  (git "https://github.com/Happstack/happstack-hsp.git" []) >>= debianize >>= inGroups ["happstack", "lens", "important"]
  _happstack_jmacro <-  (git "https://github.com/Happstack/happstack-jmacro.git" []) >>= debianize >>= inGroups ["happstack", "lens", "important"]
  _happstack_lite <-  (hackage "happstack-lite") >>= debianize >>= inGroups ["happstack", "important"]
  _happstack_plugins <-  (hackage "happstack-plugins" >>= patch $(embedFile "patches/happstack-plugins.diff")) >>= debianize >>= skip (Reason "Needs plugins-auto")
  _happstack_scaffolding <-  (git "https://github.com/seereason/happstack-scaffolding" [] >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["seereason", "important"]
  _happstack_search <- darcs ("http://src.seereason.com/happstack-search") >>= inGroups ["happstack", "important"]
              -- ,  (hackage "happstack-server") >>= debianize
  _happstack_server <- git "https://github.com/Happstack/happstack-server" [] >>=
                       -- flag (P.CabalDebian ["--cabal-flags", "hslogger"]) >>=
                       debianize >>=
                       inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _happstack_server_tls <-  (git "https://github.com/Happstack/happstack-server-tls" []) >>= debianize >>= inGroups ["happstack", "important"]
  _happstack_static_routing <-  (hackage "happstack-static-routing") >>= debianize {->>= inGroups ["happstack", "important"]-}
  _happstack_util <- hackage "happstack-util" >>=
                     patch $(embedFile "patches/happstack-util.diff") >>=
                     flag (P.DebVersion "6.0.3-1") >>=
                     debianize >>=
                     inGroups ["happstack", "important"]
  _happy <-  (hackage "happy") >>= debianize
            >>= flag (P.RelaxDep "happy")
            >>= flag (P.BuildDep "happy")
            >>= flag (P.CabalDebian ["--executable", "happy", "--build-dep", "happy"])
             -- >>= flag (P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"),
            >>= apply (execCabalM $ do mapM_ (\ name -> (debInfo . atomSet) %= (Set.insert $ InstallData (BinPkgName "happy") name name))
                                               [ "HappyTemplate-arrays-coerce"
                                               , "GLR_Lib-ghc"
                                               , "HappyTemplate-arrays-ghc-debug"
                                               , "HappyTemplate"
                                               , "GLR_Lib"
                                               , "HappyTemplate-arrays-debug"
                                               , "GLR_Base"
                                               , "HappyTemplate-ghc"
                                               , "HappyTemplate-arrays-ghc"
                                               , "HappyTemplate-coerce"
                                               , "HappyTemplate-arrays"
                                               , "HappyTemplate-arrays-coerce-debug"
                                               , "GLR_Lib-ghc-debug" ] ) >>= inGroups ["platform"]
  _harp <-  (git "https://github.com/seereason/harp" []) >>= debianize
  _hashable <-  (hackage "hashable") >>= debianize
  _hashed_storage <-  (hackage "hashed-storage") >>= debianize >>= skip (Reason "Non type-variable argument in the constraint: MonadState (TreeState m_aFTg) (t m_aFTg)")
               -- Built into ghc-7.8.3
  _hashtables <-  (hackage "hashtables") >>= debianize
  _haskeline <-  (hackage "haskeline") >>= debianize
  _haskell_darcs <-  (darcs "http://darcs.net/reviewed"
                     >>= flag (P.CabalDebian ["--source-package-name", "darcs"])
                     >>= flag (P.CabalDebian ["--default-package", "darcs"])
                     >>= flag (P.CabalDebian ["--cabal-flags", "-http"]) -- the http flag forces network < 2.5
                     -- >>= patch $(embedFile "patches/darcs.diff")
                    ) >>= debianize >>= skip (Reason "Unmet build dependencies: libghc-vector-dev (<< 0.11)")
  _haskell_devscripts <- git {-"http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git"-}
                           "http://github.com/ddssff/haskell-devscripts" [] >>= flag (P.RelaxDep "python-minimal")
  _haskell_either <-  (hackage "either") >>= debianize
  _sr_extra <-  (git ("https://github.com/seereason/sr-extra") []
                              -- Don't push out libghc-extra-dev, it now comes from Neil Mitchell's repo
                              {- `apply` (replacement "sr-extra" "Extra") -}
                       ) >>= debianize >>= inGroups ["ghcjs-libs", "autobuilder-group"]
  _haskell_help <-  (git ("https://github.com/seereason/sr-help") []) >>= debianize >>= inGroups ["autobuilder-group"]
  _haskell_lexer <-  (hackage "haskell-lexer"
                              >>= pflag (P.DebVersion "1.0-3build2")
                              >>= wflag (P.DebVersion "1.0-3+b1")
                              >>= tflag (P.DebVersion "1.0-5")) >>= debianize
  _haskell_list <-  (hackage "List") >>= debianize
  _haskell_names <-  (hackage "haskell-names") >>= debianize
  _haskell_newtype <-  (hackage "newtype" >>= wflag (P.DebVersion "0.2-1") >>= tflag (P.DebVersion "0.2-3")) >>= debianize
  _haskell_packages <-  (hackage "haskell-packages" {->>= patch $(embedFile "patches/haskell-packages.diff")-}) >>= debianize >>= inGroups ["happstack", "important"]
  _sr_revision <-  (git ("https://github.com/seereason/sr-revision") []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _haskell_src <-  (hackage "haskell-src" >>= flag (P.BuildDep "happy")) >>= debianize >>= inGroups ["platform"]
  -- The source package name is set to haskell-src-exts by the
  -- cabal-debian package, Debian.Debianize.Details.debianDefaults.
  -- But where does that leave ghcjs-haskell-src-exts?
  _haskell_src_exts <- hackage "haskell-src-exts" >>= flag (P.BuildDep "happy") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- haskell_src_meta =  (hackage "haskell-src-meta") >>= debianize
  -- haskell_src_meta =  (git "https://github.com/bmillwood/haskell-src-meta" []) >>= debianize
  -- haskell_src_meta <-  (git "https://github.com/ddssff/haskell-src-meta" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _haskell_src_meta <- hackage "haskell-src-meta" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "th-path", "important"]
  _haste_compiler <- hack "haste-compiler" >>= flag (P.CabalDebian ["--default-package", "haste-compiler"])
  _haste_ffi_parser <- git' "https://github.com/RudolfVonKrugstein/haste-ffi-parser" []
  _haTeX <-  (git "https://github.com/Daniel-Diaz/HaTeX" [Commit "cc66573a0587094667a9150411ea748d6592db36" {-avoid rebuild-}]
                                >>= patch $(embedFile "patches/HaTeX-texty.diff")
                                >>= patch $(embedFile "patches/HaTeX-doc.diff")
                    ) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _haXml <-  (hackage "HaXml") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hdaemonize <-  (git "https://github.com/madhadron/hdaemonize" []) >>= debianize
  _heap <-  (hackage "heap") >>= debianize
               -- ,  (hackage "heist" >>= patch $(embedFile "patches/heist.diff")) >>= debianize
  _hex <-  (hackage "hex") >>= debianize
  _hexpat <- hackage "hexpat" >>= debianize
  _highlighting_kate <-  (hackage "highlighting-kate") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _hinotify <-  (hackage "hinotify") >>= debianize
  _hint <-  (hackage "hint") >>= debianize >>= skip (Reason "requires ghc-mtl")
  _hJavaScript <-  (hackage "HJavaScript"
                              >>= patch $(embedFile "patches/hjavascript.diff")
                              >>= pflag (P.DebVersion "0.4.7-3++1")
                              >>= tflag (P.DebVersion "0.4.7-6")) >>= debianize
               -- Not used, and not building.
               -- ,  (hackage "hoauth") >>= debianize
  _hJScript <- git "https://github.com/seereason/HJScript" [] >>= debianize >>= inGroups ["happstack", "important"]
  _hledger <-  (git "https://github.com/simonmichael/hledger" [] >>= cd "hledger-lib") >>= debianize
           {-
               -- Needs a build dependency on libXrandr-dev and the cabal package x11.
               , P.Package { P.spec =  (Hackage "xmobar") >>= Debianize
                           , P.flags = [] }
           -}
  _hlint <-  (hackage "hlint") >>= debianize >>= inGroups [{-"ghcjs-libs",-} "ghc-libs"] >>= skip (Reason "[libghc-refact-doc] -> []")
  _hostname <-  (hackage "hostname"  >>= inGroups ["ghcjs-libs", "ghc-libs"]
                              >>= wflag (P.DebVersion "1.0-4")
                              >>= pflag (P.DebVersion "1.0-4build1")
                              >>= qflag (P.DebVersion "1.0-4build3")
                              >>= sflag (P.DebVersion "1.0-1~hackage1")
                              >>= tflag (P.DebVersion "1.0-6")) >>= debianize
               -- The Sid package has no profiling libraries, so dependent packages
               -- won't build.  Use our debianization instead.  This means keeping
               -- up with sid's version.
  _hourglass <-  (hackage "hourglass") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hpdf <-  (hackage "HPDF") >>= debianize
  _hS3 <-  (git "https://github.com/scsibug/hS3.git" []) >>= debianize
                               >>= apply (execCabalM $ doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
  _hs_bibutils <-  (hackage "hs-bibutils") >>= debianize
  _hscolour <-  (hackage "hscolour") >>= debianize >>= flag (P.RelaxDep "hscolour") >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hse_cpp <-  (hackage "hse-cpp") >>= debianize >>= inGroups ["happstack", "important"]
  _hsemail <-  (hackage "hsemail") >>= debianize -- (rel release [] [P.DebVersion "1.7.1-2build2"])
  _hslogger <-  (hackage "hslogger") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "important"]
  _hslua <-  (hackage "hslua") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _hsOpenSSL <-  (hackage "HsOpenSSL"
                              >>= flag (P.DevelDep "libssl-dev")
                              >>= flag (P.DevelDep "libcrypto++-dev")) >>= debianize
  _hsp <-  (hackage "hsp" >>= flag (P.BuildDep "hsx2hs")) >>= debianize >>= inGroups ["happstack", "important"]
  _hspec <-  (hackage "hspec") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_core <-  (hackage "hspec-core") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_discover <-  (hackage "hspec-discover") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_expectations <-  (hackage "hspec-expectations") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_meta <-  (hackage "hspec-meta") >>= debianize
  _hsSyck <-  (hackage "HsSyck") >>= debianize
  _hStringTemplate <- hackage "HStringTemplate" >>= debianize
  _hsx2hs <- {- git "https://github.com/seereason/hsx2hs.git" [] -}
             {- git "file:///home/dsf/git/hsx2hs" [] -}
             hackage "hsx2hs" >>= {- patch $(embedFile "patches/hsx2hs.diff") >>= -}
             flag (P.CabalDebian ["--executable", "hsx2hs",
                                  "--conflicts", "hsx2hs:haskell-hsx-utils",
                                  "--replaces", "hsx2hs:haskell-hsx-utils",
                                  "--provides", "hsx2hs:haskell-hsx-utils"]) >>=
             debianize >>=
             inGroups ["happstack", "lens", "important"]
  _hsx_jmacro <-  (git "https://github.com/Happstack/hsx-jmacro.git" []) >>= debianize >>= inGroups ["happstack", "lens", "important"]
  _hsyslog <-  (hackage "hsyslog") >>= debianize
  _htf <-  (hackage "HTF" >>= flag (P.BuildDep "cpphs")) >>= debianize
  _html <-  (hackage "html"
                             >>= tflag (P.DebVersion "1.0.1.2-7")
                             >>= pflag (P.DebVersion "1.0.1.2-5")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _html_entities <- darcs ("http://src.seereason.com/html-entities")
  _html_xml_utils <- apt "sid" "html-xml-utils"
  _http_client <-  (hackage "http-client") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
  _http_client_tls <-  (hackage "http-client-tls") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
      -- Deprecated in favor of http-conduit
      -- ,  (hackage "http-client-conduit") >>= debianize
      -- Deprecated in favor of conduit-extra
      -- ,  (hackage "attoparsec-conduit") >>= debianize
      -- ,  (hackage "blaze-builder-conduit") >>= debianize
      -- ,  (hackage "zlib-conduit") >>= debianize
  _http_common <-  (hackage "http-common") >>= debianize >>= inGroups ["platform", "happstack", "important"]
  _http_conduit <-  (hackage "http-conduit") >>= debianize >>= inGroups ["conduit", "important"]
  _http_date <-  (hackage "http-date") >>= debianize
  _http_media <-  (hackage "http-media") >>= debianize >>= inGroups ["servant"]
  _http <-  (hackage "HTTP") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _http2 <-  (hackage "http2") >>= debianize
  _http_streams <-  (hackage "http-streams") >>= debianize >>= inGroups ["platform", "appraisalscribe", "important"]
  _http_types <-  (hackage "http-types" >>= flag (P.CabalPin "0.8.6")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"] -- web-routes specifies << 0.9
  _hUnit <-  (hackage "HUnit") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _hunt <-  (git "https://github.com/hunt-framework/hunt.git" [] >>= cd "hunt-searchengine" ) >>= debianize >>= skip (Reason "No instance for (Foldable t0) arising from a use of ‘elem’")
  _hxt_charproperties <-  (hackage "hxt-charproperties") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hxt <-  (hackage "hxt" >>= flag (P.CabalDebian ["--cabal-flags", "network-uri"])) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hxt_regex_xmlschema <-  (hackage "hxt-regex-xmlschema") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hxt_unicode <-  (hackage "hxt-unicode") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- ,  (darcs "haskell-tiny-server" ("http://src.seereason.com/tiny-server") >>= flag (P.BuildDep "hsx2hs")
               --                >>= flag (P.SkipPackage {- has a "derives SafeCopy" -})) >>= debianize
  _i18n <-  (hackage "i18n" >>= flag (P.DebVersion "0.3-1~hackage1")) >>= debianize >>= skip (Reason "Could not find module ‘System.IO.UTF8’")
  _iconv <-  (hackage "iconv") >>= debianize
  _idris <-  hackage "idris" >>=
             -- patch $(embedFile "patches/idris.diff") -- adds *.idr to extra-source-files >>=
             flag (P.BuildDep "libgc-dev") >>=
             flag (P.CabalDebian ["--default-package", "idris"]) >>=
             debianize >>=
             skip (Reason "Unmet build dependencies: libghc-optparse-applicative-dev (<< 0.12)")
  -- incremental_sat_solver = pure $ P.Package { P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs ("http://src.seereason.com/haskell-incremental-sat-solver-debian")) , P.flags = [] }
  _incremental_sat_solver <-  (git "https://github.com/seereason/incremental-sat-solver" []) >>= debianize
  _indents <-  (hackage "indents") >>= debianize
  _instant_generics <- hackage "instant-generics" >>= flag (P.SkipVersion "0.3.7") >>= debianize >>= broken
  _intervals <-  (hackage "intervals") >>= debianize
  _ioRefCAS <- (hackage "IORefCAS") >>= debianize >>= skip (Reason "Version 0.2.0.1 build fails")
  _io_storage <-  (hackage "io-storage" >>= pflag (P.DebVersion "0.3-2") >>= tflag (P.DebVersion "0.3-5")) >>= debianize
  _io_streams <-  (git "https://github.com/snapframework/io-streams" []) >>= debianize -- pull request to allow atto-parsec-0.13
  _iproute <-  (hackage "iproute") >>= debianize
  _ircbot <-  (hackage "ircbot") >>= debianize >>= inGroups ["happstack", "important"]
  _irc <-  (hackage "irc") >>= debianize
  _iso3166_country_codes <-  (hackage "iso3166-country-codes") >>= debianize
  _ixset <-  (git "https://github.com/Happstack/ixset.git" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"] -- ,  (hackage "ixset") >>= debianize
  _ixset_typed <-  (hackage "ixset-typed") >>= debianize >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _jmacro <-  (hackage "jmacro") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= inGroups ["happstack", "lens", "th-path", "important"]
  _jmacro_rpc <- hackage "jmacro-rpc" >>= inGroups ["happstack", "important"] >>= debianize >>= broken
  _jmacro_rpc_happstack <- hackage "jmacro-rpc-happstack" >>= flag (P.SkipVersion "0.2.1") >>= debianize >>= broken -- Really just waiting for jmacro-rpc
  _jquery <- apt "sid" "jquery" >>= skip (Reason "Missing dependency node-source-map") {- >>= patch $(embedFile "patches/jquery.diff") -} -- Revert to version 1.7.2+dfsg-3, version 1.7.2+dfsg-3.2 gives us a nearly empty jquery.min.js 
  _jquery_goodies <- apt "sid" "jquery-goodies" >>= patch $(embedFile "patches/jquery-goodies.diff")
               -- We want to stick with jqueryui-1.8 for now, so create
               -- packages with the version number embedded in the name.
  _jqueryui18 <- darcs ("http://src.seereason.com/jqueryui18")
  _js_flot <-  (hackage "js-flot") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _js_jquery <-  (hackage "js-jquery") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _jsaddle <- git "https://github.com/ghcjs/jsaddle" [] >>= debianize >>= inGroups ["ghcjs-libs"]
  _json <-  (hackage "json") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "seereason", "important"] -- darcs "haskell-json" (repo ++ "/haskell-json")
  _juicyPixels <-  (hackage "JuicyPixels") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _jwt <-  (hackage "jwt") >>= debianize >>= inGroups [ "authenticate", "important"] -- dependency of happstack-authenticate-2
  _kan_extensions <- hackage "kan-extensions" >>= flag (P.CabalPin "4.2.3") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _keys <-  (hackage "keys") >>= debianize
  _language_css <-  (hackage "language-css" >>= flag (P.DebVersion "0.0.4.1-1~hackage1")) >>= debianize
  _language_ecmascript <-  (hackage "language-ecmascript") >>= debianize
  _language_haskell_extract <-  (hackage "language-haskell-extract") >>= debianize
  _language_java <-  (hackage "language-java" >>= flag (P.BuildDep "alex")) >>= debianize
  _language_javascript <-  (hackage "language-javascript"
                              >>= flag (P.BuildDep "happy")
                              >>= flag (P.BuildDep "alex")
                           ) >>= debianize
  _largeword <-  (hackage "largeword") >>= debianize
               -- No cabal file
               -- ,  (git "haskell-logic-hs" "https://github.com/smichal/hs-logic") >>= debianize
           {-  , apt "wheezy" "haskell-leksah"
               , apt "wheezy" "haskell-leksah-server" -- for leksah -}
  _latex <-  (hackage "latex") >>= debianize
  -- This commit adds the sequence numbers we need to generated function parameters
  _lens <- git "https://github.com/ekmett/lens" [Commit "950eb5be34fb40bf0111ded6bc91c1ffcd2a786b"] >>=
           apply (replacement "lens" "microlens-compat") >>=
           debianize >>= inGroups ["lens", "ghcjs-libs", "ghc-libs"]
  -- _lens <-  (hackage "lens") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _lens_compat <-  (git "https://github.com/ddssff/lens-compat" []) >>= debianize >>= inGroups ["lens"]
  _lens_family_core <-  (hackage "lens-family-core") >>= debianize
  _lens_family <-  (hackage "lens-family") >>= debianize
  _lens_family_th <-  (hackage "lens-family-th") >>= debianize
      -- These five fail because representable-functors fails, it wasn't updated
      -- for the consolidation of comonad
      {-
      ,  (hackage "representable-functors" {- >>= patch $(embedFile "patches/representable-functors.diff") -}) >>= debianize
      ,  (hackage "representable-tries") >>= debianize
      ,  (hackage "algebra") >>= debianize
      ,  (hackage "universe" {- >>= patch $(embedFile "patches/universe.diff") -}) >>= debianize
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
  _lifted_async <-  (hackage "lifted-async") >>= debianize >>= inGroups ["ghcjs-comp"]
  _lifted_base <-  (hackage "lifted-base") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _linear <-  (hackage "linear") >>= debianize >>= skip (Reason "Requires bytes")
  _list_extras <-  (hackage "list-extras") >>= debianize
  _listLike <-  (git "https://github.com/ddssff/ListLike" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] --  (hackage "ListLike") >>= debianize
  _list_tries <-  (hackage "list-tries" {- >>= patch $(embedFile "patches/list-tries.diff") -}) >>= debianize >>= inGroups ["happstack", "important"] -- version 0.5.2 depends on dlist << 0.7
  _loch_th <-  (hackage "loch-th") >>= debianize
  _logging <- hackage "logging" >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs", "important"]
  _logic_classes <-  (git "https://github.com/seereason/logic-classes" []) >>= debianize >>= inGroups ["seereason", "important"]
  _logic_TPTP <-  (hackage "logic-TPTP") >>= debianize
                 >>= patch $(embedFile "patches/logic-TPTP.diff")
                 >>= flag (P.BuildDep "alex")
                 >>= flag (P.BuildDep "happy")
  -- logic_TPTP = pure $ P.Package { P.spec = Debianize'' (Patch (Hackage "logic-TPTP") $(embedFile "patches/logic-TPTP.diff")) Nothing, P._flags = [ P.BuildDep "alex", P.BuildDep "happy" ] }
               -- , apt "sid" "haskell-maybet"
  _logict <- createPackage (Debianize'' (Hackage "logict") Nothing) mempty [] >>= inGroups ["ghcjs-libs", "ghc-libs"] :: TSt PackageId
  _loop <-  (hackage "loop") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _lucid <-  (hackage "lucid") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _maccatcher <-  (hackage "maccatcher"
                              >>= pflag (P.DebVersion "2.1.5-3")
                              >>= tflag (P.DebVersion "2.1.5-5build1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _magic <-  (hackage "magic" >>= flag (P.DevelDep "libmagic-dev")) >>= debianize
           {-  , P.Package { P._spec = Quilt (Apt "wheezy" "magic-haskell") (Darcs ("http://src.seereason.com/magic-quilt"))
                           , P._flags = mempty } -}
  _mainland_pretty <-  (hackage "mainland-pretty") >>= debianize
  _makedev <- apt "wheezy" "makedev" >>= skip (Reason "Use standard")
  _markdown <-  (hackage "markdown" {- >>= patch $(embedFile "patches/markdown.diff") -}) >>= debianize >>= inGroups ["happstack", "important"]
  _markdown_unlit <-  (hackage "markdown-unlit" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize
  _matrix <-  (hackage "matrix") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- ,  (hackage "hlatex") >>= debianize
  _maybeT <-  (hackage "MaybeT" >>= flag (P.DebVersion "1.2-6")) >>= debianize >>= skip (Reason "Could not deduce (Applicative (MaybeT m))")
  _memoize <-  (hackage "memoize") >>= debianize
  _memory <-  (hackage "memory") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _memoTrie <-  (hackage "MemoTrie") >>= debianize
  _microlens <- hackage "microlens" >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_mtl <- hackage "microlens-mtl" >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_th <- hackage "microlens-th" >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_compat <- git "https://github.com/seereason/microlens-compat.git" [] >>=
                       apply (replacement "microlens-compat" "lens") >>=
                       debianize >>= inGroups ["ghc-libs", "ghcjs-libs", "th-path", "important"]
  _mime <- darcs ("http://src.seereason.com/haskell-mime")
  _mime_mail <-  (git "https://github.com/snoyberg/mime-mail.git" [] >>= cd "mime-mail") >>= debianize >>= inGroups [ "authenticate", "important"]
  _mime_types <-  (hackage "mime-types") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
  _mirror <-  (git "https://github.com/seereason/mirror" []
                        >>= flag (P.CabalDebian ["--executable", "debian-mirror"])) >>= debianize >>= inGroups ["autobuilder-group"]
  _missingH <-  (hackage "MissingH") >>= debianize
  _mmap <-  (hackage "mmap") >>= debianize
  _mmorph <-  (hackage "mmorph") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _module_management <-  (git "https://github.com/seereason/module-management" [] >>= flag (P.BuildDep "rsync")) >>= debianize
  _monadCatchIO_mtl <-  (hackage "MonadCatchIO-mtl" >>= patch $(embedFile "patches/monadcatchio-mtl.diff")) >>= debianize
  _monadCatchIO_transformers <-  (hackage "MonadCatchIO-transformers" >>= qflag (P.DebVersion "0.3.0.0-2build2")) >>= debianize
  _monad_control <-  (hackage "monad-control") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _monadcryptorandom <-  (hackage "monadcryptorandom") >>= debianize >>= inGroups ["authenticate", "important"]
  _monadLib <-  (hackage "monadLib") >>= debianize
               -- Putting this in our repo can cause problems, because when it is
               -- installed some packages can't compile unless you add package
               -- qualifiers to their imports.  For this reason, when we run the
               -- autobuilder with the --lax flag we usually get a failure from
               -- some package that builds after monads-tf got installed.  On the
               -- other hand, without monads-tf we lose this dependency chain:
               -- monads-tf <- options <- fay.
  _monadlist <-  (hackage "monadlist") >>= debianize >>= inGroups ["clckwrks", "important"]
  _monad_logger <-  (hackage "monad-logger") >>= debianize >>= inGroups [ "authenticate", "important"]
  _monad_loops <-  (hackage "monad-loops") >>= debianize >>= inGroups [ "authenticate", "important"]
  _monad_parallel <-  (hackage "monad-parallel") >>= debianize
  _monad_par <-  (hackage "monad-par") >>= debianize
  _monad_par_extras <-  (hackage "monad-par-extras") >>= debianize
  _monadRandom <-  (hackage "MonadRandom") >>= debianize
  _monads_tf <-  (hackage "monads-tf") >>= debianize
  _monad_task <- hackage "monad-task" >>= debianize >>= skip (Reason "0.1.0 requires transformers<4")
  _monoid_transformer <-  (hackage "monoid-transformer") >>= debianize -- apt (rel release "wheezy" "quantal") "haskell-monoid-transformer"
  _mtl <-  (hackage "mtl") >>= debianize >>= inGroups ["platform"]
  _mtl_compat <-  (hackage "mtl-compat") >>= debianize
  _mtl_unleashed <-  (git "https://github.com/seereason/mtl-unleashed" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _mtlparse <-  (hackage "mtlparse") >>= debianize
  _multimap <-  (hackage "multimap") >>= debianize
  _multipart <-  (hackage "multipart") >>= debianize >>= inGroups ["platform"]
  _multiset <-  (hackage "multiset") >>= debianize
  _murmur_hash <-  (hackage "murmur-hash") >>= debianize
  _mwc_random <-  (hackage "mwc-random") >>= debianize
  _mysql <- hackage "mysql" >>= debianize
  _mysql_simple <- hackage "mysql-simple" >>= debianize
  _nano_hmac <- hackage "nano-hmac" >>= patch $(embedFile "patches/nano-hmac.diff") >>= flag (P.DebVersion "0.2.0ubuntu1") >>= debianize
  _nanospec <-  (hackage "nanospec" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize -- avoid circular dependency nanospec <-> silently
  _nats <-  (hackage "nats") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _network_conduit <-  (hackage "network-conduit") >>= debianize
  _network <-  (hackage "network") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _network_info <-  (hackage "network-info") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _network_uri <-  (hackage "network-uri") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _newtype_generics <-  (hackage "newtype-generics") >>= debianize >>= inGroups ["autobuilder-group"]
  -- _nodejs <- uri "https://nodejs.org/dist/v0.12.7/node-v0.12.7.tar.gz" "5523ec4347d7fe6b0f6dda1d1c7799d5" >>=
  --            debdir (Git "https://github.com/seereason/nodejs-debian" []) >>= inGroups ["ghcjs-comp"]
  _nodejs <- uri "https://deb.nodesource.com/node_5.x/pool/main/n/nodejs/nodejs_5.6.0.orig.tar.gz" "6f7c2cec289a20bcd970240dd63c1395" >>=
            debdir (Uri "https://deb.nodesource.com/node_5.x/pool/main/n/nodejs/nodejs_5.6.0-1nodesource1~trusty1.debian.tar.gz" "6272a4f41058ee7cf9fa1a1696beb343") >>= inGroups ["ghcjs-comp"]
  _numeric_extras <-  (hackage "numeric-extras") >>= debianize
  _numInstances <-  (hackage "NumInstances") >>= debianize
  _objectName <-  (hackage "ObjectName") >>= debianize
  _old_locale <-  (hackage "old-locale") >>= debianize
  _old_time <-  (hackage "old-time") >>= debianize
  _oo_prototypes <-  (hackage "oo-prototypes") >>= debianize
  _openGL <-  (hackage "OpenGL" >>= inGroups ["gl"] >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize >>= skip (Reason "too old for openglraw")
  _openGLRaw <-  (hackage "OpenGLRaw" >>= inGroups ["gl"]
                     >>= flag (P.DevelDep "libgl1-mesa-dev")) >>= debianize
  _openid <-  (hackage "openid" >>= patch $(embedFile "patches/openid.diff")) >>= debianize >>= skip (Reason "No instance for (Applicative (Assoc m))")
           {-  , P.Package { P.spec =  (Patch (Hackage "openid") $(embedFile "patches/openid-ghc76.diff")) >>= Debianize
                           , P.flags = [] } -}
  _openssl_streams <-  (hackage "openssl-streams") >>= debianize >>= inGroups ["platform"]
  _operational <- hackage "operational" >>= flag P.OmitLTDeps >>= debianize
  _optparse_applicative <-  (hackage "optparse-applicative") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _ordered <-  (hackage "ordered") >>= debianize
  _pandoc <-  (-- git "https://github.com/jgm/pandoc" [Commit "360e35459fb4bf2611ad414774485b8f553201dd"]
                      hackage "pandoc" -- For syb-0.6 and aeson-0.10 we need a release newer than 1.15.0.6
                             >>= patch $(embedFile "patches/pandoc.diff")
                             -- >>= flag (P.RelaxDep "libghc-pandoc-doc")
                             >>= flag (P.BuildDep "alex")
                             >>= flag (P.BuildDep "happy")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _pandoc_types <-  (hackage "pandoc-types") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _pango <-  (hackage "pango") >>= debianize
  _parallel <-  (hackage "parallel") >>= debianize >>= inGroups ["platform"]
  _parseargs <-  (hackage "parseargs") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- , apt (rel release "wheezy" "quantal") "haskell-parsec2" >>= patch $(embedFile "patches/parsec2.diff")
  _parsec <-  (hackage "parsec" >>= apply (substitute "parsec2" "parsec3")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _parse_dimacs <-  (hackage "parse-dimacs") >>= debianize
  _parsers <-  (hackage "parsers" {- >>= patch $(embedFile "patches/parsers.diff") -}) >>= debianize
  _pbkdf2 <-  (hackage "PBKDF2") >>= debianize >>= skip (Reason "[libghc-multiset-dev (<< 0.3)] -> []")
               -- , apt (rel release "wheezy" "quantal") "haskell-pcre-light"
  _pcre_light <-  (hackage "pcre-light"
                              -- >>= patch $(embedFile "patches/pcre-light.diff")
                              >>= flag (P.DevelDep "libpcre3-dev")) >>= debianize >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _pem <-  (hackage "pem") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _permutation <-  (hackage "permutation") >>= debianize
  _pipes <-  (hackage "pipes") >>= debianize
  _placeholders <-  (hackage "placeholders") >>= debianize
  _plugins_auto <-  (hackage "plugins-auto" >>= patch $(embedFile "patches/plugins-auto.diff")) >>= debianize >>= skip (Reason "Couldn't match expected type ‘Int#’ with actual type ‘Int’")
  _plugins <-  hackage "plugins" {->>= patch $(embedFile "patches/plugins.diff")-} >>= debianize
  _plugins_ng <-  (git "https://github.com/ddssff/plugins-ng" []) >>= debianize >>= skip (Reason "needs fsnotify << 0.2")
  _po4a <- apt "wheezy" "po4a" >>= skip (Reason "use standard trusty version")
  _pointed <- hackage "pointed" >>= flag (P.CabalPin "4.2.0.2") >>= debianize -- waiting for kan-extensions >= 5
  _pointedlist <-  (hackage "pointedlist") >>= debianize
  _polyparse <-  (hackage "polyparse") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _prelude_extras <-  (hackage "prelude-extras") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _prettyclass <-  (hackage "prettyclass") >>= debianize >>= inGroups ["authenticate", "important"]
  _pretty_show <-  (hackage "pretty-show" >>= flag (P.BuildDep "happy")) >>= debianize
  _primitive <-  (hackage "primitive") >>= debianize
  _process_extras <-
       (git "https://github.com/seereason/process-extras" []) >>= debianize
                   >>= apply (substitute "process-extras" "process-listlike")
                   >>= inGroups ["ghcjs-libs", "autobuilder-group"]
  _processing <-  (hackage "processing") >>= debianize >>= skip (Reason "[libghc-multiset-prof (<< 0.3)] -> []")
  _profunctors <-  (hackage "profunctors"
                     >>= apply (replacement "profunctors" "profunctors-extras")) >>= debianize
                     >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _propLogic <-  (git "https://github.com/ddssff/PropLogic" []) >>= debianize
  _pseudomacros <-  (hackage "pseudomacros") >>= debianize
  _psQueue <- (hackage "PSQueue"
                              >>= pflag (P.DebVersion "1.1-2")
                              >>= qflag (P.DebVersion "1.1-2build2")
                              >>= sflag (P.DebVersion "1.1-1")
                              >>= tflag (P.DebVersion "1.1-4")) >>= debianize >>= wskip
  _psqueues <- hackage "psqueues" >>= debianize
  _publicsuffixlist <-  (hackage "publicsuffixlist" >>= tflag (P.DebVersion "0.1-1build4")) >>= debianize >>= inGroups ["platform"]
  _pureMD5 <-  (hackage "pureMD5" >>= tflag (P.DebVersion "2.1.2.1-3build3")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _pwstore_purehaskell <-  (hackage "pwstore-purehaskell"
                              >>= flag (P.SkipVersion "2.1.2")
                              -- >>= patch $(embedFile "patches/pwstore-purehaskell.diff")
                              -- >>= flag (P.DebVersion "2.1-1~hackage1")
                           ) >>= debianize
               -- Retired
               -- , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
  _quickCheck <-  (hackage "QuickCheck" >>= flag (P.BuildDep "libghc-random-prof") {->>= flag (P.CabalDebian ["--no-tests"])-}) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _quickcheck_gent <-  (hackage "QuickCheck-GenT") >>= debianize >>= skip (Reason "Unmet build dependencies: libghc-quickcheck2-dev (<< 2.7) libghc-quickcheck2-prof (<< 2.7)")
  _quickcheck_instances <-  (hackage "quickcheck-instances") >>= debianize
  _quickcheck_io <-  (hackage "quickcheck-io") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- quickCheck1 =  (hackage "QuickCheck" >>= flag (P.CabalPin "1.2.0.1") >>= flag (P.DebVersion "1.2.0.1-2") >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize
  _random <-  (hackage "random" >>= flag (P.SkipVersion "1.0.1.3")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"] -- 1.1.0.3 fixes the build for ghc-7.4.2 / base < 4.6
  _reducers <- hack "reducers"
  _reflection <-  (hackage "reflection") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] -- avoid rebuild
  _reform_blaze <- git "https://github.com/Happstack/reform-blaze.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _reform <- git "https://github.com/Happstack/reform.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _reform_hamlet <- git "https://github.com/Happstack/reform-hamlet.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _reform_happstack <- git "https://github.com/Happstack/reform-happstack.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _reform_hsp <- git "https://github.com/Happstack/reform-hsp.git" [] >>= flag (P.BuildDep "hsx2hs") >>= debianize >>= inGroups ["happstack", "important"]
  _regex_applicative <-  (hackage "regex-applicative") >>= debianize
  _regex_base <-  (hackage "regex-base"
                             >>= tflag (P.DebVersion "0.93.2-4")
                             >>= pflag (P.DebVersion "0.93.2-2")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _regex_compat <-  (hackage "regex-compat"
                             >>= pflag (P.DebVersion "0.95.1-2")
                             >>= tflag (P.DebVersion "0.95.1-4")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _regex_compat_tdfa <-  (hackage "regex-compat-tdfa") >>= debianize
  _regex_pcre_builtin <-  (hackage "regex-pcre-builtin"
                              -- Need to email Audrey Tang <audreyt@audreyt.org> about this.
                              >>= patch $(embedFile "patches/regex-pcre-builtin.diff")
                              >>= flag (P.DevelDep "libpcre3-dev")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _regex_posix <-  (hackage "regex-posix" >>= tflag (P.DebVersion "0.95.2-3")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _regexpr <-  (hackage "regexpr" >>= flag (P.DebVersion "0.5.4-5build1")) >>= debianize
  _regex_tdfa <-  (hackage "regex-tdfa"
                          -- Although it might be nice to start using regex-tdfa-rc everywhere
                          -- we are using regex-tdfa, the cabal package names are different so
                          -- packages can't automatically start using regex-tdfa-rc.
                          >>= apply (substitute "regex-tdfa" "regex-tdfa-rc")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _regex_tdfa_rc <-  (hackage "regex-tdfa-rc"
                              >>= apply (substitute "regex-tdfa-rc" "regex-tdfa")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- reified_records =  (hackage "reified-records" >>= patch $(embedFile "patches/reified-records.diff")) >>= debianize
  _reified_records <-  (hg "https://bitbucket.org/ddssff/reified-records") >>= debianize
  _resource_pool <- hackage "resource-pool" >>= debianize
  _resourcet <-  (hackage "resourcet") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _rJson <-  hackage "RJson" >>=
             patch $(embedFile "patches/RJson.diff") >>=
             wflag (P.DebVersion "0.3.7-1~hackage1") >>=
             debianize >>=
             skip (Reason "Ambiguous occurrence ‘escape’")
  _rsa <-  (hackage "RSA") >>= debianize >>= inGroups ["authenticate", "important"]
  _rss <-  (hackage "rss" {- >>= patch $(embedFile "patches/rss.diff") -}) >>= debianize
  _safecopy <-  (git "https://github.com/acid-state/safecopy" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _safe <-  (hackage "safe") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _safeSemaphore <-  (hackage "SafeSemaphore") >>= debianize >>= inGroups ["happstack", "important"]
  _sandi <-  (hackage "sandi") >>= debianize -- replaces dataenc
  _sat <-  (hackage "sat"
                              >>= patch $(embedFile "patches/sat.diff")
                              >>= flag (P.DebVersion "1.1.1-1~hackage1")) >>= debianize
  _scientific <-  (hackage "scientific") >>= debianize
               -- ,  (hackage "arithmoi" >>= flag (P.BuildDep "llvm-dev")) >>= debianize
  _scotty <- hackage "scotty" {- >>= patch $(embedFile "patches/scotty.diff") -} >>= debianize -- allow warp-3.1.3
  _seclib <-  (darcs ("http://src.seereason.com/seclib")) >>= debianize >>= skip (Reason "No instance for (Applicative (Sec s))")
  _securemem <-  (hackage "securemem") >>= debianize
  _seereason_base <-
       (git "https://github.com/seereason/seereason-base" []) >>= debianize >>= inGroups ["seereason", "important"]
  _seereason_keyring <- darcs ("http://src.seereason.com/seereason-keyring") >>= flag (P.UDeb "seereason-keyring-udeb")
  _seereason_ports <-  (git "https://github.com/seereason/seereason-ports" []) >>= debianize
  _semigroupoids <-  (hackage "semigroupoids"
                     >>= apply (replacement "semigroupoids" "semigroupoid-extras")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _semigroups <-  (hackage "semigroups") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _sendfile <-  (hackage "sendfile" >>= tflag (P.DebVersion "0.7.9-1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _servant <- hackage "servant" >>= debianize >>= inGroups ["servant"]
  _servant_server <- hackage "servant-server" >>= debianize >>= inGroups ["servant-server"] >>= inGroups ["servant"]
  _setenv <-  (hackage "setenv") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _set_extra <-  (darcs ("http://src.seereason.com/set-extra")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- I don't think we use this any more
               -- ,  (darcs "haskell-old-exception" ("http://src.seereason.com/old-exception")) >>= debianize
  _set_monad <-  (hackage "set-monad") >>= debianize
  _sha <-  (hackage "SHA") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] -- apt (rel release "wheezy" "quantal") "haskell-sha"
  _shake <-  (hackage "shake") >>= debianize >>= inGroups ["ghcjs-libs"]
  _shakespeare <-  (hackage "shakespeare") >>= debianize >>= inGroups ["happstack", "important", "ghcjs-libs"]
  _shakespeare_js <-  (hackage "shakespeare-js") >>= debianize
  _shellmate <- hack "shellmate"
  _shelly <-  (hackage "shelly" >>= flag (P.CabalPin "1.6.3.4")) >>= debianize >>= inGroups ["ghcjs-comp"]
  _showplease <-  (git "https://github.com/ddssff/showplease" []) >>= debianize
  _silently <-  (hackage "silently") >>= debianize
  _simple_reflect <-  (hackage "simple-reflect") >>= debianize
  _simple_sendfile <-  (hackage "simple-sendfile") >>= debianize
  _singletons <-  (hackage "singletons") >>= debianize
  _smallcheck <-  (hackage "smallcheck") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _smtpClient <-  (hackage "SMTPClient") >>= debianize
  _snap_core <- hackage "snap-core" >>= debianize
  _snap_server <- hackage "snap-server" >>= debianize
  _socks <-  (hackage "socks") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _sodium <- hackage "sodium" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _sourcemap <-  (hackage "sourcemap") >>= debianize >>= inGroups ["happstack", "important"]
  _spine <-  (hackage "spine") >>= debianize
  _split <- hackage "split" >>= {-patch $(embedFile "patches/split.diff") >>= tflag (P.DebVersion "0.2.2-1") >>=-} debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _spoon <-  hackage "spoon" >>= debianize
  _srcloc <-  (hackage "srcloc") >>= debianize
  _stateVar <-  (hackage "StateVar") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _stb_image <-  (hackage "stb-image") >>= debianize
  _stm_chans <-  (hackage "stm-chans") >>= debianize >>= inGroups ["platform"]
  _stm <-  (hackage "stm") >>= debianize >>= inGroups ["platform"]
  _streaming_commons <-  (hackage "streaming-commons") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important"]
  _strict <-  (hackage "strict"
                              >>= pflag (P.DebVersion "0.3.2-2")
                              >>= tflag (P.DebVersion "0.3.2-7")) >>= debianize -- apt (rel release "wheezy" "quantal") "haskell-strict" -- for leksah
               -- ,  (hackage "strict-concurrency" >>= wflag (P.DebVersion "0.2.4.1-2")) >>= debianize
  _strict_io <-  (hackage "strict-io") >>= debianize >>= inGroups ["GenI"] >>= skip (Reason "dependencies are missing: deepseq >=1.1 && <1.4")
  _stringable <-  (hackage "stringable") >>= debianize -- this can be done with listlike-instances
  _string_conversions <- hackage "string-conversions" >>= debianize >>= inGroups ["servant"]
  _stringbuilder <-  (hackage "stringbuilder") >>= debianize
  _stringsearch <-  (hackage "stringsearch") >>= debianize
  _sunroof_compiler <-  (git "http://github.com/ku-fpg/sunroof-compiler" [] >>= patch $(embedFile "patches/sunroof-compiler.diff")) >>= debianize >>= skip (Reason "Setup.hs:3:1: parse error on input ‘import’")
  _syb <-  (hackage "syb") >>= debianize >>= inGroups ["platform"] -- haskell-src-meta requres syb<0.6
  _syb_with_class <-  (git "http://github.com/seereason/syb-with-class" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] -- Version 0.6.1.5 tries to derive typeable instances when building rjson, which is an error for ghc-7.8
  _syb_with_class_instances_text <-
                  (hackage "syb-with-class-instances-text"
                              >>= pflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.SkipVersion "0.0.1-3")
                              >>= tflag (P.DebVersion "0.0.1-6build1")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= broken
  _system_fileio <-  (hackage "system-fileio") >>= debianize
  _system_filepath <-  (hackage "system-filepath") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- , P.Package { P.spec = Debianize'' (Patch (Hackage "xml-enumerator") $(embedFile "patches/xml-enumerator.diff")) Nothing , P.flags = [] }
  _tagged <-  (hackage "tagged") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _tagshare <-  (hackage "tagshare") >>= debianize
  _tagsoup <-  (hackage "tagsoup") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _tagstream_conduit <-  (hackage "tagstream-conduit") >>= debianize >>= inGroups ["conduit", "authenticate", "important"]
  _tar <-  (hackage "tar" >>= flag (P.CabalDebian ["--cabal-flags", "-old-time"])) >>= debianize
           {-  -- This is built into ghc-7.8.3
               ,  (hackage "terminfo"
                                        >>= flag (P.DevelDep "libncurses5-dev")
                                        >>= flag (P.DevelDep "libncursesw5-dev")) >>= debianize -}
  _tasty <-  (hackage "tasty") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _tasty_hunit <-  (hackage "tasty-hunit") >>= debianize
  _tasty_quickcheck <-  (hackage "tasty-quickcheck") >>= debianize
  _tasty_smallcheck <-  (hackage "tasty-smallcheck") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _template_default <-  (hackage "template-default" >>= patch $(embedFile "patches/template-default.diff")) >>= debianize >>= skip (Reason "Not in scope: data constructor ‘ClassP’")
  _temporary <-  (hackage "temporary") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _tensor <- hackage "Tensor" >>= tflag (P.DebVersion "1.0.0.1-2") >>= debianize >>= broken
  _test_framework <-  (hackage "test-framework") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _test_framework_hunit <-  (hackage "test-framework-hunit") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _test_framework_quickcheck2 <-  (git "https://github.com/seereason/test-framework" [] >>= cd "quickcheck2") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _test_framework_quickcheck <-  (git "https://github.com/seereason/test-framework" [] >>= cd "quickcheck") >>= debianize >>= skip (Reason "confused debian dependencies")
  _test_framework_smallcheck <-  (hackage "test-framework-smallcheck") >>= debianize
  _test_framework_th <-  (hackage "test-framework-th" >>= tflag (P.DebVersion "0.2.4-1build4")) >>= debianize
  _testing_feat <- hackage "testing-feat" >>= {-patch $(embedFile "patches/testing-feat.diff") >>=-} debianize
  _texmath <-  (hackage "texmath") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _text_binary <- hackage "text-binary" >>= debianize >>= inGroups ["ghcjs-libs"]
  _text <-  (hackage "text" >>= flag (P.CabalDebian ["--cabal-flags", "-integer-simple"]) >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize >>= inGroups ["platform"]
  _text_icu <-  (hackage "text-icu" >>= flag (P.DevelDep "libicu-dev")) >>= debianize
  _text_show <-  (hackage "text-show") >>= debianize
  _text_stream_decode <-  (hackage "text-stream-decode" >>= patch $(embedFile "patches/text-stream-decode.diff")) >>= debianize >>= inGroups ["conduit", "important"]
  _tf_random <-  (hackage "tf-random") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _th_alpha <-  (git "https://github.com/jkarni/th-alpha.git" []) >>= debianize
  _th_context <-  (git "http://github.com/seereason/th-context" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _th_desugar <- {-(git "http://github.com/goldfirere/th-desugar" [])-} hackage "th-desugar" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _th_expand_syns <-  (hackage "th-expand-syns") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  -- th_instance_reification =  (git "https://github.com/seereason/th-instance-reification.git" []) >>= debianize
  _th_kinds_fork <-  (git "http://github.com/ddssff/th-kinds-fork" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _th_lift <-  (hackage "th-lift") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _th_orphans <-  (hackage "th-orphans") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _th_typegraph <-  (git "http://github.com/seereason/th-typegraph" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _threads <-  (hackage "threads") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _th_reify_many <-  (hackage "th-reify-many") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _time_compat <-  (hackage "time-compat") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _time_locale_compat <-  (hackage "time-locale-compat") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _tinymce <- apt "wheezy" "tinymce"
  _tls <-  (hackage "tls") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
              -- tls-extra deprecated in favor of tls
              -- ,  (hackage "tls-extra" >>= patch $(embedFile "patches/tls-extra.diff")) >>= debianize
  _transformers_base <- hackage "transformers-base" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _transformers_compat <- hackage "transformers-compat" >>=
                          -- flag (P.CabalPin "0.4.0.4") >>= -- waiting for newer monad-control and monad-parallel
                          {-patch $(embedFile "patches/transformers-compat.diff") >>=-}
                          debianize >>=
                          inGroups ["ghcjs-libs", "ghc-libs"]
  _transformers_free <-  (hackage "transformers-free") >>= debianize
  _traverse_with_class <-  (hackage "traverse-with-class") >>= debianize >>= inGroups ["happstack", "important"]
  _trifecta <-  (hackage "trifecta" {->>= patch $(embedFile "patches/trifecta.diff")-}) >>= debianize
  _tyb <- hackage "TYB" >>= debianize >>= skip (Reason "Needs update for current template-haskell")
  _type_eq <- hackage "type-eq" >>= flag (P.BuildDep "cpphs") >>= debianize
  _uglymemo <-  (hackage "uglymemo") >>= debianize
  _unbounded_delays <-  (hackage "unbounded-delays") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _unexceptionalio <- hackage "unexceptionalio" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _unicode_names <-  (git "https://github.com/seereason/unicode-names" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1")) >>= debianize
  _unicode_properties <-  (git "https://github.com/seereason/unicode-properties" [] >>= flag (P.DebVersion "3.2.0.0-1~hackage1")) >>= debianize
  _unification_fd <-  (hackage "unification-fd" >>= flag (P.SkipVersion "0.8.0")) >>= debianize
  _union_find <-  (hackage "union-find") >>= debianize
               -- ,  (hackage "Elm") >>= debianize
               -- ,  (hackage "elm-server" {- >>= patch $(embedFile "patches/elm-server.diff") -}) >>= debianize
  _uniplate <-  (hackage "uniplate") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _units <-  (hackage "units") >>= debianize >>= skip (Reason "[libghc-singletons-prof (<< 2)] -> []")
  _units_parser <-  (hackage "units-parser") >>= debianize
  _unix_compat <-  (hackage "unix-compat") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _unix_time <-  (hackage "unix-time" {->>= flag (P.CabalDebian ["--no-run-tests"])-}) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"] -- doctest assumes cabal build dir is dist
  _unixutils <-  (git "https://github.com/seereason/haskell-unixutils" []) >>= debianize >>= inGroups ["ghcjs-libs", "autobuilder-group"]
  _unixutils_shadow <-  (hackage "Unixutils-shadow") >>= debianize
  _unordered_containers <-  (hackage "unordered-containers") >>= debianize
               -- Obsolete after ghc-6.10
               -- ,  (hackage "utf8-prelude" >>= flag (P.DebVersion "0.1.6-1~hackage1")) >>= debianize
               -- The GHC in wheezy conflicts with libghc-containers-dev, so we can't build this.
               -- , wonly $  (hackage "containers") >>= debianize
  _urlencoded <-  (hackage "urlencoded" {->>= patch $(embedFile "patches/urlencoded.diff")-}) >>= debianize
  _userid <-  (git "https://github.com/seereason/userid" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "happstack", "important"]
  _utf8_light <-  (hackage "utf8-light") >>= debianize
  _utf8_string <-  (hackage "utf8-string"
                              >>= flag (P.RelaxDep "hscolour")
                              >>= flag (P.RelaxDep "cpphs")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- , P.Package { P.spec = Apt (rel release "wheezy" "quantal") "haskell-utf8-string"
               --             , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
  _utility_ht <-  (hackage "utility-ht") >>= debianize
  _uuid <-  (hackage "uuid") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _uuid_orphans <-  (git "https://github.com/seereason/uuid" [] >>= cd "uuid-orphans") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "clckwrks", "important"]
  _uuid_types <-  (hackage "uuid-types") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _vacuum <-  (hackage "vacuum" >>= flag (P.SkipVersion "2.1.0.1")) >>= debianize >>= skip (Reason "#error Unsupported GHC version in ClosureTypes.hs!")
  _validation <-  (hackage "Validation" >>= patch $(embedFile "patches/validation.diff")) >>= debianize
  _value_supply <-  (hackage "value-supply") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _vault <-  (hackage "vault") >>= debianize
  _vc_darcs <- darcs ("http://src.seereason.com/vc-darcs")
  _vc_git_dired <- git "https://github.com/ddssff/vc-git-dired" []
  _vector_algorithms <-  (hackage "vector-algorithms") >>= debianize
  _vector_binary_instances <- hack "vector-binary-instances"
  _vector <-  (hackage "vector") >>= debianize
  _vector_space <-  (hackage "vector-space") >>= debianize
  _virthualenv <-  (hackage "virthualenv" >>= patch $(embedFile "patches/virthualenv.diff")) >>= debianize >>= skip (Reason "dependencies are missing: filepath >=1.1.0.3 && <1.4")
{-
  _virthualenv <- pure $ P.Package { P._spec = Debianize'' (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff")) Nothing
                                 , P._flags =  mempty
                                 , P._post = [] } :: TSt Package
-}
  _void <-  (hackage "void") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _vty <-  (hackage "vty") >>= debianize
  _wai_app_static <-  (hackage "wai-app-static") >>= debianize >>= inGroups ["servant"]
  _wai <- hackage "wai" {- >>= patch $(embedFile "patches/wai.diff") -} >>= debianize >>= inGroups ["happstack", "important"]
  _wai_extra <-  (hackage "wai-extra") >>= debianize
  _wai_logger <-  (hackage "wai-logger") >>= debianize
  _wai_middleware_static <-  (hackage "wai-middleware-static") >>= debianize
  _warp <-  (hackage "warp") >>= debianize
  _webdriver <-  (git "https://github.com/kallisti-dev/hs-webdriver.git" [{-Commit "0251579c4dd5aebc26a7ac5b300190f3370dbf9d"-} {- avoid rebuild -}]) >>= debianize
  _web_encodings <-  (hackage "web-encodings" >>= patch $(embedFile "patches/web-encodings.diff")) >>= debianize >>= skip (Reason "Deprecated in hackage")
{-
  _web_encodings <- pure $ P.Package { P._spec = Debianize'' (Patch (Hackage "web-encodings") $(embedFile "patches/web-encodings.diff")) Nothing
                                   , P._flags = mempty
                                   , P._post = [] } :: TSt Package
-}
  _web_plugins <-  (git "http://github.com/clckwrks/web-plugins" [] >>= cd "web-plugins") >>= debianize
  _web_routes_boomerang <- git "https://github.com/Happstack/web-routes-boomerang.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _web_routes <- git "https://github.com/Happstack/web-routes.git" [] >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _web_routes_happstack <- git "https://github.com/Happstack/web-routes-happstack.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _web_routes_hsp <- git "https://github.com/Happstack/web-routes-hsp.git" [] >>= debianize >>= inGroups ["happstack", "important"]
{-
  _web_routes_mtl <- git "https://github.com/Happstack/web-routes-mtl.git" [] >>= flag (P.DebVersion "0.20.1-1~hackage1") >>= debianize >>= inGroups ["happstack", "important"]
-}
  _web_routes_th <- git "https://github.com/Happstack/web-routes-th.git" [] >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
              -- web_routes_transformers =  (git "https://github.com/Happstack/web-routes.git" [] >>= cd "web-routes-transformers") >>= debianize -- requires transformers ==0.2.*
  _web_routes_wai <- git "https://github.com/Happstack/web-routes-wai.git" [] >>= debianize >>= inGroups ["happstack", "important"]
  _webkit_sodium <- git "https://github.com/ghcjs/ghcjs-examples" [] >>= cd "webkit-sodium" >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _webkitgtk3 <- hackage "webkitgtk3" >>= flag (P.CabalPin "0.13.1.3") >>= flag (P.BuildDep "libwebkitgtk-3.0-dev") >>= debianize >>= inGroups ["glib"]
  _webkitgtk3_javascriptcore <- hackage "webkitgtk3-javascriptcore" >>= debianize
  _websockets <- (git "https://github.com/jaspervdj/websockets.git" [Commit "1b87107c9a4f5db9b05c828de1e80368bc0d3bba" {- avoid rebuilds -}]) >>= debianize
  _wl_pprint <-  (hackage "wl-pprint") >>= debianize
  _wl_pprint_extras <-  (git {-"https://github.com/ekmett/wl-pprint-extras"-}"https://github.com/seereason/wl-pprint-extras" []) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _wl_pprint_text <-  (hackage "wl-pprint-text") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
               -- Our applicative-extras repository has several important patches.
  _word8 <-  (hackage "word8") >>= debianize
  _word_trie <-  (hackage "word-trie") >>= debianize
  _x509 <-  (hackage "x509") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _x509_store <-  (hackage "x509-store") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _x509_system <-  (hackage "x509-system") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _x509_validation <-  (hackage "x509-validation") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"] :: TSt PackageId
  _xdg_basedir <-  (hackage "xdg-basedir" >>= tflag (P.DebVersion "0.2.2-2")) >>= debianize
  _xhtml <-  (hackage "xhtml" >>= wflag (P.DebVersion "3000.2.1-1") >>= qflag (P.DebVersion "3000.2.1-1build2") >>= tflag (P.DebVersion "3000.2.1-4")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _xml_conduit <-  (hackage "xml-conduit") >>= debianize >>= inGroups ["conduit", "authenticate", "important"]
  _xml <-  (hackage "xml") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"] -- apt (rel release "wheezy" "quantal") "haskell-xml"
  _xmlgen <-  (hackage "xmlgen") >>= debianize
  _xmlhtml <-  (hackage "xmlhtml") >>= debianize >>= skip (Reason "Unmet build dependencies: libghc-blaze-builder-dev (<< 0.4)")
  _xml_types <-  (hackage "xml-types") >>= debianize
  _xss_sanitize <-  (hackage "xss-sanitize" >>= qflag (P.DebVersion "0.3.2-1build1")) >>= debianize
  _yaml <-  (hackage "yaml") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _yaml_light <-  (hackage "yaml-light"
                              >>= wflag (P.DebVersion "0.1.4-2")
                              >>= pflag (P.DebVersion "0.1.4-2")
                              >>= qflag (P.DebVersion "0.1.4-2build1")
                              >>= tflag (P.DebVersion "0.1.4-5build1")) >>= debianize
  _yi <-  (hackage "yi") >>= debianize >>= skip (Reason "requires hint")
  _yi_language <-  (hackage "yi-language" >>= flag (P.BuildDep "alex")) >>= debianize
  _yi_rope <-  (hackage "yi-rope") >>= debianize
  _zip_archive <-  (hackage "zip-archive") >>= flag (P.BuildDep "zip") >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _zlib_bindings <-  (hackage "zlib-bindings") >>= debianize >>= inGroups [ "authenticate", "important"]
  _zlib <-  (hackage "zlib"
                      >>= flag (P.CabalPin "0.5.4.2") -- Cabal-1.22.6.0 is not ready for zlib-0.6
                      >>= flag (P.DevelDep "zlib1g-dev")) >>= debianize >>= inGroups ["ghcjs-libs", "ghc-libs", "platform"]
  _zlib_enum <-  (hackage "zlib-enum") >>= debianize >>= inGroups [ "authenticate", "important"]

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
  _th_typegraph `depends` [_set_extra, _th_desugar, _th_orphans]
  _web_routes_happstack `depends` [_happstack_server]
  _x509 `depends` [_pem, _asn1_parse]
  _x509_validation `depends` [_x509_store]

  -- Create the ghcjs library package targets
  findGroup "ghcjs-libs" >>= mapM_ ghcjs_flags
  noTests -- Some package test suites fail, some hang, especially with ghcjs

  return ()

noTests :: TSt ()
noTests = use P.packageMap >>= mapM_ (flag (P.CabalDebian ["--no-tests"])) . Map.keys
