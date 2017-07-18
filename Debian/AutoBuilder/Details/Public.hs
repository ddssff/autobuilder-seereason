{-# LANGUAGE CPP, FlexibleContexts, MultiWayIf, OverloadedStrings, RecordWildCards, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-name-shadowing #-}
module Debian.AutoBuilder.Details.Public ( buildTargets ) where

import Control.Lens (use, view, (%=))
import Data.FileEmbed (embedFile)
import Data.Map as Map (elems)
import Data.Set as Set (fromList, insert, member, Set)
import Data.Text as Text (unlines)
--import Data.Version (Version(Version))
import Debian.AutoBuilder.Details.Common -- (named, ghcjs_flags, putSrcPkgName)
import Debian.AutoBuilder.Types.Packages as P (TSt,
                                               PackageFlag(CabalPin, DevelDep, DebVersion, BuildDep, CabalDebian, RelaxDep, Revision,
                                                           NoDoc, UDeb, OmitLTDeps, SkipVersion), packageMap,
                                               pid, groups, PackageId, hackage, debianize, flag, apply, patch,
                                               darcs, apt, git, hg, cd, release,
                                               GroupName, inGroups, createPackage)
import Debian.AutoBuilder.Types.ParamRec (ParamRec(..))
import Debian.Debianize as D
    (doExecutable, execCabalM, rulesFragments, InstallFile(..), debInfo, atomSet, Atom(InstallData))
import Debian.Relation (BinPkgName(..))
import Debian.Releases (baseRelease, BaseRelease(Trusty, Artful))
import Debian.Repo.Fingerprint (RetrieveMethod(Debianize'', Hackage {-, Git-}), GitSpec(Commit))
--import Debug.Trace (trace)

-- Group descriptions:
--    ghcjs-libs - all the libraries we build ghcjs packages for
--    th-path - libraries required by th-path, an appraisalscribe dependency in private
--    happstack -- stuff required to build happstack-server
--    appraisalscribe - libraries that are dependencies for the appraisalscribe server and its private dependencies

findGroup :: GroupName -> TSt (Set P.PackageId)
findGroup name =
  (Set.fromList . map (view pid) . filter (Set.member name . view groups) . Map.elems) <$> use packageMap

buildTargets :: ParamRec -> TSt ()
buildTargets _params = do
  rel <- baseRelease <$> use release
#if 0
  let ghc8 :: TSt a -> TSt (Maybe a)
      ghc8 action = if hvrVersion params >= Just (Version [8] []) then Just <$> action else pure Nothing
      ghc7 :: TSt a -> TSt (Maybe a)
      ghc7 action = if hvrVersion params <  Just (Version [8] []) then Just <$> action else pure Nothing
#else
  -- Still using ghc7
  let ghc8 :: TSt a -> TSt (Maybe a)
      ghc8 _action = pure Nothing
      ghc7 :: TSt a -> TSt (Maybe a)
      ghc7 action = Just <$> action
#endif

  commonTargets

  --------------------------------------------------
  -- INDIVIDUAL PACKAGES (alphabetized by symbol) --
  --------------------------------------------------

  _abstract_deque <-  hackage (Just "0.3") "abstract-deque" >>= debianize []
  _abstract_par <- hackage (Just "0.3.3") "abstract-par" >>= debianize []
  -- _aeson <- hackage (Just "1.2.1.0") "aeson" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "important"] -- pin latest
  _aeson_pretty <- hackage (Just "0.8.5") "aeson-pretty" >>= debianize []
  _agi <- darcs ("https://github.com/ddssff/haskell-agi") >>= skip (Reason "No instance for (Applicative (AGIT m))")
  _alex <- ghc7 $
           hackage (Just "3.1.7") "alex" >>=
           patch $(embedFile "patches/alex.diff") >>=
           flag (P.CabalDebian ["--default-package", "alex"]) >>=
           flag (P.RelaxDep "alex") >>=
           flag (P.BuildDep "happy") >>=
           debianize [] >>=
           inGroups []
  -- No Debian build trees found in /home/dsf/.autobuilder/hackage/allocated-processor-0.0.2
  -- _allocated_processor <- hackage Nothing "allocated-processor"
  _annotated_wl_pprint <- hack (Just "0.7.0") "annotated-wl-pprint"
  _appar <- hackage Nothing "appar" >>= flag (P.DebVersion "1.1.4-1") >>= debianize []
  _async <-  (hackage (Just "2.1.0") "async") >>= debianize []
  _atomic_primops <-  (hackage (Just "0.8.0.4") "atomic-primops") >>= debianize []
  _attempt <-  (hackage (Just "0.4.0.1") "attempt") >>= debianize []
  _attoparsec_enumerator <-  (hackage (Just "0.3.4") "attoparsec-enumerator") >>= debianize []
               -- This was merged into attoparsec
               -- ,  (hackage "attoparsec-text" `patch` $(embedFile "patches/attoparsec-text.diff") >>= flag (P.Revision "")) >>= debianize []
               -- Deprecated
               -- ,  (hackage "attoparsec-text-enumerator") >>= debianize []
  _auto_update <-  (hackage (Just "0.1.4") "auto-update") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"]
  _base16_bytestring <-  (hackage (Just "0.1.1.6") "base16-bytestring") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _base_unicode_symbols <-  (hackage (Just "0.2.2.4") "base-unicode-symbols" >>= tflag (P.DebVersion "0.2.2.4-3")) >>= debianize []
  _bimap <-  (hackage (Just "0.3.2") "bimap") >>= debianize []
  -- _binary_tagged <- hackage Nothing "binary-tagged" >>= debianize []
  _bindings_dSL <-  (hackage (Just "1.0.23") "bindings-DSL") >>= debianize []
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
  _blaze_from_html <-  (hackage (Just "0.3.2.1") "blaze-from-html") >>= debianize [] >>= inGroups ["happstack", "important"]
  _blaze_textual <-  (hackage (Just "0.2.1.0") "blaze-textual") >>= debianize [] >>= inGroups ["happstack", "important"]
  _blaze_textual_native <-  (hackage (Just "0.2.1.1") "blaze-textual-native"
                             >>= patch $(embedFile "patches/blaze-textual-native.diff")
                             >>= flag (P.Revision "")) >>= debianize [] >>= inGroups ["happstack", "important"]
  _boolean <-  (hackage (Just "0.2.3") "Boolean") >>= debianize []
  -- _bugzilla <- broken <$> apt "squeeze" "bugzilla" -- requires python-central (>= 0.5)
  _byteorder <-  (hackage (Just "1.0.4") "byteorder" >>= tflag (P.DebVersion "1.0.4-1")) >>= debianize []
  _bytes <-  (hackage (Just "0.15.0.1") "bytes") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-cereal-dev (<< 0.5)")
  -- bytestring-builder is now part of bytestring, but some packages (fast-logger) still depend on it (now patched)
  _bytestring_nums <-  (hackage (Just "0.3.6") "bytestring-nums") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-bytestring-nums"
  _bytestring_trie <-  (hackage (Just "0.2.4.1") "bytestring-trie") >>= debianize []
               -- ,  (hackage "cairo-pdf") >>= debianize []
  -- This is provided by ghc.  1.24 is for ghc-8.  Why am I even
  -- building this?  Maybe because packages are failing with 1.22.4.0?
  -- _cabal <- hackage (Just "1.24.0.0") "Cabal" >>= debianize []

  -- Build ghc debs for the version of Cabal shipped with our ghcjs.
  -- These debs need special names (e.g. libghc-cabal1228-dev) to
  -- avoid conflicts with the virtual package provided by ghc.
  _cabal <- hackage (Just "1.22.8.0") "Cabal" >>= debianize []
  _cabal_install <- ghc8 $
                    hackage (Just "1.24.0.0") "cabal-install" >>=
                    -- debianize [] >>=
                    patch $(embedFile "patches/cabal-install.diff") >>= -- cabal-debian-4.35.7 outputs libghc-cabal-dev | ghc instead of ghc | libghc-cabal-dev
                    flag (P.CabalDebian ["--default-package", "cabal-install"]) >>=
                    inGroups []
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
  _certificate <- hackage (Just "1.3.9") "certificate" >>= patch $(embedFile "patches/certificate.diff") >>= tflag (P.DebVersion "1.3.9-1build4") >>= debianize []
  _cgi <- (hackage (Just "3001.2.2.2") "cgi" {- `patch` $(embedFile "patches/cgi.diff") -}) >>= debianize [] >>= inGroups ["platform"] >>= skip (Reason "Depends on exceptions < 0.7")
  _charset <-  (hackage (Just "0.3.7.1") "charset") >>= debianize []
  _charsetdetect_ae <-  (hackage (Just "1.1.0.1") "charsetdetect-ae") >>= debianize []
  _cheapskate <- git "https://github.com/seereason/cheapskate" [] {-hackage (Just "0.1.0.3") "cheapskate"-} >>= debianize [] >>= skip (Reason "data default dependency")
  _cipher_aes128 <-  (hackage (Just "0.7.0.1") "cipher-aes128") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _cipher_aes <-  (hackage (Just "0.2.11") "cipher-aes") >>= debianize []
  _cipher_des <-  (hackage (Just "0.0.6") "cipher-des" >>= tflag (P.DebVersion "0.0.6-1")) >>= debianize []
  _cipher_rc4 <-  (hackage (Just "0.1.4") "cipher-rc4" >>= tflag (P.DebVersion "0.1.4-1")) >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _citeproc_hs <-  (hackage (Just "0.3.10") "citeproc-hs") >>= debianize [] >>= skip (Reason "Non type-variable argument\nthe constraint: MonadState EvalState m\n(Use FlexibleContexts to permit this)")
  _closure_compiler <- apt "sid" "closure-compiler"
  _cmdargs <-  (hackage (Just "0.10.14") "cmdargs") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _colour <-  hackage (Just "2.3.3") "colour" >>= pflag (P.DebVersion "2.3.3-1build1")
                              >>= qflag (P.DebVersion "2.3.3-1build1")
                              >>= sflag (P.DebVersion "2.3.3-1")
                              >>= tflag (P.DebVersion "2.3.3-4")
                              >>= debianize []
               -- , apt "wheezy" "haskell-configfile"
  _comfort_graph <- hackage (Just "0.0.1") "comfort-graph" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>=
                    skip (Reason "transformers dependency too old")
  _concatenative <-  (hackage (Just "1.0.1") "concatenative") >>= debianize []
  _concrete_typerep <-  hackage (Just "0.1.0.2") "concrete-typerep" >>=
                        tflag (P.DebVersion "0.1.0.2-2build3") >>=
                        debianize [] >>=
                        skip (Reason "Constructor ‘TypeRep’ should have 4 arguments, but has been given 3")
  _cond <-  (hackage (Just "0.4.1.1") "cond") >>= debianize []
  _conduit_extra <-  (hackage (Just "1.1.13.2") "conduit-extra") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "conduit", "important", "servant"]
  _configurator <- hackage (Just "0.3.0.0") "configurator" >>= debianize []
  _configFile <-  (hackage (Just "1.1.4") "ConfigFile") >>= debianize []
  _constrained_normal <-  (hackage (Just "1.0.2") "constrained-normal") >>= debianize []
  _constraints <- hackage (Just "0.8") "constraints" >>= debianize []
  _consumer <- darcs ("http://src.seereason.com/haskell-consumer") >>= skip (Reason "build failure")
  _control_monad_free <-  (hackage (Just "0.6.1") "control-monad-free") >>= debianize []
  -- apt "sid" "debian-keyring=2014.03.03" -- The current version (2014.04.25) seems to be missing some keys that we need
  _cprng_aes <-  (hackage (Just "0.6.1") "cprng-aes") >>= debianize []
  _cpu <- hackage (Just "0.1.2") "cpu" >>= tflag (P.DebVersion "0.1.2-1") >>= debianize []
  -- A bunch of missing dependencies - glob, cassava, hastache, statistics
  -- _criterion <- hackage (Just "1.1.1.0") "criterion" >>= debianize []
  _crypto_cipher_types <-  (hackage (Just "0.0.9") "crypto-cipher-types" >>= tflag (P.DebVersion "0.0.9-1")) >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto <-  (hackage (Just "4.2.5.1") "Crypto") >>= debianize []
  _cryptohash_conduit <-  (hackage (Just "0.1.1") "cryptohash-conduit") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "servant"]
  _cryptohash_sha256 <- hackage (Just "0.11.100.1") "cryptohash-sha256" >>= debianize []
  _crypto_numbers <-  (hackage (Just "0.2.7") "crypto-numbers") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey <-  (hackage (Just "0.2.8") "crypto-pubkey") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _crypto_pubkey_types <-  (hackage (Just "0.4.3") "crypto-pubkey-types") >>= debianize []
               -- crypto-pubkey-types-0.3.2 depends on older asn1-types
  _crypto_random_api <-  (hackage (Just "0.2.0") "crypto-random-api" >>= tflag (P.DebVersion "0.2.0-2")) >>= debianize []
  _crypto_random <-  (hackage (Just "0.0.9") "crypto-random") >>= debianize []
  _css <-  (hackage (Just "0.2") "css") >>= debianize [] >>= skip (Reason "No instance for (Applicative (CSSM x))")
  _csv <-  (hackage (Just "0.1.2") "csv" >>= pflag (P.DebVersion "0.1.2-2")
                              >>= tflag (P.DebVersion "0.1.2-5build1")) >>= debianize []
  _curl <-  (hackage (Just "1.3.8") "curl" >>= tflag (P.DebVersion "1.3.8-2")) >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-curl"
  _currency <-  (hackage (Just "0.2.0.0") "currency") >>= debianize []
  _data_accessor <-  (hackage (Just "0.2.2.7") "data-accessor") >>= debianize []
  _data_accessor_template <-  (hackage (Just "0.2.1.13") "data-accessor-template") >>= debianize []
  _data_binary_ieee754 <- hack (Just "0.4.4") "data-binary-ieee754"
  _dataenc <- ghc7 $ (hackage (Just "0.14.0.7") "dataenc" >>= patch $(embedFile "patches/dataenc.diff")) >>= debianize []
  _data_lens <- hackage (Just "2.10.7") "data-lens" {-`patch` $(embedFile "patches/data-lens.diff")-} >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= skip (Reason "Unmet build dependencies: libghcjs-comonad-dev (<< 4.3) libghcjs-semigroupoids-dev (<< 5.1)")
  _data_lens_light <- hackage (Just "0.1.2.2") "data-lens-light" >>= debianize [] >>= inGroups ["ghc-libs"]
  -- _data_lens_template <-  (hackage (Just "2.1.9") "data-lens-template") >>= debianize []
  _data_object <-  (hackage (Just "0.3.1.9") "data-object" >>= patch $(embedFile "patches/data-object.diff")) >>= debianize []
  _data_ordlist <-  (hackage (Just "0.4.7.0") "data-ordlist") >>= debianize []
  _data_reify <-  (hackage (Just "0.6.1") "data-reify") >>= debianize []
  _data_r_tree <-  (hackage (Just "0.0.5.0") "data-r-tree") >>= debianize []
  _data_stringmap <-  (hackage (Just "1.0.1.1") "data-stringmap") >>= debianize []
  _date_cache <-  (hackage (Just "0.3.0") "date-cache" >>= tflag (P.DebVersion "0.3.0-3")) >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _datetime <- hackage (Just "0.3.1") "datetime" {->>= pflag (P.DebVersion "0.2.1-2") >>= tflag (P.DebVersion "0.2.1-5build1")-} >>= debianize []
  _debhelper <- apt "wheezy" "debhelper" >>= patch $(embedFile "patches/debhelper.diff") >>= skip (Reason "Use standard")
  _debootstrap <- apt "sid" "debootstrap" >>= flag (P.UDeb "debootstrap-udeb")
               -- Build fails due to some debianization issue
               -- , apt "wheezy" "geneweb"
  _decimal <-  (hackage (Just "0.4.2") "Decimal") >>= debianize [] -- for hledger
  _deepseq_generics <- hackage (Just "0.2.0.0") "deepseq-generics" >>= {-patch $(embedFile "patches/deepseq-generics.diff") >>=-} debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _derive <-  (hackage (Just "2.6.2") "derive") >>= debianize []
  _directory_tree <-  (hackage (Just "0.12.0") "directory-tree") >>= debianize []
  _dlist <-  (hackage (Just "0.8.0.1") "dlist") >>= debianize []
               -- Natty only(?)
  _doctest <-  (hackage (Just "0.11.0") "doctest") >>= debianize [] >>= inGroups [{-"ghcjs-libs",-} "ghc-libs"]
      -- This package fails to build in several different ways because it has no modules.
      -- I am just going to patch the packages that use it to require transformers >= 0.3.
      -- Specifically, distributive and lens.
  _dpkg <- apt "wheezy" "dpkg" >>= patch $(embedFile "patches/dpkg.diff") >>= skip (Reason "use standard")
  _drbg <-  (hackage (Just "0.5.4") "DRBG") >>= debianize [] >>= inGroups ["authenticate", "important"] >>= skip (Reason "requires update to use current cereal")
  -- Depends on several old packages
  -- _dropbox_sdk <-  (hackage (Just "0.3.1") "dropbox-sdk") >>= debianize [] >>= patch $(embedFile "patches/dropbox-sdk.diff")
  _dynamic_state <-  (hackage (Just "0.2.2.0") "dynamic-state") >>= debianize []
  _dyre <-  (hackage (Just "0.8.12") "dyre") >>= debianize []
  _easy_file <-  (hackage (Just "0.2.1") "easy-file") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _ed25519 <- hackage (Just "0.0.5.0") "ed25519" >>= patch $(embedFile "patches/ed25519.diff") >>= debianize []
  _edisonAPI <- hackage (Just "1.3.1") "EdisonAPI" >>= debianize []
  _edisonCore <- ( (hackage (Just "1.3.1.1") "EdisonCore" >>= qflag (P.DebVersion "1.2.1.3-9build2")) >>= debianize [])
  _ekg_core <-  (hackage (Just "0.1.1.1") "ekg-core") >>= debianize []
  _enumerator <-  (hackage (Just "0.4.20") "enumerator" >>= qflag (P.DebVersion "0.4.19-1build2")) >>= debianize []
  _erf <-  (hackage (Just "2.0.0.0") "erf"
                              >>= pflag (P.DebVersion "2.0.0.0-3")
                              >>= wflag (P.DebVersion "2.0.0.0-3")
                              >>= tflag (P.DebVersion "2.0.0.0-5")) >>= debianize []
  _errors <-  (hackage (Just "2.1.2") "errors") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _expiring_cache_map <-  (hackage (Just "0.0.5.4") "expiring-cache-map") >>= debianize []
  _executable_path <-  (hackage (Just "0.0.3") "executable-path"
                              >>= pflag (P.DebVersion "0.0.3-1")
                              >>= tflag (P.DebVersion "0.0.3-3")) >>= debianize []
               -- , apt (rel release "wheezy" "quantal") "haskell-digest"
               -- , apt (rel release "wheezy" "quantal") "haskell-dlist"
  _extensible_exceptions <-  (hackage (Just "0.1.1.4") "extensible-exceptions" -- required for ghc-7.6.  Conflicts with ghc-7.4 in wheezy.
                              >>= tflag (P.DebVersion "0.1.1.4-2")) >>= debianize []
  _failure <- hackage (Just "0.2.0.3") "failure" >>= debianize []
  -- Patch removes dependency on bytestring-builder, now part of bytestring.
  _fast_logger <- hackage (Just "2.4.6") "fast-logger">>= patch $(embedFile "patches/fast-logger.diff") >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs", "authenticate", "important"]
  _fay_base <-  (hackage (Just "0.20.0.1") "fay-base") >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _fay <-  (hackage (Just "0.23.1.16") "fay" {- >>= patch $(embedFile "patches/fay.diff") -}) >>= debianize [] >>= flag (P.CabalDebian [ "--depends", "haskell-fay-utils:cpphs" ]) >>= skip (Reason "too old for current syb and optparse-applicative")
  _fay_jquery <-  (git "https://github.com/faylang/fay-jquery" []) >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _fay_text <-  (hackage (Just "0.3.2.2") "fay-text") >>= debianize [] >>= skip (Reason "Waiting for newer fay")
  _feed <- git "https://github.com/seereason/feed" [] {-hackage "feed"-} >>= tflag (P.DebVersion "0.3.9.2-1") >>= debianize []
  _file_embed <-  (hackage (Just "0.0.10") "file-embed") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _file_location <- hackage (Just "0.4.9.1") "file-location" >>= {-flag (P.CabalDebian [ "--source-package-name", "file-location" ]) >>=-} debianize []
  _filelock <- hackage Nothing "filelock" >>= debianize []
  _fingertree <- hack (Just "0.1.1.0") "fingertree"
  _fixed <- hackage (Just "0.2.1.1") "fixed" >>= debianize []
  _flock <- hackage (Just "0.3.1.8") "flock" >>= debianize []
  _fmlist <-  (hackage (Just "0.9") "fmlist") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _foreign_var <-  (hackage (Just "0.1") "foreign-var") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= skip (Reason "dependencies")
  _formlets <-  (hackage (Just "0.8") "formlets"
                               >>= patch $(embedFile "patches/formlets.diff")
                               >>= flag (P.DebVersion "0.8-1~hackage1")) >>= debianize []
  _frquotes <-  (hackage (Just "0.2.1") "frquotes") >>= debianize []
               -- Usable versions of this package are available in some dists -
               -- e.g. trusty and wheezy.
               -- , apt "trusty" "foo2zjs"
  _fsnotify <-  (hackage (Just "0.2.1") "fsnotify") >>= debianize []
  _ftgl <-  (hackage (Just "2.1") "FTGL"
                     -- >>= patch $(embedFile "patches/FTGL.diff")
                     >>= flag (P.DevelDep "libftgl-dev")
                     >>= flag (P.DevelDep "libfreetype6-dev")) >>= debianize []
  _gd <-  (hackage (Just "3000.7.3") "gd" >>= patch $(embedFile "patches/gd.diff")
                       >>= flag (P.DevelDep "libgd-dev")
                       >>= flag (P.DevelDep "libc6-dev")
                       >>= flag (P.DevelDep "libfreetype6-dev")
                       >>= wflag (P.DebVersion "3000.7.3-1")
                       >>= qflag (P.DebVersion "3000.7.3-1build2")
                       >>= tflag (P.DebVersion "3000.7.3-3")) >>= debianize []
               -- ,  (flags [P.BuildDep "libm-dev", P.BuildDep "libfreetype-dev"] (hackage (Just "3000.7.3") "gd")) >>= debianize []
  _gdiff <-  (hackage (Just "1.1") "gdiff") >>= debianize []
  _gdiff_th <- git "https://github.com/ddssff/gdiff-th" [] >>= debianize []
  _genI <-  (darcs "http://hub.darcs.net/kowey/GenI" >>= patch $(embedFile "patches/GenI.diff")) >>= debianize [] >>= inGroups ["GenI"]
  -- ghc76 <- ghcFlags $ apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff")
  -- ghc78 <- ghcFlags $ apt "experimental" "ghc" >>= patch $(embedFile "patches/trac9262.diff")
  -- _ghc710 <- apt "experimental" "ghc" >>= ghcFlags
  --                       >>= patch $(embedFile "patches/ghc.diff")
  --                           >>= skip (Reason "stick with current, avoid huge rebuild")
  _ghc8 <-
      case rel of
        Artful -> apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc"] >>= skip (Reason "Artful includes ghc8")
        Trusty -> apt "sid" "ghc" >>= patch $(embedFile "patches/ghc.diff") >>= inGroups ["ghc"] >>= skip (Reason "Waiting for interactive support in ghcjs-0.2.1")
  _ghc_boot <- hackage (Just "8.0.2") "ghc-boot" >>= debianize [] >>= skip (Reason "Encountered missing dependencies: 2> binary ==0.8.*")
  _ghc_boot_th <- hackage (Just "8.0.2") "ghc-boot-th" >>= debianize []
  _ghc_exactprint <- git "https://github.com/alanz/ghc-exactprint" [] >>= debianize []
  _terminal_size <- hackage (Just "0.3.2.1") "terminal-size"  >>= debianize [] >>= inGroups ["ghcid"]
  _ghcid <- hackage (Just "0.6.4") "ghcid" >>= debianize [] >>= inGroups ["ghcid"]
  _ghcjs_base <- git "https://github.com/ghcjs/ghcjs-base" [] >>= debianize [] {->>= ghcjs_only-} -- causes double --ghcjs to be passed to cabal-debian
  -- ghcjs_vdom = ghcjs_flags ( (git "https://github.com/seereason/ghcjs-vdom" [Branch "base48"]) >>= debianize `putSrcPkgName` "ghcjs-ghcjs-vdom")
  _ghcjs_ffiqq <- git "https://github.com/ghcjs/ghcjs-ffiqq" [] >>= putSrcPkgName "ghcjs-ghcjs-ffiqq" >>= debianize [] >>= inGroups ["ghcjs-libs"] >>= skip (Reason "[libghc-ghcjs-base-doc] -> []")
  _ghcjs_prim <- git "https://github.com/ghcjs/ghcjs-prim" [] >>= debianize [] >>= inGroups ["ghcjs-comp", "glib"]
  _ghc_boot <- ghc8 $ hackage (Just "8.0.1") "ghc-boot" >>= debianize [] -- Required by haddock-api
  _ghc_mtl <- (hackage (Just "1.2.1.0") "ghc-mtl") >>= debianize [] {- >>= skip (Reason "No instance for (MonadIO GHC.Ghc)") -}
  _ghc_paths <-  (hackage (Just "0.1.0.9") "ghc-paths" >>= tflag (P.DebVersion "0.1.0.9-3")) >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-ghc-paths" -- for leksah
               -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
               -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
               --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
               -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
               -- Errors were encountered while processing:
               --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
               -- E: Sub-process /usr/bin/dpkg returned an error code (1)
  _ghc_simple <- (hackage (Just "0.4") "ghc-simple") >>= debianize []
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
  _haddock_api8 <-
      ghc8 $ hackage (Just "2.17.3") "haddock-api" >>=
             flag (P.CabalDebian ["--default-package", "haddock-api"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>=
{-
             -- This breaks the build
             apply (execCabalM $ (debInfo . rulesFragments) %=
                                         Set.insert (Text.unlines [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                  , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"])) >>=
-}
             debianize [] >>= inGroups ["ghcjs-comp"]
  _haddock_api7 <-
      ghc7 $ hackage (Just "2.16.1") "haddock-api" >>=
             flag (P.CabalDebian ["--default-package", "haddock-api"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>=
             flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>=
             -- FIXME - This cabal-debian stuff does nothing because this isn't a Debianize target
             apply (execCabalM $ (debInfo . rulesFragments) %=
                                         Set.insert (Text.unlines [ "# Force the Cabal dependency to be the version provided by GHC"
                                                                  , "DEB_SETUP_GHC_CONFIGURE_ARGS = --constraint=Cabal==$(shell dpkg -L ghc | grep 'package.conf.d/Cabal-' | sed 's/^.*Cabal-\\([^-]*\\)-.*$$/\\1/')\n"])) >>=
             debianize [] >>= inGroups ["ghcjs-comp"]
  _hackage_security <- hackage (Just "0.5.2.2") "hackage-security" >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>= debianize []
  _half <-  (hackage (Just "0.2.2.3") "half" >>= inGroups ["gl"]) >>= debianize []
  _hamlet <-  (hackage (Just "1.2.0") "hamlet") >>= debianize [] >>= inGroups ["ghcjs-libs"] >>= skip (Reason "No input files to haddock?")
  -- seereason still depends on this
  _happy <- ghc7 $
            hackage (Just "1.19.5") "happy"
            >>= flag (P.CabalDebian ["--executable", "happy"])
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
                                               , "GLR_Lib-ghc-debug" ] )
           >>= flag (P.RelaxDep "happy")
           >>= flag (P.BuildDep "happy")
           >>= debianize []
           >>= inGroups []
  _harp <-  (git "https://github.com/seereason/harp" []) >>= debianize []
  _hashable <-  (hackage (Just "1.2.4.0") "hashable") >>= debianize []
  _hashed_storage <-  (hackage (Just "0.5.11") "hashed-storage") >>= debianize [] >>= skip (Reason "Non type-variable argument in the constraint: MonadState (TreeState m_aFTg) (t m_aFTg)")
               -- Built into ghc-7.8.3
  _hashtables <- hackage (Just "1.2.1.0") "hashtables" >>= patch $(embedFile "patches/hashtables.diff") >>= debianize []
  -- _haskeline <- hackage (Just "0.7.2.3") "haskeline" >>= debianize []
  _haskell_darcs <-  (darcs "http://darcs.net/reviewed"
                     >>= flag (P.CabalDebian ["--source-package-name", "darcs"])
                     >>= flag (P.CabalDebian ["--default-package", "darcs"])
                     >>= flag (P.CabalDebian ["--cabal-flags", "-http"]) -- the http flag forces network < 2.5
                     -- >>= patch $(embedFile "patches/darcs.diff")
                    ) >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-vector-dev (<< 0.11)")
  _haskell_devscripts <-
      -- Revert to version we used from 8/2016-11/2016
      git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "a143f70d333663e1447998d6facbebf67cd5045f"] >>=
      -- git "http://anonscm.debian.org/cgit/pkg-haskell/haskell-devscripts.git" [Commit "2668216c654b0b302cb51162b2246c39cd6adc1e"] >>=
      -- git "https://github.com/ddssff/haskell-devscripts" [Branch "0.12"] >>=
      -- This changes --show-details=direct to --show-details=always in check-recipe
      patch $(embedFile "patches/haskell-devscripts.diff") >>=
      flag (P.RelaxDep "python-minimal") >>= inGroups ["platform"] >>=
      (if rel == Artful then skip (Reason "use standard") else return)
  _haskell_list <-  (hackage (Just "0.5.2") "List") >>= debianize []
  _haskell_names <-  git "https://github.com/haskell-suite/haskell-names" [] >>= debianize [] >>= inGroups ["ghc-libs","pretty"]
  _haskell_newtype <-  (hackage (Just "0.2") "newtype" >>= wflag (P.DebVersion "0.2-1") >>= tflag (P.DebVersion "0.2-3")) >>= debianize []
  _haskell_packages <-  (hackage (Just "0.3") "haskell-packages" {->>= patch $(embedFile "patches/haskell-packages.diff")-}) >>= debianize [] >>= inGroups ["happstack", "important"] >>= skip (Reason "duplicate FromJSON instances")
  _sr_revision <-  (git ("https://github.com/seereason/sr-revision") []) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _haskell_src <-  (hackage (Just "1.0.2.0") "haskell-src" >>= flag (P.BuildDep "happy")) >>= debianize [] >>= inGroups ["platform"]
  -- The source package name is set to haskell-src-exts by the
  -- cabal-debian package, Debian.Debianize.Details.debianDefaults.
  -- But where does that leave ghcjs-haskell-src-exts?
  -- _hastache <- hackage Nothing "hastache" >>= debianize []
  _haste_compiler <- hack (Just "0.5.5.0") "haste-compiler" >>= flag (P.CabalDebian ["--default-package", "haste-compiler"]) >>= skip (Reason "Unmet build dependencies: libghc-shellmate-dev (<< 0.3) libghc-shellmate-prof (<< 0.3)")
  _haste_ffi_parser <- git' "https://github.com/RudolfVonKrugstein/haste-ffi-parser" []
  _haXml <- {-git "https://github.com/ddssff/HaXml" []-} hackage (Just "1.25.3") "HaXml" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "pretty"]
  _hclip <- hackage (Just "3.0.0.4") "Hclip" >>= debianize []
  _hdaemonize <-  (git "https://github.com/madhadron/hdaemonize" []) >>= debianize [] >>= skip (Reason "Module ‘System.Posix.Syslog’ does not export ‘syslog’")
  _heap <-  (hackage (Just "1.0.3") "heap") >>= debianize []
               -- ,  (hackage (Just "0.13.1.2") "heist" >>= patch $(embedFile "patches/heist.diff")) >>= debianize []
  _hex <-  (hackage (Just "0.1.2") "hex") >>= debianize []
  _hexpat <- hackage (Just "0.20.9") "hexpat" >>= debianize []
  _hinotify <-  (hackage (Just "0.3.8.1") "hinotify") >>= debianize []
  _hint <-  (hackage (Just "0.6.0") "hint") >>= debianize [] {- >>= skip (Reason "requires ghc-mtl") -}
  -- _hit <- hackage Nothing "hit" >>= debianize []
  _hledger <- git "https://github.com/simonmichael/hledger" [] >>= cd "hledger-lib" >>= debianize [] >>= skip (Reason "requires mtl-compat")
           {-
               -- Needs a build dependency on libXrandr-dev and the cabal package x11.
               , P.Package { P.spec =  (Hackage "xmobar") >>= Debianize []
                           , P.flags = [] }
           -}
  _hlint <-  (hackage (Just "1.8.53") "hlint") >>= debianize [] >>= inGroups [{-"ghcjs-libs",-} "ghc-libs"] >>= skip (Reason "[libghc-refact-doc] -> []")

  -- _hpack <- hackage Nothing "hpack" >>= debianize []
  _hpdf <-  (hackage (Just "1.4.10") "HPDF") >>= debianize []
  _hS3 <- ghc7 $ (git "https://github.com/scsibug/hS3.git" []) >>= debianize []
                               >>= apply (execCabalM $ doExecutable (BinPkgName "hs3") (InstallFile {execName = "hs3", sourceDir = Nothing, destDir = Nothing, destName = "hs3"}))
  _hs_bibutils <-  (hackage (Just "5.5") "hs-bibutils") >>= debianize []
  _hscolour <-  (hackage (Just "1.24.1") "hscolour") >>= debianize [] >>= flag (P.RelaxDep "hscolour") >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hse_cpp <-  (hackage (Just "0.2") "hse-cpp") >>= debianize [] {->>= patch $(embedFile "patches/hse-cpp.diff")-} >>= inGroups ["happstack", "important"]
  -- _hse_cpp <- git "https://github.com/haskell-suite/hse-cpp" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _hsemail <- hackage (Just "1.7.7") "hsemail" >>= debianize []
  _hsOpenSSL <-  (hackage (Just "0.11.3.2") "HsOpenSSL"
                              >>= flag (P.DevelDep "libssl-dev")
                              >>= flag (P.DevelDep "libcrypto++-dev")) >>= debianize []
  _hsp <-  (hackage (Just "0.10.0") "hsp" >>= flag (P.BuildDep "hsx2hs")) >>= debianize [] >>= inGroups ["happstack", "important"]
  _hspec <-  (hackage (Just "2.2.3") "hspec") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_core <-  (hackage (Just "2.2.3") "hspec-core") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_discover <-  (hackage (Just "2.2.3") "hspec-discover") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_expectations <-  (hackage (Just "0.7.2") "hspec-expectations") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _hspec_meta <-  (hackage (Just "2.2.1") "hspec-meta") >>= debianize []
  _hsSyck <-  (hackage (Just "0.53") "HsSyck") >>= debianize []
  _hStringTemplate <- hackage (Just "0.8.5") "HStringTemplate" >>= debianize []
  _hsx2hs <- {- git "https://github.com/seereason/hsx2hs.git" [] -}
             {- git "file:///home/dsf/git/hsx2hs" [] -}
             hackage (Just "0.14.0") "hsx2hs" >>=
             flag (P.CabalDebian ["--executable", "hsx2hs",
                                  "--conflicts", "hsx2hs:haskell-hsx-utils",
                                  "--replaces", "hsx2hs:haskell-hsx-utils",
                                  "--provides", "hsx2hs:haskell-hsx-utils"]) >>=
             debianize [] >>=
             inGroups ["happstack", "lens", "important"]
  _hsyslog <-  (hackage (Just "4") "hsyslog") >>= debianize []
  _htf <-  (hackage (Just "0.13.1.0") "HTF" >>= flag (P.BuildDep "cpphs")) >>= debianize []
  _html_entities <- darcs ("http://src.seereason.com/html-entities")
  _html_xml_utils <- apt "sid" "html-xml-utils"
      -- Deprecated in favor of http-conduit
      -- ,  (hackage (Just "0.2.0.1") "http-client-conduit") >>= debianize []
      -- Deprecated in favor of conduit-extra
      -- ,  (hackage (Just "1.0.1.2") "attoparsec-conduit") >>= debianize []
      -- ,  (hackage (Just "1.0.0") "blaze-builder-conduit") >>= debianize []
      -- ,  (hackage (Just "1.0.0") "zlib-conduit") >>= debianize []
  _http_common <-  (hackage (Just "0.8.2.0") "http-common") >>= debianize [] >>= inGroups ["platform", "happstack", "important"]
  _http_date <-  (hackage (Just "0.0.6.1") "http-date") >>= debianize []
  _http2 <-  (hackage (Just "1.6.1") "http2") >>= debianize []
  _hunt <-  (git "https://github.com/hunt-framework/hunt.git" [] >>= cd "hunt-searchengine" ) >>= debianize [] >>= skip (Reason "No instance for (Foldable t0) arising from a use of ‘elem’")
               -- ,  (darcs "haskell-tiny-server" ("http://src.seereason.com/tiny-server") >>= flag (P.BuildDep "hsx2hs")
               --                >>= flag (P.SkipPackage {- has a "derives SafeCopy" -})) >>= debianize []
  _i18n <-  (hackage (Just "0.3") "i18n" >>= flag (P.DebVersion "0.3-1~hackage1")) >>= debianize [] >>= skip (Reason "Could not find module ‘System.IO.UTF8’")
  _iconv <-  (hackage (Just "0.4.1.3") "iconv") >>= debianize []
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
  _intervals <-  (hackage (Just "0.7.2") "intervals") >>= debianize []
  _ioRefCAS <- (hackage (Just "0.2.0.1") "IORefCAS") >>= debianize [] >>= skip (Reason "Version 0.2.0.1 build fails")
  _io_storage <-  (hackage (Just "0.3") "io-storage" >>= pflag (P.DebVersion "0.3-2") >>= tflag (P.DebVersion "0.3-5")) >>= debianize []
  -- _io_streams <- git "https://github.com/snapframework/io-streams" [] >>= debianize [] >>= inGroups ["important"] -- pull request to allow atto-parsec-0.13
  _iproute <-  (hackage (Just "1.7.0") "iproute") >>= debianize []
  _iso3166_country_codes <-  (hackage (Just "0.20140203.7") "iso3166-country-codes") >>= debianize []
  _jmacro <-  (hackage (Just "0.6.14") "jmacro") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= inGroups ["happstack", "lens", "th-path", "important"]
  _jmacro_rpc <- hackage (Just "0.3.2") "jmacro-rpc" >>= inGroups ["happstack", "important"] >>= debianize [] >>= broken
  _jmacro_rpc_happstack <- hackage (Just "0.3.2") "jmacro-rpc-happstack" >>= flag (P.SkipVersion "0.2.1") >>= debianize [] >>= broken -- Really just waiting for jmacro-rpc
  _jquery <- apt "sid" "jquery" >>= skip (Reason "Missing dependency node-source-map") {- >>= patch $(embedFile "patches/jquery.diff") -} -- Revert to version 1.7.2+dfsg-3, version 1.7.2+dfsg-3.2 gives us a nearly empty jquery.min.js 
  _jquery_goodies <- apt "sid" "jquery-goodies"
                     -- >>= patch $(embedFile "patches/jquery-goodies.diff")
               -- We want to stick with jqueryui-1.8 for now, so create
               -- packages with the version number embedded in the name.
  _jqueryui18 <- darcs ("http://src.seereason.com/jqueryui18")
  _js_flot <-  (hackage (Just "0.8.3") "js-flot") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _js_jquery <-  (hackage (Just "3.1.0") "js-jquery") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _jsaddle <- git "https://github.com/ghcjs/jsaddle" [] >>= debianize [] >>= inGroups ["ghcjs-libs"] >>= skip (Reason "Unmet build dependencies: libghcjs-lens-dev (<< 4.15)")
  _json <-  (hackage (Just "0.9.1") "json") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "seereason", "important"] -- darcs "haskell-json" (repo ++ "/haskell-json")
  _keys <-  (hackage (Just "3.11") "keys") >>= debianize []
  _language_css <-  (hackage Nothing "language-css" >>= flag (P.DebVersion "0.0.4.1-1~hackage1")) >>= debianize []
  _language_ecmascript <-  (hackage (Just "0.17.0.1") "language-ecmascript") >>= debianize [] >>= skip (Reason "relax data-default dependency")
  _language_haskell_extract <-  (hackage (Just "0.2.4") "language-haskell-extract") >>= debianize []
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
  _lattices <- hackage (Just "1.5.0") "lattices" >>= debianize []
  -- This commit adds the sequence numbers we need to generated function parameters
  -- _lens <-  (hackage (Just "4.14") "lens") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
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
  _lifted_async <-  (hackage (Just "0.9.3") "lifted-async") >>= debianize [] >>= inGroups ["ghcjs-comp"]
  _linear <-  (hackage (Just "1.20.2") "linear") >>= debianize [] >>= skip (Reason "Requires bytes")
  _list_extras <-  (hackage (Just "0.4.1.4") "list-extras") >>= debianize []
  _list_tries <-  (hackage (Just "0.6.3") "list-tries" {- >>= patch $(embedFile "patches/list-tries.diff") -}) >>= debianize [] >>= inGroups ["happstack", "important"] -- version 0.5.2 depends on dlist << 0.7
  _loch_th <-  (hackage (Just "0.2.1") "loch-th") >>= debianize []
  _logging <- hackage (Just "3.0.4") "logging" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs", "important"]
  _logic_TPTP <-  (hackage (Just "0.4.4.0") "logic-TPTP") >>= debianize []
                 >>= patch $(embedFile "patches/logic-TPTP.diff")
                 >>= flag (P.BuildDep "alex")
                 >>= flag (P.BuildDep "happy")
  -- logic_TPTP = pure $ P.Package { P.spec = Debianize'' (Patch (Hackage (Just "0.4.4.0") "logic-TPTP") $(embedFile "patches/logic-TPTP.diff")) Nothing, P._flags = [ P.BuildDep "alex", P.BuildDep "happy" ] }
               -- , apt "sid" "haskell-maybet"
  _logict <- createPackage (Debianize'' (Hackage "logict") Nothing) mempty [] >>= inGroups ["ghcjs-libs", "ghc-libs"] :: TSt PackageId
  _maccatcher <-  (hackage (Just "2.1.5") "maccatcher"
                              >>= pflag (P.DebVersion "2.1.5-3")
                              >>= tflag (P.DebVersion "2.1.5-5build1")) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _magic <-  (hackage (Just "1.1") "magic" >>= flag (P.DevelDep "libmagic-dev")) >>= debianize []
           {-  , P.Package { P._spec = Quilt (Apt "wheezy" "magic-haskell") (Darcs ("http://src.seereason.com/magic-quilt"))
                           , P._flags = mempty } -}
  _mainland_pretty <-  (hackage (Just "0.4.1.4") "mainland-pretty") >>= debianize []
  _makedev <- apt "wheezy" "makedev" >>= skip (Reason "Use standard")
  _markdown <-  (hackage (Just "0.1.14") "markdown" {- >>= patch $(embedFile "patches/markdown.diff") -}) >>= debianize [] >>= inGroups ["happstack", "important"]
  _markdown_unlit <-  (hackage (Just "0.4.0") "markdown-unlit" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize []
  _maybeT <-  (hackage (Just "1.2") "MaybeT" >>= flag (P.DebVersion "1.2-6")) >>= debianize [] >>= skip (Reason "Could not deduce (Applicative (MaybeT m))")
  _memoize <-  (hackage (Just "0.8.1") "memoize") >>= debianize []
  _memoTrie <- ghc7 $  (hackage (Just "0.6.4") "MemoTrie") >>= debianize []
{-
  _microlens <- ghc7 $ hackage (Just "0.4.6.0") "microlens" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_dev <- ghc7 $ hackage (Just "0.4.6.0") "microlens-dev" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_ghc <- ghc7 $ hackage (Just "0.4.6.0") "microlens-ghc" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_mtl <- ghc7 $ hackage (Just "0.1.9.0") "microlens-mtl" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_platform <- ghc7 $ hackage (Just "0.3.4.0") "microlens-platform" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_th <- ghc7 $ hackage (Just "0.4.0.0") "microlens-th" >>= debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs"]
  _microlens_compat <-
      ghc7 $ git "https://github.com/seereason/microlens-compat.git" [] >>=
             apply (replacement "microlens-compat" "lens") >>=
             debianize [] >>= inGroups ["ghc-libs", "ghcjs-libs", "th-path", "important"]
-}
  _mime_mail <-  (git "https://github.com/snoyberg/mime-mail.git" [] >>= cd "mime-mail") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _missingH <-  (hackage (Just "1.4.0.1") "MissingH") >>= debianize []
  _mmap <-  (hackage (Just "0.5.9") "mmap") >>= debianize []
  _module_management <-  (git "https://github.com/seereason/module-management" [] >>= flag (P.BuildDep "rsync")) >>= debianize []
  _monadCatchIO_mtl <-  (hackage (Just "0.3.1.0") "MonadCatchIO-mtl" >>= patch $(embedFile "patches/monadcatchio-mtl.diff")) >>= debianize []
  _monadCatchIO_transformers <- hackage (Just "0.3.1.3") "MonadCatchIO-transformers" >>=
                                qflag (P.DebVersion "0.3.0.0-2build2") >>=
                                patch $(embedFile "patches/MonadCatchIO-transformers.diff") >>=
                                debianize []
  _monadcryptorandom <-  (hackage (Just "0.7.0") "monadcryptorandom") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _monadLib <-  (hackage (Just "3.7.3") "monadLib") >>= debianize []
               -- Putting this in our repo can cause problems, because when it is
               -- installed some packages can't compile unless you add package
               -- qualifiers to their imports.  For this reason, when we run the
               -- autobuilder with the --lax flag we usually get a failure from
               -- some package that builds after monads-tf got installed.  On the
               -- other hand, without monads-tf we lose this dependency chain:
               -- monads-tf <- options <- fay.
  _monadlist <-  (hackage (Just "0.0.2") "monadlist") >>= debianize [] >>= inGroups ["clckwrks", "important"]
  _monad_loops <-  (hackage (Just "0.4.3") "monad-loops") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _monad_parallel <-  (hackage (Just "0.7.2.2") "monad-parallel") >>= debianize []
  _monad_par <-  (hackage (Just "0.3.4.8") "monad-par") >>= debianize []
  _monad_par_extras <-  (hackage (Just "0.3.3") "monad-par-extras") >>= debianize []
  _monads_tf <-  (hackage (Just "0.1.0.3") "monads-tf") >>= debianize []
  _monad_task <- hackage (Just "0.1.0") "monad-task" >>= debianize [] >>= skip (Reason "0.1.0 requires transformers<4")
  _mono_traversable <- hackage (Just "1.0.0.1") "mono-traversable" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _monoid_transformer <-  (hackage (Just "0.0.3") "monoid-transformer") >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-monoid-transformer"
  _mtl <- hackage (Just "2.2.1") "mtl" >>= patch $(embedFile "patches/mtl.diff") >>= debianize [] >>= inGroups ["platform"] >>= askip (Reason "use standard")
  _mtl_compat <-  (hackage (Just "0.2.1.3") "mtl-compat") >>= debianize [] >>= skip (Reason "build failure")
  _mtl_unleashed <-  (git "https://github.com/seereason/mtl-unleashed" []) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _mtlparse <-  (hackage (Just "0.1.4.0") "mtlparse") >>= debianize []
  _multimap <-  (hackage (Just "1.2.1") "multimap") >>= debianize []
  _multipart <-  (hackage (Just "0.1.2") "multipart") >>= debianize [] >>= inGroups ["platform"]
  _multiset <-  (hackage (Just "0.3.3") "multiset") >>= debianize []
  _murmur_hash <-  (hackage (Just "0.1.0.9") "murmur-hash") >>= debianize []
  _mwc_random <-  (hackage (Just "0.13.4.0") "mwc-random") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _mysql <- hackage (Just "0.1.1.8") "mysql" >>= flag (P.BuildDep "libmysqlclient-dev") >>= debianize [] >>= skip (Reason "dependencies")
  _mysql_simple <- hackage (Just "0.2.2.5") "mysql-simple" >>= flag (P.BuildDep "libmysqlclient-dev") >>= debianize [] >>= skip (Reason "dependencies")
  _nano_hmac <- hackage Nothing "nano-hmac" >>= patch $(embedFile "patches/nano-hmac.diff") >>= flag (P.DebVersion "0.2.0ubuntu1") >>= debianize []
  _nanospec <-  (hackage (Just "0.2.1") "nanospec" >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize [] -- avoid circular dependency nanospec <-> silently
  -- Empty as of ghc-7.10
  _nats <-  (hackage (Just "1.1.1") "nats") >>= debianize [] >>= flag P.NoDoc >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- Deprecated in favor if conduit-extra
  -- _network_conduit <- hackage (Just "1.1.0") "network-conduit" >>= debianize []
  _newtype_generics <- hackage (Just "0.5") "newtype-generics" >>=
                       debianize [] >>=
                       (if rel == Artful then flag (P.DebVersion "0.5-2build1") else return) >>=
                       inGroups ["autobuilder-group"]
  _numeric_extras <-  (hackage (Just "0.1") "numeric-extras") >>= debianize []
  _numInstances <-  (hackage (Just "1.4") "NumInstances") >>= debianize []
  _objectName <-  (hackage (Just "1.1.0.1") "ObjectName") >>= debianize []
  _oo_prototypes <-  (hackage (Just "0.1.0.0") "oo-prototypes") >>= debianize []
  _openGL <-  (hackage (Just "2.13.1.0") "OpenGL" >>= inGroups ["gl"] >>= flag (P.DevelDep "libglu1-mesa-dev")) >>= debianize [] >>= skip (Reason "too old for openglraw")
  _openGLRaw <-  (hackage (Just "3.2.1.0") "OpenGLRaw" >>= inGroups ["gl"]
                     >>= flag (P.DevelDep "libgl1-mesa-dev")) >>= debianize []
  _openid <-  (hackage (Just "0.2.0.2") "openid" >>= patch $(embedFile "patches/openid.diff")) >>= debianize [] >>= skip (Reason "No instance for (Applicative (Assoc m))")
           {-  , P.Package { P.spec =  (Patch (Hackage (Just "0.2.0.2") "openid") $(embedFile "patches/openid-ghc76.diff")) >>= Debianize []
                           , P.flags = [] } -}
  _operational <- hackage (Just "0.2.3.3") "operational" >>= flag P.OmitLTDeps >>= debianize []
  _ordered <-  (hackage (Just "0.1") "ordered") >>= debianize []
  _pango <-  (hackage (Just "0.13.1.1") "pango") >>= debianize [] >>= skip (Reason "see cairo")
  _parallel <-  (hackage (Just "3.2.1.0") "parallel") >>= debianize [] >>= inGroups ["platform"]
  _parse_dimacs <-  (hackage (Just "1.3") "parse-dimacs") >>= debianize []
  _parsers <-  (hackage (Just "0.12.4") "parsers" {- >>= patch $(embedFile "patches/parsers.diff") -}) >>= debianize []
  _pbkdf2 <-  (hackage (Just "0.3.1.5") "PBKDF2") >>= debianize [] >>= skip (Reason "[libghc-multiset-dev (<< 0.3)] -> []")
               -- , apt (rel release "wheezy" "quantal") "haskell-pcre-light"
  _pcre_light <- hackage (Just "0.4.0.4") "pcre-light" >>=
                 -- Tell it that build tool libpcre means deb libpcre3-dev, and tell
                 -- it to install libpcre3-dev.
                 flag (P.CabalDebian ["--exec-map", "libpcre:libpcre3-dev"]) >>=
                 flag (P.DevelDep "libpcre3-dev") >>=
                 debianize [] >>=
                 inGroups ["ghc-libs", "ghcjs-libs"]
  _permutation <-  (hackage (Just "0.5.0.5") "permutation") >>= debianize []
  _pipes <-  (hackage (Just "4.2.0") "pipes") >>= debianize []
  _placeholders <-  (hackage (Just "0.1") "placeholders") >>= debianize []
  _plugins_auto <-  (hackage (Just "0.0.4") "plugins-auto" >>= patch $(embedFile "patches/plugins-auto.diff")) >>= debianize [] >>= skip (Reason "Couldn't match expected type ‘Int#’ with actual type ‘Int’")
  _plugins <- git "https://github.com/stepcut/plugins" [] >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-dev"]) >>= flag (P.CabalDebian ["--missing-dependency", "libghc-cabal-prof"]) >>= debianize [] >>= skip (Reason "obsolete")
  _plugins_ng <-  (git "https://github.com/ddssff/plugins-ng" []) >>= debianize [] >>= skip (Reason "needs fsnotify << 0.2")
  _po4a <- apt "wheezy" "po4a" >>= skip (Reason "use standard trusty version")
  _pointed <- git "https://github.com/ekmett/pointed" [] >>= debianize []
  _pointedlist <-  (hackage (Just "0.6.1") "pointedlist") >>= debianize []
  -- We can't put upgraded versions of pretty in the repo because the template haskell
  -- version (which is bundled with ghc) conflicts, in particular via th-typegraph.
  -- _pretty <- git "https://github.com/ddssff/pretty" [] >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "pretty"]
  _primitive <-
      -- 0.6.1.0 depends on base<4.9, ghc-prim<0.5, transformers<0.5, so for ghc8 we probably need 0.6.2.0
      -- hackage (Just "0.6.1.0") "primitive" >>=
      hackage (Just "0.6.2.0") "primitive" >>=
      debianize []
  _processing <-  (hackage (Just "1.2.0.2") "processing") >>= debianize [] >>= skip (Reason "[libghc-multiset-prof (<< 0.3)] -> []")
  _pseudomacros <-  (hackage (Just "0.0.2") "pseudomacros") >>= debianize []
  _psQueue <- (hackage (Just "1.1") "PSQueue"
                              >>= pflag (P.DebVersion "1.1-2")
                              >>= qflag (P.DebVersion "1.1-2build2")
                              >>= sflag (P.DebVersion "1.1-1")
                              >>= tflag (P.DebVersion "1.1-4")) >>= debianize [] >>= wskip
  _psqueues <- hackage (Just "0.2.2.2") "psqueues" >>= debianize []
  _publicsuffixlist <-  (hackage (Just "0.1") "publicsuffixlist" >>= tflag (P.DebVersion "0.1-1build4")) >>= debianize [] >>= inGroups ["platform"]
  _pwstore_purehaskell <-  (hackage (Just "2.1.4") "pwstore-purehaskell"
                              >>= flag (P.SkipVersion "2.1.2")
                              -- >>= patch $(embedFile "patches/pwstore-purehaskell.diff")
                              -- >>= flag (P.DebVersion "2.1-1~hackage1")
                           ) >>= debianize []
               -- Retired
               -- , apt (rel release "wheezy" "quantal") "haskell-quickcheck1"
  _quickcheck_gent <-  (hackage Nothing "QuickCheck-GenT") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-quickcheck2-dev (<< 2.7) libghc-quickcheck2-prof (<< 2.7)")
  -- _quickcheck_instances <-  (hackage (Just "0.3.12") "quickcheck-instances") >>= debianize []
  _quickcheck_io <-  (hackage (Just "0.1.3") "quickcheck-io") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- quickCheck1 =  (hackage "QuickCheck" >>= flag (P.CabalPin "1.2.0.1") >>= flag (P.DebVersion "1.2.0.1-2") >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize []
  _reducers <- hack (Just "3.12.1") "reducers"
  _reform <- git "https://github.com/Happstack/reform.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _reform_hamlet <- git "https://github.com/Happstack/reform-hamlet.git" [] >>= debianize [] >>= inGroups ["happstack", "important"]
  _regex_applicative <-  (hackage (Just "0.3.3") "regex-applicative") >>= debianize []
  _regex_compat_tdfa <-  (hackage (Just "0.95.1.4") "regex-compat-tdfa") >>= debianize []
  _regexpr <-  (hackage (Just "0.5.4") "regexpr" >>= flag (P.DebVersion "0.5.4-5build1")) >>= debianize []
  _regex_tdfa_rc <-  (hackage (Just "1.1.8.3") "regex-tdfa-rc"
                              >>= apply (substitute "regex-tdfa-rc" "regex-tdfa")) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  -- reified_records =  (hackage (Just "0.2.2") "reified-records" >>= patch $(embedFile "patches/reified-records.diff")) >>= debianize []
  _reified_records <-  (hg "https://bitbucket.org/ddssff/reified-records") >>= debianize []
  _resource_pool <- hackage (Just "0.2.3.2") "resource-pool" >>= debianize []
  _rJson <-  hackage (Just "0.3.7") "RJson" >>=
             patch $(embedFile "patches/RJson.diff") >>=
             wflag (P.DebVersion "0.3.7-1~hackage1") >>=
             debianize [] >>=
             skip (Reason "Ambiguous occurrence ‘escape’")
  _rsa <-  (hackage (Just "2.2.0") "RSA") >>= debianize [] >>= inGroups ["authenticate", "important"]
  _rss <-  (hackage (Just "3000.2.0.5") "rss" {- >>= patch $(embedFile "patches/rss.diff") -}) >>= debianize [] >>= skip (Reason "time dependency")
  _safeSemaphore <-  (hackage (Just "0.10.1") "SafeSemaphore") >>= debianize [] >>= inGroups ["happstack", "important"]
  _sandi <-  (hackage (Just "0.4.0") "sandi") >>= debianize [] -- replaces dataenc
  _sat <-  (hackage (Just "1.1.1") "sat"
                              >>= patch $(embedFile "patches/sat.diff")
                              >>= flag (P.DebVersion "1.1.1-1~hackage1")) >>= debianize []
  _scotty <- hackage (Just "0.10.2") "scotty" {- >>= patch $(embedFile "patches/scotty.diff") -} >>= debianize [] >>= skip (Reason "data-default dependency")
  _seclib <-  (darcs ("http://src.seereason.com/seclib")) >>= debianize [] >>= skip (Reason "No instance for (Applicative (Sec s))")
  _securemem <-  (hackage (Just "0.1.9") "securemem") >>= debianize []
  _setenv <-  (hackage (Just "0.1.1.3") "setenv") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _set_monad <-  (hackage (Just "0.2.0.0") "set-monad") >>= debianize []
  _shake <-  (hackage (Just "0.15.10") "shake") >>= debianize [] >>= inGroups ["ghcjs-libs"] >>= skip (Reason "dependencies")
  _shakespeare_js <-  (hackage (Just "1.3.0") "shakespeare-js") >>= debianize [] >>= skip (Reason "No input files to haddock?")
  _shellmate <- hack (Just "0.3.3") "shellmate"
  _shelly <- hackage (Just "1.6.8.1") "shelly" >>= debianize [] >>= inGroups ["ghcjs-comp"]
  _silently <-  (hackage (Just "1.2.5") "silently") >>= debianize []
  _simple_reflect <-  (hackage (Just "0.3.2") "simple-reflect") >>= debianize []
  _simple_sendfile <-  (hackage (Just "0.2.25") "simple-sendfile") >>= debianize []
  _singletons <- hackage (Just "2.1") "singletons" >>= flag (P.CabalPin "2.1") >>= debianize [] -- 2.2 requires base-4.9
  _smallcheck <-  (hackage (Just "1.1.1") "smallcheck") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _smtpClient <-  (hackage (Just "1.1.0") "SMTPClient") >>= debianize []
  _snap_core <- hackage (Just "0.9.5.0") "snap-core" >>= debianize [] >>= skip (Reason "glib")
  _snap_server <- hackage (Just "0.9.3.4") "snap-server" >>= debianize [] >>= skip (Reason "snap-core, glib")
  _sodium <- hackage (Just "0.11.0.3") "sodium" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= skip (Reason "dependencies")
  _sourcemap <-  (hackage (Just "0.1.6") "sourcemap") >>= debianize [] >>= inGroups ["happstack", "important"]
  _spine <-  (hackage (Just "0.1") "spine") >>= debianize []
  _spoon <-  hackage (Just "0.3.1") "spoon" >>= debianize []
  _srcloc <-  (hackage (Just "0.5.1.0") "srcloc") >>= debianize []
  -- _stack <- hackage Nothing "stack" >>= debianize []
  _stb_image <-  (hackage (Just "0.2.1") "stb-image") >>= debianize []
  _stm_chans <-  (hackage (Just "3.0.0.4") "stm-chans") >>= debianize [] >>= inGroups ["platform"]
  _stm <-  (hackage (Just "2.4.4.1") "stm") >>= debianize [] >>= inGroups ["platform"] >>= askip (Reason "use standard")
  _strict <-  (hackage (Just "0.3.2") "strict"
                              >>= pflag (P.DebVersion "0.3.2-2")
                              >>= tflag (P.DebVersion "0.3.2-7")) >>= debianize [] -- apt (rel release "wheezy" "quantal") "haskell-strict" -- for leksah
               -- ,  (hackage (Just "0.2.4.1") "strict-concurrency" >>= wflag (P.DebVersion "0.2.4.1-2")) >>= debianize []
  _strict_io <-  (hackage (Just "0.2.1") "strict-io") >>= debianize [] >>= inGroups ["GenI"] >>= skip (Reason "dependencies are missing: deepseq >=1.1 && <1.4")
  _stringable <-  (hackage (Just "0.1.3") "stringable") >>= debianize [] -- this can be done with listlike-instances
  _string_conversions <- hackage (Just "0.4") "string-conversions" >>= debianize [] >>= inGroups ["servant"]
  _stringbuilder <-  (hackage (Just "0.5.0") "stringbuilder") >>= debianize []
  _stringsearch <-  (hackage (Just "0.3.6.6") "stringsearch") >>= debianize []
  _sunroof_compiler <-  (git "http://github.com/ku-fpg/sunroof-compiler" [] >>= patch $(embedFile "patches/sunroof-compiler.diff")) >>= debianize [] >>= skip (Reason "Setup.hs:3:1: parse error on input ‘import’")
  _syb <-  (hackage (Just "0.6") "syb") >>= debianize [] >>= inGroups ["platform"] -- haskell-src-meta requres syb<0.6
  _syb_with_class_instances_text <-
                  (hackage (Just "0.0.1") "syb-with-class-instances-text"
                              >>= pflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.DebVersion "0.0.1-3")
                              >>= wflag (P.SkipVersion "0.0.1-3")
                              >>= tflag (P.DebVersion "0.0.1-6build1")) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= broken
  _system_fileio <-  (hackage (Just "0.3.16.3") "system-fileio") >>= debianize []
  _tagshare <-  (hackage (Just "0.0") "tagshare") >>= debianize []
  _tar <-  (hackage (Just "0.5.0.3") "tar" >>= flag (P.CabalDebian ["--cabal-flags", "-old-time"])) >>= debianize []
           {-  -- This is built into ghc-7.8.3
               ,  (hackage (Just "0.3.2.5") "terminfo"
                                        >>= flag (P.DevelDep "libncurses5-dev")
                                        >>= flag (P.DevelDep "libncursesw5-dev")) >>= debianize -}
  _tasty <-  (hackage (Just "0.11.0.3") "tasty") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _tasty_hunit <-  (hackage (Just "0.9.2") "tasty-hunit") >>= debianize []
  _tasty_golden <-  (hackage (Just "2.3.1") "tasty-golden") >>= debianize []
  _tasty_quickcheck <-  (hackage (Just "0.8.4") "tasty-quickcheck") >>= debianize []
  _tasty_smallcheck <-  (hackage (Just "0.8.1") "tasty-smallcheck") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _template_default <-  (hackage (Just "0.1.1") "template-default" >>= patch $(embedFile "patches/template-default.diff")) >>= debianize [] >>= skip (Reason "Not in scope: data constructor ‘ClassP’")
  _tensor <- hackage (Just "1.0.0.1") "Tensor" >>= tflag (P.DebVersion "1.0.0.1-2") >>= debianize [] >>= broken
  _testing_feat <- hackage (Just "0.4.0.3") "testing-feat" >>= {-patch $(embedFile "patches/testing-feat.diff") >>=-} debianize []
  _text_binary <- hackage (Just "0.2.1") "text-binary" >>= debianize [] >>= inGroups ["ghcjs-libs"]
  _text <-  (hackage (Just "1.2.2.1") "text" >>= flag (P.CabalDebian ["--cabal-flags", "-integer-simple"]) >>= flag (P.CabalDebian ["--no-tests"])) >>= debianize [] >>= inGroups ["platform", "test8"] >>= askip (Reason "use standard")
  _text_icu <-  (hackage (Just "0.7.0.1") "text-icu" >>= flag (P.DevelDep "libicu-dev")) >>= debianize []
  _text_show <-  (hackage (Just "3.3") "text-show") >>= debianize []
  _text_stream_decode <-  (hackage (Just "0.1.0.5") "text-stream-decode" >>= patch $(embedFile "patches/text-stream-decode.diff")) >>= debianize [] >>= inGroups ["conduit", "important"]
  _th_alpha <-  (git "https://github.com/ddssff/th-alpha.git" []) >>= debianize []
  _th_context <-  (git "http://github.com/seereason/th-context" []) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  -- th_instance_reification =  (git "https://github.com/seereason/th-instance-reification.git" []) >>= debianize []
  _th_kinds_fork <-  (git "http://github.com/ddssff/th-kinds-fork" []) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "th-path", "important"]
  _time_locale_compat <-  (hackage (Just "0.1.1.3") "time-locale-compat") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "happstack", "important"]
  _tinymce <- apt "wheezy" "tinymce"
  _transformers_free <-  (hackage (Just "1.0.1") "transformers-free") >>= debianize []
  _traverse_with_class <-  (hackage (Just "0.2.0.4") "traverse-with-class") >>= debianize [] >>= inGroups ["happstack", "important"]
  _trifecta <-  (hackage (Just "1.5.2") "trifecta" {->>= patch $(embedFile "patches/trifecta.diff")-}) >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-comonad-dev (<< 5) libghc-comonad-prof (<< 5)")
  _tyb <- hackage (Just "0.2.3") "TYB" >>= debianize [] >>= skip (Reason "Needs update for current template-haskell")
  _type_eq <- hackage (Just "0.5") "type-eq" >>= flag (P.BuildDep "cpphs") >>= debianize [] >>= skip (Reason "dependencies")
  _uglymemo <-  (hackage (Just "0.1.0.1") "uglymemo") >>= debianize []
  _unbounded_delays <-  (hackage (Just "0.1.0.9") "unbounded-delays") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _unexceptionalio <- hackage (Just "0.3.0") "unexceptionalio" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _unification_fd <-  (hackage (Just "0.10.0.1") "unification-fd" >>= flag (P.SkipVersion "0.8.0")) >>= debianize []
  _union_find <-  (hackage (Just "0.2") "union-find") >>= debianize []
               -- ,  (hackage "Elm") >>= debianize []
               -- ,  (hackage "elm-server" {- >>= patch $(embedFile "patches/elm-server.diff") -}) >>= debianize []
  _uniplate <-  (hackage (Just "1.6.12") "uniplate") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "appraisalscribe", "important"]
  _units <-  (hackage (Just "2.4") "units") >>= debianize [] >>= skip (Reason "[libghc-singletons-prof (<< 2)] -> []")
  _units_parser <-  (hackage (Just "0.1.0.0") "units-parser") >>= debianize []
  _universe_base <- hackage Nothing "universe-base" >>= debianize []
  _universe_instances_base <- hackage Nothing "universe-instances-base" >>= debianize []
  _universe_reverse_instances <- hackage Nothing "universe-reverse-instances" >>= debianize []
  _unix_time <-  (hackage (Just "0.3.6") "unix-time" {->>= flag (P.CabalDebian ["--no-run-tests"])-}) >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs", "authenticate", "important"] -- doctest assumes cabal build dir is dist
  _unixutils_shadow <-  (hackage (Just "1.0.0") "Unixutils-shadow") >>= debianize []
  _unordered_containers <-  (hackage (Just "0.2.7.1") "unordered-containers") >>= debianize []
               -- Obsolete after ghc-6.10
               -- ,  (hackage "utf8-prelude" >>= flag (P.DebVersion "0.1.6-1~hackage1")) >>= debianize []
               -- The GHC in wheezy conflicts with libghc-containers-dev, so we can't build this.
               -- , wonly $  (hackage "containers") >>= debianize []
  _urlencoded <-  (hackage (Just "0.4.1") "urlencoded" {->>= patch $(embedFile "patches/urlencoded.diff")-}) >>= debianize []
  _utf8_light <-  (hackage (Just "0.4.2") "utf8-light") >>= debianize []
  _vacuum <-  (hackage (Just "2.2.0.0") "vacuum" >>= flag (P.SkipVersion "2.1.0.1")) >>= debianize [] >>= skip (Reason "#error Unsupported GHC version in ClosureTypes.hs!")
  _validation <-  (hackage (Just "0.2.0") "Validation" >>= patch $(embedFile "patches/validation.diff")) >>= debianize []
  _value_supply <-  (hackage (Just "0.6") "value-supply") >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"]
  _vector <- hackage (Just "0.12.0.0") "vector" >>= debianize []
  _vector_algorithms <- hackage (Just "0.7.0.1") "vector-algorithms" >>=
                        patch $(embedFile "patches/vector-algorithms.diff") >>=
                        debianize [] >>=
                        inGroups ["ghcjs-libs", "ghc-libs"]
  _vector_binary_instances <- hackage (Just "0.2.3.4") "vector-binary-instances" >>= debianize [] >>= skip (Reason "vector dependency")
  _vector_space <- ghc7 $ (hackage (Just "0.10.3") "vector-space") >>= debianize []
  _virthualenv <-  (hackage (Just "0.2.2") "virthualenv" >>= patch $(embedFile "patches/virthualenv.diff")) >>= debianize [] >>= skip (Reason "dependencies are missing: filepath >=1.1.0.3 && <1.4")
{-
  _virthualenv <- pure $ P.Package { P._spec = Debianize'' (Patch (Hackage "virthualenv") $(embedFile "patches/virthualenv.diff")) Nothing
                                 , P._flags =  mempty
                                 , P._post = [] } :: TSt Package
-}
  _vty <- ghc7 $ (hackage (Just "5.7.1") "vty") >>= debianize []
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
{-
  _web_routes_mtl <- git "https://github.com/Happstack/web-routes-mtl.git" [] >>= flag (P.DebVersion "0.20.1-1~hackage1") >>= debianize [] >>= inGroups ["happstack", "important"]
-}
              -- web_routes_transformers =  (git "https://github.com/Happstack/web-routes.git" [] >>= cd "web-routes-transformers") >>= debianize [] -- requires transformers ==0.2.*
  _web_routes_wai <- git "https://github.com/Happstack/web-routes-wai.git" [] >>= debianize [] >>= inGroups ["happstack", "important"] >>= skip (Reason "wai depends on obsolete bytestring-builder package")
  _webkit_sodium <- git "https://github.com/ghcjs/ghcjs-examples" [] >>= cd "webkit-sodium" >>= debianize [] >>= inGroups ["ghcjs-libs", "ghc-libs"] >>= skip (Reason "skipped jsaddle")
  _webkitgtk3_javascriptcore <- hackage (Just "0.13.1.1") "webkitgtk3-javascriptcore" >>= debianize [] >>= skip (Reason "Could not find module Gtk2HsSetup")
  _wl_pprint <-  (hackage (Just "1.2") "wl-pprint") >>= debianize []
  _word8 <-  (hackage (Just "0.1.2") "word8") >>= debianize []
  _word_trie <-  (hackage (Just "0.3.0") "word-trie") >>= debianize []
  _xdg_basedir <-  (hackage (Just "0.2.2") "xdg-basedir" >>= tflag (P.DebVersion "0.2.2-2")) >>= debianize []
  _xmlgen <-  (hackage (Just "0.6.2.1") "xmlgen") >>= debianize []
  _xmlhtml <-  (hackage (Just "0.2.3.4") "xmlhtml") >>= debianize [] >>= skip (Reason "Unmet build dependencies: libghc-blaze-builder-dev (<< 0.4)")
  _xml_types <-  (hackage (Just "0.3.6") "xml-types") >>= debianize []
  _yaml_light <-  (hackage (Just "0.1.4") "yaml-light"
                              >>= wflag (P.DebVersion "0.1.4-2")
                              >>= pflag (P.DebVersion "0.1.4-2")
                              >>= qflag (P.DebVersion "0.1.4-2build1")
                              >>= tflag (P.DebVersion "0.1.4-5build1")) >>= debianize []
  -- _yi <-  (hackage (Just "0.12.6") "yi") >>= debianize [] {- >>= skip (Reason "requires hint") -}
  _yi_language <- ghc7 $ (hackage (Just "0.2.1") "yi-language" >>= flag (P.BuildDep "alex")) >>= debianize []
  _yi_rope <-  (hackage (Just "0.7.0.1") "yi-rope") >>= debianize []
  _zlib_bindings <-  (hackage (Just "0.1.1.5") "zlib-bindings") >>= debianize [] >>= inGroups [ "authenticate", "important"]
  _zlib_enum <- hackage (Just "0.2.3.1") "zlib-enum" >>= debianize [] >>= flag (P.CabalDebian ["--no-tests"]) >>= inGroups [ "authenticate", "important"]

  -- Create the ghcjs library package targets
  -- findGroup "ghcjs-libs" >>= mapM_ ghcjs_also
  noTests -- Some package test suites fail, some hang, especially with ghcjs

  return ()
