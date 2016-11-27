{-# LANGUAGE FlexibleContexts #-}
import Control.Lens
import Data.Map as Map (insert)
import Debian.AutoBuilder.Details.Versions (seereasonDefaults)
import Debian.Debianize
import Debian.Relation (BinPkgName(..), Relation(Rel), SrcPkgName(..))

main :: IO ()
main = performDebianization $ do
         seereasonDefaults
         (debInfo . sourceFormat) .= Native3
         (debInfo . sourcePackageName) .= Just (SrcPkgName "autobuilder-seereason")
         (debInfo . executable) %= Map.insert (BinPkgName "autobuilder-seereason")
                                              (InstallFile {execName = "autobuilder-seereason",
                                                            sourceDir = Nothing, destDir = Nothing,
                                                            destName = "autobuilder-seereason"})
         (debInfo . executable) %= Map.insert (BinPkgName "seereason-darcs-backups")
                                              (InstallFile {execName = "seereason-darcs-backups",
                                                            sourceDir = Nothing, destDir = Nothing,
                                                            destName = "seereason-darcs-backups"})
         mapM (addDep depends)
                 [ "apt"
                 , "apt-file"
                 , "apt-utils"
                 , "build-essential"
                 , "curl"
                 , "darcs"
                 , "debhelper"
                 , "debian-archive-keyring"
                 , "debootstrap"
                 , "dupload"
                 , "git"
                 -- Used to run debian/Debianize.hs scripts
                 -- , "ghc", "cabal-install", "libghc-autobuilder-seereason-dev"
                 , "mercurial"
                 , "quilt"
                 , "rsync"
                 , "seereason-keyring"
                 , "subversion"
                 , "tla"
                 ]
         mapM (addDep recommends)
                 [ "libghc-happstack-authenticate-dev" -- used by mimo
                 , "libghc-happstack-foundation-dev" -- used by mimo
                 , "libghc-hsp-dev" -- used by mimo
                 , "libghc-safecopy-dev" -- used by mimo
                 , "libghc-seereason-ports-dev" -- used by most (all?) of our web apps
                 , "libghc-text-dev"
                 , "libghc-utility-ht-dev" -- used by mimo
                 ]

         addDep conflicts "autobuilder"
         addDep replaces "autobuilder"

addDep lns dep =
    let pkg = BinPkgName "autobuilder-seereason" in
    (debInfo . binaryDebDescription pkg . relations . lns) %= (++ [[Rel (BinPkgName dep) Nothing Nothing]])
