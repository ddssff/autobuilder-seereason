{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Debian.AutoBuilder.Types.Packages as P
import Debian.Relation (SrcPkgName(SrcPkgName))
import Debian.Repo.Fingerprint (RetrieveMethod(..), DebSpec(SrcDeb))

data Build = Production | Testing
build = Production
-- build = Testing

repo = "http://src.seereason.com" :: String
localRepo home = "file://" ++ home ++ "/darcs"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs" :: String

happstackRepo = "http://hub.darcs.net/stepcut/happstack" :: String
--happstackRepo = repo ++ "/happstack"

asciiToString :: B.ByteString -> String
asciiToString = map (chr . fromIntegral) . B.unpack

named :: String -> [Packages] -> Packages
named s = P.Named (fromString s) . P.Packages

ghcjs_flags :: Packages -> Packages
ghcjs_flags NoPackage = NoPackage
ghcjs_flags p@(Named {..}) = p {packages = ghcjs_flags packages}
ghcjs_flags p@(Packages {..}) = p {list = map ghcjs_flags list}
ghcjs_flags p@(Package {..}) =
             p `flag` P.CabalDebian ["--ghcjs", "--no-ghc"]
               `flag` P.CabalDebian ["--source-package-name=" <> srcName spec]
               `flag` P.BuildDep "libghc-cabal-ghcjs-dev"
               `flag` P.BuildDep "ghcjs"
               `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.3)"
               `putSrcPkgName` (SrcPkgName (srcName spec))
    where
      srcName :: RetrieveMethod -> String
      srcName (Debianize' p' fs) = case fs of
                                     (SrcDeb (SrcPkgName name) : _) -> name
                                     [] -> srcName (Debianize p')
      srcName (Debianize p') = srcName p'
      srcName (Patch x _) = srcName x
      srcName m = "ghcjs-" <> map toLower (fromMaybe (cabName m) (dropPrefix "haskell-" (cabName m)))
      cabName :: RetrieveMethod -> String
      cabName (Hackage n) = n
      cabName (Debianize p') = cabName p'
      cabName (Debianize' p' _) = cabName p'
      cabName (Cd d _) = d
      cabName _ = error $ "ghcjs_flags - unsupported target type: " ++ show spec

      -- Hack to get the Debianize' retrieve method to look different
      -- for packages built with both ghc and ghcjs.
      putSrcPkgName :: Packages -> SrcPkgName -> Packages
      putSrcPkgName p@(Package {spec = Debianize' m fs}) src = p {spec = Debianize' m (SrcDeb src : filter (not . isSrcDeb) fs)}
      putSrcPkgName p src = p
      isSrcDeb (SrcDeb {}) = True
      isSrcDeb _ = False

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str
