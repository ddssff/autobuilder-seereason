{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (isNothing)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Debian.AutoBuilder.Types.Packages as P
import Debian.Repo.Fingerprint (RetrieveMethod(..))
import System.FilePath (takeFileName)

data Build = Production | Testing
build = Production
-- build = Testing

-- repo = "http://src.seereason.com" :: String
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
    p `putSrcPkgName` makeSrcPkgName spec
      `flag` P.CabalDebian ["--ghcjs", "--no-ghc"]
      `flag` P.CabalDebian ["--source-package-name=" <> makeSrcPkgName spec]
      `flag` P.BuildDep "libghc-cabal-ghcjs-dev"
      `flag` P.BuildDep "ghcjs"
      `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.3)"

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize p) = makeSrcPkgName p
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ takeFileName url -- applying this to an url is sketchy
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: Packages -> String -> Packages
putSrcPkgName p@(Package {spec = Debianize _, flags = flags}) name =
    p {flags = SourceDebName name : filter (isNothing . sourceDebName) flags}
putSrcPkgName p _ = p

sourceDebName (SourceDebName s) = Just s
sourceDebName _ = Nothing

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str
