{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}
module Debian.AutoBuilder.Details.Common where

import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages (flag, Package(spec))
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

named :: String -> [P.Packages] -> P.Packages
named s = P.Named (fromString s) . P.Packages

ghcjs_flags :: P.Package -> P.Package
ghcjs_flags p =
    p `putSrcPkgName` makeSrcPkgName (P.spec p)
      `flag` P.CabalDebian ["--ghcjs", "--no-ghc"]
      `flag` P.CabalDebian ["--source-package-name=" <> makeSrcPkgName (P.spec p)]
      `flag` P.BuildDep "libghc-cabal-ghcjs-dev"
      `flag` P.BuildDep "ghcjs"
      `flag` P.BuildDep "haskell-devscripts (>= 0.8.21.3)"

makeSrcPkgName :: RetrieveMethod -> String
makeSrcPkgName (Hackage n) = "ghcjs-" ++ map toLower n
makeSrcPkgName (Debianize' p s) = fromMaybe (makeSrcPkgName p) s
makeSrcPkgName (Patch p _) = makeSrcPkgName p
makeSrcPkgName (Git url _) = "ghcjs-" ++ takeFileName url -- applying this to an url is sketchy
makeSrcPkgName m = error $ "ghcjs_flags - unsupported target type: " ++ show m

putSrcPkgName :: Package -> String -> Package
putSrcPkgName p name = p {spec = putSrcPkgName' (spec p) name}

putSrcPkgName' :: RetrieveMethod -> String -> RetrieveMethod
putSrcPkgName' (Debianize' cabal _) name = Debianize' cabal (Just name)
putSrcPkgName' (Proc x) name = Proc (putSrcPkgName' x name)
-- More - we need a traversal - is it Typeable yet?
putSrcPkgName' p _ = p

dropPrefix :: Monad m => String -> String -> m String
dropPrefix pre str | isPrefixOf pre str = return $ drop (length pre) str
dropPrefix pre str = fail $ "Expected prefix " ++ show pre ++ ", found " ++ show str
