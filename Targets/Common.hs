module Targets.Common where

import qualified Data.ByteString as B
import Data.Char (chr, toLower)
import Data.Function (on)
import Data.List (sortBy)
import Data.String (fromString)
import qualified Data.Map as Map
import Debian.AutoBuilder.Types.Packages (Packages, RetrieveMethod(Debianize, Patch, Hackage, Darcs))
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Types.Packages as P

data Build = Production | Testing
build = Production
-- build = Testing

repo = "http://src.seereason.com"
localRepo home = "file://" ++ home ++ "/darcs"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs"

happstackRepo = "http://hub.darcs.net/stepcut/happstack"
--happstackRepo = repo ++ "/happstack"

asciiToString :: B.ByteString -> String
asciiToString = map (chr . fromIntegral) . B.unpack
