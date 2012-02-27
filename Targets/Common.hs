module Targets.Common where

import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Debian.AutoBuilder.Params as P
import qualified Debian.AutoBuilder.Types.Packages as P

data Build = Production | Testing
build = Production
-- build = Testing

repo = "http://src.seereason.com"
localRepo home = "file://" ++ home ++ "/darcs"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs"

happstackRepo = "http://patch-tag.com/r/mae/happstack"
--happstackRepo = repo ++ "/happstack"
