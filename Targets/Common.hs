module Targets.Common where

import Data.Function (on)
import qualified Debian.AutoBuilder.Params as P

data Build = Production | Testing
build = Production
-- build = Testing

repo = "http://src.seereason.com"
localRepo home = "file://" ++ home ++ "/darcs"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs"

happstackRepo = "http://patch-tag.com/r/mae/happstack"
--happstackRepo = repo ++ "/happstack"

-- |Make sure the targets are in alphabetical order by debian source package name.
checkOrder ts =
    check ts
    where
      check (a : b : more) =
          case (compare `on` P.name) a b of
            GT -> error $ "Misordered targets: " ++ P.name a ++ " precedes " ++ P.name b
            _ -> check (b : more)
      check _ = ts
