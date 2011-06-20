module Targets.Common where

import Debian.AutoBuilder.ParamClass (Target(..))

data Build = Production | Testing
build = Production
--build = Testing

repo = "http://src.seereason.com"
localRepo home = "file://" ++ home ++ "/darcs"
privateRepo = "ssh://upload@src.seereason.com/srv/darcs"

happstackRepo = "http://patch-tag.com/r/mae/happstack"
--happstackRepo = repo ++ "/happstack"
