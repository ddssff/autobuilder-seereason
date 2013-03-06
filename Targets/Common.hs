module Targets.Common where

import qualified Data.ByteString as B
import Data.Char (chr)

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
