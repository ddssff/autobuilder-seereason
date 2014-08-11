{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
module Debian.AutoBuilder.Details.Common where

import Debian.AutoBuilder.Types.Packages as P
import qualified Data.ByteString as B
import Data.Char (chr)
import Data.String (IsString(fromString))

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

named :: String -> [Packages] -> Packages
named s = P.Named (fromString s) . P.Packages
