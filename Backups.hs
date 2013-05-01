module Main where

import Network.URI (URIAuth(..))
import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget { app = "darcs"
                            , auth = URIAuth { uriUserInfo = "upload@"
                                             , uriRegName = "src.seereason.com"
                                             , uriPort = "" }
                            , localTop = "/home/autobuilder/backups"
                            , remoteTop = "/srv"
                            , keep = 50 })
