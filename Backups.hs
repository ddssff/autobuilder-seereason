module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget { app = "darcs"
                            , auth = URIAuth { uriUserInfo = "autobuilder@"
                                             , uriRegName = "deb.seereason.com"
                                             , uriPort = "" }
                            , localTop = "/home/autobuilder/backups"
                            , remoteTop = "/srv"
                            , keep = 50 })
