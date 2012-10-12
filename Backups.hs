module Main where

import System.Archive.Site (BackupTarget(..), backup)

main = backup (BackupTarget {app = "darcs", user = "upload", host = "src.seereason.com", keep = 50})
