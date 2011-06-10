module Main where

import Extra.SSH (sshExport)
import System.Archive.Prune (prune)
import System.Archive.UpdateMirror
import System.Environment (getArgs, withArgs)
import System.Exit (ExitCode(ExitSuccess, ExitFailure), exitWith)
import System.IO (hPutStr, hPutStrLn, stderr)

-- * Update List of Targets Here

-- Other than the hostname, these values are based on those passed to
-- the server.  However, we can't really know what those are because
-- the server runs on a remote system.
target = "darcs"
user = "upload"
host = "src.seereason.com"
local = "/srv/backups/" ++ target

-- Should be equivalent to "%Y-%m-%d_%H:%M:%S", but this seems to work
-- better when passed to the prune function and used to parse a date.
format = "%F_%T"

mytargets :: [Target]
mytargets =
    [ RsyncTarget { prettyName = target
                  , src = [ user ++ "@" ++ host ++ ":/srv/" ++ target
                          ]
                  , dest = local ++ "/"
                  , config = genericConfig target format
                  , options = [Rsync "--progress", Rsync "--stats"]
                  }
    ]

main :: IO ()
main = getArgs >>= return . elem "--initialize" >>= \ init ->
       if init
       then hPutStr stderr ("Authenticating connection with " ++ user ++ "@" ++ host ++ "...") >>
            sshExport (user ++ "@" ++ host) Nothing >>=
            either (\ s -> hPutStrLn stderr ("initialization failed: " ++ s) >> exitWith (ExitFailure 1))
                   (\ () -> hPutStrLn stderr "done." >> exitWith ExitSuccess)
       else withArgs [target] (updateMirrorMain mytargets) >>
            prune format local (target ++ "-") 50
