{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-binds -fno-warn-unused-imports -fno-warn-name-shadowing #-}
module Targets.Public ( targets ) where

import Data.Char (toLower)
import Data.Set (empty, singleton)
import qualified Debian.AutoBuilder.Types.Packages as P
import Debian.AutoBuilder.Types.Packages
import Targets.Common (repo, localRepo, happstackRepo)

-- |the _home parameter has an underscore because normally it is unused, but when
-- we need to build from a local darcs repo we use @localRepo _home@ to compute
-- the repo location.
targets :: String -> String -> P.Packages
targets _home release =
    P.Packages empty $
    [ main _home release
    , autobuilder _home
    , digestiveFunctors
    , authenticate _home release
    , happstackdotcom _home
    , happstack
    -- , fixme
    -- , higgsset
    -- , jsonb
    -- , glib
    -- , plugins
    -- , frisby
    -- , failing
    -- , algebra
    -- , agda
    -- , other
    ]

fixme =
    P.Packages (singleton "fixme") $
    [ debianize "test-framework-smallcheck" []
    , P.Package { P.name = "haskell-geni"
                , P.spec = DebDir (Darcs "http://code.haskell.org/GenI") (Darcs (repo ++ "/haskell-geni-debian"))
                , P.flags = [] }
    ]

unixutils _home =
    P.Packages (singleton "Unixutils")
    [ P.Package { P.name = "haskell-unixutils"
                , P.spec = Darcs (repo ++ "/haskell-unixutils")
                , P.flags = [] }
    , P.Package { P.name = "haskell-progress"
                , P.spec = Debianize (Darcs (repo ++ "/progress"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-extra"
                , P.spec = Darcs "http://src.seereason.com/haskell-extra"
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-help"
                , P.spec = Darcs "http://src.seereason.com/haskell-help"
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-scaffolding"
                , P.spec = Darcs (repo ++ "/happstack-scaffolding")
                           -- Don't use Debianize here, it restores the doc package which crashes the build
                , P.flags = [] } ]

autobuilder _home =
    P.Packages (singleton "autobuilder") $
    [ unixutils _home
    , P.Package { P.name = "autobuilder"
                , P.spec = Darcs (repo ++ "/autobuilder")
                , P.flags = [] }
    , P.Package { P.name = "haskell-cabal-debian"
                , P.spec = Darcs (repo ++ "/cabal-debian")
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian"
                , P.spec = Darcs (repo ++ "/haskell-debian")
                , P.flags = [P.RelaxDep "cabal-debian"] }
    , P.Package { P.name = "haskell-debian-mirror"
                , P.spec = Darcs "http://src.seereason.com/mirror"
                , P.flags = [] }
    , P.Package { P.name = "haskell-debian-repo"
                , P.spec = Darcs "http://src.seereason.com/haskell-debian-repo"
                , P.flags = [] }
    , P.Package { P.name = "haskell-archive"
                , P.spec = Darcs "http://src.seereason.com/archive"
                , P.flags = [] } ]

digestiveFunctors =
    P.Packages (singleton "digestive-functors")
    [ debianize "digestive-functors" [P.CabalPin "0.2.1.0" ]  -- Waiting to move all these packages to 0.3.0.0 when hsp support is ready
    , debianize "digestive-functors-blaze" [P.CabalPin "0.2.1.0", P.DebVersion "0.2.1.0-1~hackage1" ]
    , P.Package { P.name = "haskell-digestive-functors-happstack"
                , P.spec = Debianize (Hackage "digestive-functors-happstack")
                , P.flags = [P.CabalPin "0.1.1.5"] }
    , P.Package { P.name = "haskell-digestive-functors-hsp"
                , P.spec = Debianize (Patch 
                                      (Darcs (repo ++ "/digestive-functors-hsp"))
                                      (unlines
                                       [ "--- old/digestive-functors-hsp.cabal\t2012-04-13 19:27:00.000000000 -0700"	
                                       , "+++ new/digestive-functors-hsp.cabal\t2012-04-17 05:50:20.146609985 -0700"	
                                       , "@@ -15,7 +15,7 @@"				    
                                       , " Library"	  
                                       , "   Exposed-modules:     Text.Digestive.HSP.Html4"	
                                       , "   Build-depends:       base == 4.*,"		
                                       , "-                       digestive-functors >= 0.2 && < 0.3,"	
                                       , "+                       digestive-functors >= 0.2,"	
                                       , "                        hsp,"	      
                                       , "                        hsx,"	      
                                       , "                        text" ]))
                , P.flags = [] } ]

happstack =
    P.Packages (singleton "happstack")
    [ P.Package { P.name = "happstack-debianization"
                , P.spec = Darcs "http://src.seereason.com/happstack-debianization"
                , P.flags = [] }
    , P.Package { P.name = "haskell-seereason-base"
                , P.spec = Darcs (repo ++ "/seereason-base")
                , P.flags = [] }
    , debianize "happstack" []
    , P.Package { P.name = "haskell-happstack-data"
                , P.spec = Debianize (Patch 
                                      (Hackage "happstack-data") 
                                      (unlines
                                       [ "--- old/happstack-data.cabal\t2012-04-16 09:55:33.000000000 -0700"
                                       , "+++ new/happstack-data.cabal\t2012-04-16 10:38:09.015673204 -0700"
                                       , "@@ -68,7 +68,7 @@"
                                       , "   Build-Depends:      binary,"
                                       , "                       bytestring,"
                                       , "                       containers,"
                                       , "-                      mtl >= 1.1 && < 2.1,"
                                       , "+                      mtl >= 1.1,"
                                       , "                       pretty,"
                                       , "                       syb-with-class-instances-text,"
                                       , "                       text >= 0.10 && < 0.12," ]))
                , P.flags = [P.DebVersion "6.0.1-1build1"] }
    , P.Package { P.name = "haskell-happstack-extra"
                , P.spec = Darcs (repo ++ "/happstack-extra")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-facebook"
                , P.spec = Darcs (repo ++ "/happstack-facebook")
                , P.flags = [] }
    , debianize "happstack-hsp" []
    -- Version 6.1.0, which is just a wrapper around the non-happstack
    -- ixset package, has not yet been uploaded to hackage.
    -- , debianize "happstack-ixset" []
    , P.Package { P.name = "haskell-happstack-ixset"
                , P.spec = DebDir (Cd "happstack-ixset" (Darcs happstackRepo)) (Darcs (repo ++ "/happstack-ixset-debian"))
                , P.flags = [] }

    , debianize "happstack-jmacro" []
    , P.Package { P.name = "haskell-happstack-search"
                , P.spec = Darcs (repo ++ "/happstack-search")
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-server"
                , P.spec = Debianize (Hackage "happstack-server")
                , P.flags = [P.DebVersion "7.0.0-1~hackage1", P.CabalPin "7.0.0"] }
    , P.Package { P.name = "haskell-happstack-util"
                , P.spec = Debianize (Patch
                                      (Hackage "happstack-util") 
                                      (unlines
                                       [ "--- old/happstack-util.cabal\t2012-04-17 05:38:25.000000000 -0700"	
                                       , "+++ new/happstack-util.cabal\t2012-04-17 05:53:09.430600945 -0700"	
                                       , "@@ -29,7 +29,7 @@"			    
                                       , "                        directory,"	    
                                       , "                        extensible-exceptions, "	
                                       , "                        hslogger >= 1.0.2,"	  
                                       , "-                       mtl >= 1.1 && < 2.1,"  
                                       , "+                       mtl >= 1.1,"	 
                                       , "                        old-locale,"	 
                                       , "                        old-time,"	 
                                       , "                        parsec < 4," ]))
                , P.flags = [P.DebVersion "6.0.3-1"] }
    ]

main _home release =
    P.Packages (singleton "main") $
    [ ghc release,
      platform release,
      debianize "hashtables" []
    , P.Package { P.name = "cpphs"
                , P.spec = Apt "sid" "cpphs"
                , P.flags = [] }
    , P.Package { P.name = "debootstrap"
                , P.spec = Apt "sid" "debootstrap"
                , P.flags = [P.UDeb "debootstrap-udeb"] }
    , apt release "geneweb"
    , P.Package { P.name = "gtk2hs-buildtools"
                , P.spec = Debianize (Hackage "gtk2hs-buildtools")
                , P.flags =
                    [ P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"
                    , P.ExtraDep "alex"
                    , P.ExtraDep "happy"
                    , P.Revision "" ] }
    , P.Package { P.name = "haskell-acid-state"
                , P.spec = Debianize (Patch
                                      (Hackage "acid-state")
                                      (unlines $
                                       [ "diff -ru old/src/Data/Acid/Local.hs new/src/Data/Acid/Local.hs"
                                       , "--- old/src/Data/Acid/Local.hs\t2012-03-25 21:24:51.000000000 -0700"
                                       , "+++ new/src/Data/Acid/Local.hs\t2012-03-29 16:49:16.030316334 -0700"
                                       , "@@ -195,9 +195,9 @@"
                                       , "          n <- case mbLastCheckpoint of"
                                       , "                 Nothing"
                                       , "                   -> return 0"
                                       , "-                Just (Checkpoint eventCutOff content)"
                                       , "+                Just (Checkpoint eventCutOff content, file)"
                                       , "                   -> do modifyCoreState_ core (\\_oldState -> case runGetLazy safeGet content of"
                                       , "-                                                               Left msg  -> checkpointRestoreError msg"
                                       , "+                                                               Left msg  -> checkpointRestoreError file msg"
                                       , "                                                                Right val -> return val)"
                                       , "                         return eventCutOff"
                                       , " "
                                       , "@@ -210,8 +210,8 @@"
                                       , "                                          , localCheckpoints = checkpointsLog"
                                       , "                                          }"
                                       , " "
                                       , "-checkpointRestoreError msg"
                                       , "-    = error $ \"Could not parse saved checkpoint due to the following error: \" ++ msg"
                                       , "+checkpointRestoreError file msg"
                                       , "+    = error $ \"Could not parse saved checkpoint from \" ++ file ++ \" due to the following error: \" ++ msg"
                                       , " "
                                       , " -- | Close an AcidState and associated logs."
                                       , " --   Any subsequent usage of the AcidState will throw an exception."
                                       , "diff -ru old/src/Data/Acid/Log.hs new/src/Data/Acid/Log.hs"
                                       , "--- old/src/Data/Acid/Log.hs\t2012-03-29 15:59:07.469454276 -0700"
                                       , "+++ new/src/Data/Acid/Log.hs\t2012-03-29 16:48:10.720124087 -0700"
                                       , "@@ -236,22 +236,22 @@"
                                       , " -- Implementation: Search the newest log files first. Once a file"
                                       , " --                 containing at least one valid entry is found,"
                                       , " --                 return the last entry in that file."
                                       , "-newestEntry :: SafeCopy object => LogKey object -> IO (Maybe object)"
                                       , "+newestEntry :: SafeCopy object => LogKey object -> IO (Maybe (object, FilePath))"
                                       , " newestEntry identifier"
                                       , "     = do logFiles <- findLogFiles identifier"
                                       , "          let sorted = reverse $ sort logFiles"
                                       , "              (_eventIds, files) = unzip sorted"
                                       , "          archives <- mapM Lazy.readFile files"
                                       , "-         return $ worker archives"
                                       , "+         return $ worker (zip archives files)"
                                       , "     where worker [] = Nothing"
                                       , "-          worker (archive:archives)"
                                       , "+          worker ((archive, file):archives)"
                                       , "               = case Archive.readEntries archive of"
                                       , "                   Done            -> worker archives"
                                       , "-                  Next entry next -> Just (decode' (lastEntry entry next))"
                                       , "-                  Fail msg        -> error msg"
                                       , "-          lastEntry entry Done   = entry"
                                       , "-          lastEntry entry (Fail msg) = error msg"
                                       , "-          lastEntry _ (Next entry next) = lastEntry entry next"
                                       , "+                  Next entry next -> Just (decode' (lastEntry file entry next), file)"
                                       , "+                  Fail msg        -> error (file ++ \": \" ++ msg)"
                                       , "+          lastEntry _ entry Done   = entry"
                                       , "+          lastEntry file entry (Fail msg) = error (file ++ \": \" ++ msg)"
                                       , "+          lastEntry file _ (Next entry next) = lastEntry file entry next"
                                       , " "
                                       , " -- Schedule a new log entry. This call does not block"
                                       , " -- The given IO action runs once the object is durable. The IO action" ]))
                , P.flags = [] }
    -- , debianize "AES" [P.DebVersion "0.2.8-1~hackage1"]
    , debianize "aeson" []
    , P.Package { P.name = "haskell-agi"
                , P.spec = Darcs "http://src.seereason.com/haskell-agi"
                , P.flags = [] }
    , debianize "ansi-terminal" [P.DebVersion "0.5.5-3build1"]
    , debianize "ansi-wl-pprint" [P.DebVersion "0.6.4-1"]
    -- Our applicative-extras repository has several important patches.
    , P.Package { P.name = "haskell-applicative-extras",
                  P.spec = Debianize (Hackage "applicative-extras"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.DebVersion "0.1.8-1"] }
    , debianize "asn1-data" [P.DebVersion "0.6.1.3-2"]
    , debianize "attempt" [P.DebVersion "0.4.0-1"]
    , debianize "failure" []
    , debianize "attoparsec" [P.DebVersion "0.10.1.1-1"]
    , debianize "attoparsec-enumerator" [P.DebVersion "0.3-3"]
    , P.Package { P.name = "haskell-attoparsec-text"
                , P.spec = Debianize (Patch
                                      (Hackage "attoparsec-text")
                                      (unlines
                                       [ "--- x/attoparsec-text.cabal.orig\t2012-01-01 12:43:48.746481982 -0800"
                                       , "+++ x/attoparsec-text.cabal\t2012-01-01 12:20:22.226482130 -0800"
                                       , "@@ -59,10 +59,10 @@"
                                       , " "
                                       , " library"
                                       , "   build-depends: base       >= 3       && < 5,"
                                       , "-                 attoparsec >= 0.7     && < 0.10,"
                                       , "+                 attoparsec >= 0.7,"
                                       , "                  text       >= 0.10    && < 0.12,"
                                       , "                  containers >= 0.1.0.1 && < 0.5,"
                                       , "-                 array      >= 0.1     && < 0.4"
                                       , "+                 array      >= 0.1     && < 0.5"
                                       , "   extensions:      CPP"
                                       , "   exposed-modules: Data.Attoparsec.Text"
                                       , "                    Data.Attoparsec.Text.FastSet" ]))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , debianize "attoparsec-text-enumerator" []
    , P.Package { P.name = "haskell-fb" 
                , P.spec = Debianize (Patch
                                      (Hackage "fb") 
                                      (unlines
                                       [ "diff -ru old/fb.cabal new/fb.cabal"
                                       , "--- old/fb.cabal\t2012-04-17 16:29:54.296561102 -0700"
                                       , "+++ new/fb.cabal\t2012-04-17 16:32:10.968553807 -0700"
                                       , "@@ -1,5 +1,5 @@"
                                       , " name:              fb"
                                       , "-version:           0.7.5"
                                       , "+version:           0.8"
                                       , " license:           BSD3"
                                       , " license-file:      LICENSE"
                                       , " author:            Felipe Lessa"
                                       , "@@ -52,14 +53,14 @@"
                                       , "     , lifted-base        >= 0.1     && < 0.2"
                                       , "     , bytestring         >= 0.9     && < 0.10"
                                       , "     , text               >= 0.11    && < 0.12"
                                       , "-    , transformers       >= 0.2     && < 0.3"
                                       , "+    , transformers       >= 0.2     && < 0.4"
                                       , "     , transformers-base"
                                       , "     , monad-control"
                                       , "-    , conduit            >= 0.2     && < 0.3"
                                       , "+    , conduit            >= 0.4     && < 0.5"
                                       , "     , http-types"
                                       , "-    , http-conduit       >= 1.2     && < 1.3"
                                       , "+    , http-conduit       >= 1.4     && < 1.5"
                                       , "     , attoparsec         >= 0.10    && < 0.11"
                                       , "-    , attoparsec-conduit >= 0.2     && < 0.3"
                                       , "+    , attoparsec-conduit >= 0.4     && < 0.5"
                                       , "     , aeson              >= 0.5     && < 0.7"
                                       , "     , time               >= 1.2     && < 1.5"
                                       , "     , old-locale"
                                       , "diff -ru old/src/Facebook/Auth.hs new/src/Facebook/Auth.hs"
                                       , "--- old/src/Facebook/Auth.hs\t2012-04-17 16:29:54.300561102 -0700"
                                       , "+++ new/src/Facebook/Auth.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE FlexibleContexts, GADTs, ScopedTypeVariables, OverloadedStrings #-}"
                                       , " module Facebook.Auth"
                                       , "     ( getAppAccessToken"
                                       , "     , getUserAccessTokenStep1"
                                       , "@@ -13,6 +14,7 @@"
                                       , " import Control.Applicative"
                                       , " import Control.Monad (join)"
                                       , " import Control.Monad.IO.Class (MonadIO(liftIO))"
                                       , "+import Control.Monad.Trans.Control (MonadBaseControl)"
                                       , " import Data.Aeson ((.:))"
                                       , " import Data.Aeson.Types (parseEither)"
                                       , " import Data.Maybe (fromMaybe)"
                                       , "@@ -41,7 +43,7 @@"
                                       , " "
                                       , " -- | Get an app access token from Facebook using your"
                                       , " -- credentials."
                                       , "-getAppAccessToken :: C.ResourceIO m =>"
                                       , "+getAppAccessToken :: (C.MonadResource m, MonadBaseControl IO m) =>"
                                       , "                      FacebookT Auth m AppAccessToken"
                                       , " getAppAccessToken ="
                                       , "   runResourceInFb $ do"
                                       , "@@ -86,7 +88,7 @@"
                                       , " -- request query parameters passed to your 'RedirectUrl' and give"
                                       , " -- to this function that will complete the user authentication"
                                       , " -- flow and give you an @'UserAccessToken'@."
                                       , "-getUserAccessTokenStep2 :: C.ResourceIO m =>"
                                       , "+getUserAccessTokenStep2 :: (MonadBaseControl IO m, C.MonadResource m) =>"
                                       , "                            RedirectUrl -- ^ Should be exactly the same"
                                       , "                                        -- as in 'getUserAccessTokenStep1'."
                                       , "                         -> [Argument]  -- ^ Query parameters."
                                       , "@@ -211,7 +213,7 @@"
                                       , " -- access token may not be valid as well.  For example, in the"
                                       , " -- case of an user access token, they may have changed their"
                                       , " -- password, logged out from Facebook or blocked your app."
                                       , "-isValid :: C.ResourceIO m =>"
                                       , "+isValid :: (MonadBaseControl IO m, C.MonadResource m) =>"
                                       , "            AccessToken anyKind"
                                       , "         -> FacebookT anyAuth m Bool"
                                       , " isValid token = do"
                                       , "@@ -241,10 +243,9 @@"
                                       , " -- the same data and expiration time as before, but you can't"
                                       , " -- assume this).  Note that expired access tokens can't be"
                                       , " -- extended, only valid tokens."
                                       , "-extendUserAccessToken ::"
                                       , "-    C.ResourceIO m =>"
                                       , "-    UserAccessToken"
                                       , "- -> FacebookT Auth m (Either FacebookException UserAccessToken)"
                                       , "+extendUserAccessToken :: (MonadBaseControl IO m, C.MonadResource m) =>"
                                       , "+                         UserAccessToken"
                                       , "+                      -> FacebookT Auth m (Either FacebookException UserAccessToken)"
                                       , " extendUserAccessToken token@(UserAccessToken _ data_ _)"
                                       , "     = do expired <- hasExpired token"
                                       , "          if expired then return (Left hasExpiredExc) else tryToExtend"
                                       , "diff -ru old/src/Facebook/Base.hs new/src/Facebook/Base.hs"
                                       , "--- old/src/Facebook/Base.hs\t2012-04-17 16:29:54.300561102 -0700"
                                       , "+++ new/src/Facebook/Base.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}"
                                       , " module Facebook.Base"
                                       , "     ( fbreq"
                                       , "     , ToSimpleQuery(..)"
                                       , "@@ -15,6 +16,7 @@"
                                       , " import Data.Typeable (Typeable)"
                                       , " "
                                       , " import qualified Control.Exception.Lifted as E"
                                       , "+import Control.Monad.Trans.Control (MonadBaseControl)"
                                       , " import qualified Data.Aeson as A"
                                       , " import qualified Data.Attoparsec.Char8 as AT"
                                       , " import qualified Data.ByteString as B"
                                       , "@@ -71,9 +73,9 @@"
                                       , " "
                                       , " -- | Converts a plain 'H.Response' coming from 'H.http' into a"
                                       , " -- JSON value."
                                       , "-asJson :: (C.ResourceThrow m, C.IsSource bsrc, A.FromJSON a) =>"
                                       , "-          H.Response (bsrc m ByteString)"
                                       , "-       -> FacebookT anyAuth (C.ResourceT m) a"
                                       , "+asJson :: (C.MonadThrow m, A.FromJSON a) =>"
                                       , "+          H.Response (C.Source m ByteString)"
                                       , "+       -> FacebookT anyAuth m a"
                                       , " asJson response = do"
                                       , "   val <- lift $ H.responseBody response C.$$ C.sinkParser A.json'"
                                       , "   case A.fromJSON val of"
                                       , "@@ -84,14 +86,14 @@"
                                       , "              , \" Facebook's response as a JSON value (\""
                                       , "              , T.pack str, \")\" ]"
                                       , " "
                                       , "+"
                                       , " -- | Converts a plain 'H.Response' into a string 'ByteString'."
                                       , "-asBS :: (C.ResourceThrow m, C.IsSource bsrc) =>"
                                       , "-        H.Response (bsrc m ByteString)"
                                       , "-     -> FacebookT anyAuth (C.ResourceT m) ByteString"
                                       , "+asBS :: (Monad m) =>"
                                       , "+        H.Response (C.Source m ByteString)"
                                       , "+     -> FacebookT anyAuth m ByteString"
                                       , " asBS response = lift $ H.responseBody response C.$$ fmap B.concat CL.consume"
                                       , " "
                                       , " "
                                       , "-"
                                       , " -- | An exception that may be thrown by functions on this"
                                       , " -- package.  Includes any information provided by Facebook."
                                       , " data FacebookException ="
                                       , "@@ -114,13 +116,13 @@"
                                       , " "
                                       , " -- | Same as 'H.http', but tries to parse errors and throw"
                                       , " -- meaningful 'FacebookException'@s@."
                                       , "-fbhttp :: C.ResourceIO m =>"
                                       , "+fbhttp :: (MonadBaseControl IO m, C.MonadResource m) =>"
                                       , "           H.Request m"
                                       , "-       -> FacebookT anyAuth (C.ResourceT m) (H.Response (C.Source m ByteString))"
                                       , "+       -> FacebookT anyAuth m (H.Response (C.Source m ByteString))"
                                       , " fbhttp req = do"
                                       , "   manager <- getManager"
                                       , "   let req' = req { H.checkStatus = \\_ _ -> Nothing }"
                                       , "-  response@(H.Response status headers _) <- lift (H.http req' manager)"
                                       , "+  response@(H.Response status _ headers _) <- lift (H.http req' manager)"
                                       , "   if isOkay status"
                                       , "     then return response"
                                       , "     else do"
                                       , "@@ -150,14 +152,15 @@"
                                       , " "
                                       , " -- | Send a @HEAD@ request just to see if the resposne status"
                                       , " -- code is 2XX (returns @True@) or not (returns @False@)."
                                       , "-httpCheck :: C.ResourceIO m =>"
                                       , "-             H.Request m"
                                       , "+httpCheck :: (MonadBaseControl IO m, C.MonadResource m) =>"
                                       , "+             H.Request (C.ResourceT m)"
                                       , "           -> FacebookT anyAuth m Bool"
                                       , "+"
                                       , " httpCheck req = runResourceInFb $ do"
                                       , "   manager <- getManager"
                                       , "   let req' = req { H.method      = HT.methodHead"
                                       , "                  , H.checkStatus = \\_ _ -> Nothing }"
                                       , "-  H.Response status _ _ <- lift (H.httpLbs req' manager)"
                                       , "+  H.Response status _ _ _ <- lift (H.httpLbs req' manager)"
                                       , "   return $! isOkay status"
                                       , "   -- Yes, we use httpLbs above so that we don't have to worry"
                                       , "   -- about consuming the responseBody.  Note that the"
                                       , "diff -ru old/src/Facebook/Graph.hs new/src/Facebook/Graph.hs"
                                       , "--- old/src/Facebook/Graph.hs\t2012-04-17 16:29:54.300561102 -0700"
                                       , "+++ new/src/Facebook/Graph.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}"
                                       , " module Facebook.Graph"
                                       , "     ( getObject"
                                       , "     , postObject"
                                       , "@@ -6,6 +7,7 @@"
                                       , " "
                                       , " "
                                       , " import Control.Applicative"
                                       , "+import Control.Monad.Trans.Control (MonadBaseControl)"
                                       , " -- import Control.Monad (mzero)"
                                       , " -- import Data.ByteString.Char8 (ByteString)"
                                       , " -- import Data.Text (Text)"
                                       , "@@ -27,7 +29,7 @@"
                                       , " "
                                       , " -- | Make a raw @GET@ request to Facebook's Graph API.  Returns a"
                                       , " -- raw JSON 'A.Value'."
                                       , "-getObject :: (C.ResourceIO m, A.FromJSON a) =>"
                                       , "+getObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>"
                                       , "              Ascii          -- ^ Path (should begin with a slash @\\/@)"
                                       , "           -> [Argument]     -- ^ Arguments to be passed to Facebook"
                                       , "           -> Maybe (AccessToken anyKind) -- ^ Optional access token"
                                       , "@@ -39,7 +41,7 @@"
                                       , " "
                                       , " -- | Make a raw @POST@ request to Facebook's Graph API.  Returns"
                                       , " -- a raw JSON 'A.Value'."
                                       , "-postObject :: (C.ResourceIO m, A.FromJSON a) =>"
                                       , "+postObject :: (C.MonadResource m, MonadBaseControl IO m, A.FromJSON a) =>"
                                       , "               Ascii               -- ^ Path (should begin with a slash @\\/@)"
                                       , "            -> [Argument]          -- ^ Arguments to be passed to Facebook"
                                       , "            -> AccessToken anyKind -- ^ Access token"
                                       , "diff -ru old/src/Facebook/Monad.hs new/src/Facebook/Monad.hs"
                                       , "--- old/src/Facebook/Monad.hs\t2012-04-17 16:29:54.296561102 -0700"
                                       , "+++ new/src/Facebook/Monad.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,4 +1,4 @@"
                                       , "-{-# LANGUAGE UndecidableInstances #-}"
                                       , "+{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}"
                                       , " module Facebook.Monad"
                                       , "     ( FacebookT"
                                       , "     , Auth"
                                       , "@@ -127,7 +127,7 @@"
                                       , " "
                                       , " "
                                       , " -- | Run a 'ResourceT' inside a 'FacebookT'."
                                       , "-runResourceInFb :: C.Resource m =>"
                                       , "+runResourceInFb :: (C.MonadResource m, MonadBaseControl IO m) =>"
                                       , "                    FacebookT anyAuth (C.ResourceT m) a"
                                       , "                 -> FacebookT anyAuth m a"
                                       , " runResourceInFb (F inner) = F $ ask >>= lift . C.runResourceT . runReaderT inner"
                                       , "diff -ru old/src/Facebook/Object/User.hs new/src/Facebook/Object/User.hs"
                                       , "--- old/src/Facebook/Object/User.hs\t2012-04-17 16:29:54.300561102 -0700"
                                       , "+++ new/src/Facebook/Object/User.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings #-}"
                                       , " module Facebook.Object.User"
                                       , "     ( User(..)"
                                       , "     , Gender(..)"
                                       , "@@ -6,6 +7,7 @@"
                                       , " "
                                       , " import Control.Applicative"
                                       , " import Control.Monad (mzero)"
                                       , "+import Control.Monad.Trans.Control (MonadBaseControl)"
                                       , " import Data.Aeson ((.:), (.:?))"
                                       , " -- import Data.ByteString.Char8 (ByteString)"
                                       , " import Data.Text (Text)"
                                       , "@@ -78,7 +80,7 @@"
                                       , " -- back by Facebook.  The user ID may be @\\\"me\\\"@, in which"
                                       , " -- case you must provide an user access token and information"
                                       , " -- about the token's owner is given."
                                       , "-getUser :: C.ResourceIO m =>"
                                       , "+getUser :: (C.MonadResource m, MonadBaseControl IO m) =>"
                                       , "            UserId         -- ^ User ID or @\\\"me\\\"@."
                                       , "         -> [Argument]     -- ^ Arguments to be passed to Facebook."
                                       , "         -> Maybe UserAccessToken -- ^ Optional user access token."
                                       , "diff -ru old/src/Facebook/OpenGraph.hs new/src/Facebook/OpenGraph.hs"
                                       , "--- old/src/Facebook/OpenGraph.hs\t2012-04-17 16:29:54.300561102 -0700"
                                       , "+++ new/src/Facebook/OpenGraph.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}"
                                       , " module Facebook.OpenGraph"
                                       , "     ( createAction"
                                       , "     , Action(..)"
                                       , "@@ -7,6 +8,7 @@"
                                       , " "
                                       , " -- import Control.Applicative"
                                       , " import Control.Arrow (first)"
                                       , "+import Control.Monad.Trans.Control (MonadBaseControl)"
                                       , " -- import Control.Monad (mzero)"
                                       , " -- import Data.ByteString.Char8 (ByteString)"
                                       , " import Data.Function (on)"
                                       , "@@ -43,7 +45,7 @@"
                                       , " -- >              [ \"recipe\" #= \"http://example.com/cookie.html\""
                                       , " -- >              , \"when\"   #= now ]"
                                       , " -- >              token"
                                       , "-createAction :: C.ResourceIO m =>"
                                       , "+createAction :: (C.MonadResource m, MonadBaseControl IO m)  =>"
                                       , "                 Action     -- ^ Action kind to be created."
                                       , "              -> [Argument] -- ^ Arguments of the action."
                                       , "              -> Maybe AppAccessToken"
                                       , "@@ -55,7 +57,7 @@"
                                       , "              -> FacebookT Auth m Id"
                                       , " createAction (Action action) query mapptoken usertoken = do"
                                       , "   creds <- getCreds"
                                       , "-  let post :: C.ResourceIO m => Ascii -> AccessToken anyKind -> FacebookT Auth m Id"
                                       , "+  let post :: (C.MonadResource m, MonadBaseControl IO m)  => Ascii -> AccessToken anyKind -> FacebookT Auth m Id"
                                       , "       post prepath = postObject (prepath <> appName creds <> \":\" <> action) query"
                                       , "   case mapptoken of"
                                       , "     Nothing       -> post \"/me/\" usertoken"
                                       , "diff -ru old/src/Facebook/Types.hs new/src/Facebook/Types.hs"
                                       , "--- old/src/Facebook/Types.hs\t2012-04-17 16:29:54.296561102 -0700"
                                       , "+++ new/src/Facebook/Types.hs\t2012-04-17 16:01:42.000000000 -0700"
                                       , "@@ -1,3 +1,4 @@"
                                       , "+{-# LANGUAGE DeriveDataTypeable, GADTs, StandaloneDeriving #-}"
                                       , " module Facebook.Types"
                                       , "     ( Credentials(..)"
                                       , "     , AccessToken(..)" ]))
                , P.flags = [] }
    , debianize "base-unicode-symbols" [P.DebVersion "0.2.2.3-1build1"]
    , apt release "haskell-base64-bytestring"
    , debianize "bimap" [P.DebVersion "0.2.4-1~hackage1"]
    , debianize "bitmap" []
    , debianize "bitset" [P.DebVersion "1.1-1~hackage1"]
    , apt release "haskell-blaze-builder"
    , P.Package { P.name = "haskell-blaze-builder-enumerator" 
                , P.spec = Debianize (Hackage "blaze-builder-enumerator")
                , P.flags = [] }
    , debianize "blaze-from-html" []
    , debianize "blaze-html" []
    , debianize "blaze-textual" [P.DebVersion "0.2.0.6-2"]
    , P.Package { P.name = "haskell-blaze-textual-native"
                , P.spec = Debianize (Patch
                                      (Hackage "blaze-textual-native")
                                      (unlines
                                       [ "--- x/blaze-textual-native.cabal.orig\t2012-01-01 12:22:11.676481147 -0800"
                                       , "+++ x/blaze-textual-native.cabal\t2012-01-01 12:22:21.716482151 -0800"
                                       , "@@ -66,7 +66,7 @@"
                                       , " "
                                       , "   if impl(ghc >= 6.11)"
                                       , "     cpp-options: -DINTEGER_GMP"
                                       , "-    build-depends: integer-gmp >= 0.2 && < 0.4"
                                       , "+    build-depends: integer-gmp >= 0.2"
                                       , " "
                                       , "   if impl(ghc >= 6.9) && impl(ghc < 6.11)"
                                       , "     cpp-options: -DINTEGER_GMP"
                                       , "--- x/Blaze/Text/Int.hs.orig\t2012-01-01 12:45:05.136482154 -0800"
                                       , "+++ x/Blaze/Text/Int.hs\t2012-01-01 12:45:26.016482025 -0800"
                                       , "@@ -40,7 +40,7 @@"
                                       , " # define PAIR(a,b) (a,b)"
                                       , " #endif"
                                       , " "
                                       , "-integral :: Integral a => a -> Builder"
                                       , "+integral :: (Integral a, Show a) => a -> Builder"
                                       , " {-# RULES \"integral/Int\" integral = bounded :: Int -> Builder #-}"
                                       , " {-# RULES \"integral/Int8\" integral = bounded :: Int8 -> Builder #-}"
                                       , " {-# RULES \"integral/Int16\" integral = bounded :: Int16 -> Builder #-}" ]))
                , flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>", P.Revision ""] }
    , apt release "haskell-bytestring-nums"
    , debianize "bytestring-trie" []
    , P.Package { P.name = "haskell-bzlib"
                , P.spec = Quilt (Apt "sid" "haskell-bzlib") (Darcs "http://src.seereason.com/haskell-bzlib-quilt")
                , P.flags = [] }
    -- , debianize "cairo-pdf" []
    , debianize "case-insensitive" [P.DebVersion "0.4.0.1-2"]
    , debianize "CC-delcont" [P.DebVersion "0.2-1~hackage1"]
    , apt release "haskell-cereal"
    , debianize "citeproc-hs" [P.DebVersion "0.3.4-1"]
    , clckwrks _home
    , debianize "uuid" []
    , debianize "maccatcher" [P.DebVersion "2.1.5-3"]
    , case release of
        "natty-seereason" -> debianize "colour" []
        "precise-seereason" -> debianize "colour" [P.DebVersion "2.3.3-1build1"]
        _ -> apt release "haskell-colour"
    -- , apt release "haskell-configfile"
    , debianize "ConfigFile" []
    , P.Package { P.name = "haskell-consumer"
                , P.spec = Darcs "http://src.seereason.com/haskell-consumer"
                , P.flags = [] }
    , debianize "cprng-aes" [P.DebVersion "0.2.3-3"]
    , patched "Crypto"
                    [ P.DebVersion "4.2.4-1"]
                    (unlines
                      [ "--- old/Data/Digest/SHA2.hs\t2012-01-03 23:14:43.000000000 -0800"
                      , "+++ new/Data/Digest/SHA2.hs\t2012-01-03 23:23:31.786481686 -0800"
                      , "@@ -106,7 +106,7 @@"
                      , " data Hash384 = Hash384 !Word64 !Word64 !Word64 !Word64 !Word64 !Word64 deriving (Eq, Ord)"
                      , " data Hash224 = Hash224 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 !Word32 deriving (Eq, Ord)"
                      , " "
                      , "-instance (Integral a) => Show (Hash8 a) where"
                      , "+instance (Integral a, Show a) => Show (Hash8 a) where"
                      , "  showsPrec _ (Hash8 a b c d e f g h) ="
                      , "   (showHex a) . (' ':) ."
                      , "   (showHex b) . (' ':) ."
                      , "@@ -146,7 +146,7 @@"
                      , "      where"
                      , "       bs = bitSize (head r)"
                      , " "
                      , "-instance (Integral h, Bits h) => Hash (Hash8 h) where"
                      , "+instance (Integral h, Bits h, Show h) => Hash (Hash8 h) where"
                      , "   toOctets (Hash8 x0 x1 x2 x3 x4 x5 x6 x7) = bitsToOctets =<< [x0, x1, x2, x3, x4, x5, x6, x7]"
                      , " "
                      , " instance Hash Hash384 where" ])
    , debianize "crypto-api" [P.DebVersion "0.10.1-1~hackage1"]
    , debianize "crypto-pubkey-types" []
    , debianize "cryptocipher" [P.DebVersion "0.3.0-2"]
    , debianize "cryptohash" [P.DebVersion "0.7.4-1"]
    , debianize "css" [P.DebVersion "0.1-1~hackage1"]
    , debianize "css-text" [P.DebVersion "0.1.1-3"]
    , apt release "haskell-curl"
    , debianize "data-accessor" []
    , debianize "data-accessor-template" [P.DebVersion "0.2.1.9-1"]
    , debianize "data-default" []
    , patched "data-object" []
                    (unlines
                      [ "--- old/data-object.cabal\t2012-01-20 06:42:12.000000000 -0800"
                      , "+++ new/data-object.cabal\t2012-01-20 10:13:25.147160370 -0800"
                      , "@@ -20,6 +20,6 @@"
                      , "                      bytestring >= 0.9.1.4,"
                      , "                      text >= 0.5,"
                      , "                      time >= 1.1.4,"
                      , "-                     failure >= 0.1.0 && < 0.2"
                      , "+                     failure >= 0.1.0"
                      , "     exposed-modules: Data.Object"
                      , "     ghc-options:     -Wall" ])
    , debianize "dataenc" [P.DebVersion "0.14.0.3-1"]
    , debianize "Diff" [P.DebVersion "0.1.3-2"]
    , apt release "haskell-digest"
    , apt release "haskell-dlist"
    -- Natty only(?)
    , debianize "double-conversion" []
    , apt release "haskell-dummy"
    -- Need this when we upgrade blaze-textual to 0.2.0.0
    -- , lucidNatty (hackage release "double-conversion" []) (debianize "double-conversion" [])
    , P.Package { P.name = "haskell-edison-api"
                , P.spec = Apt "sid" "haskell-edison-api"
                , P.flags = [] }
    , apt release "haskell-edison-core"
    , apt release "haskell-entropy"
    , debianize "enumerator" []
    , debianize "erf" [P.DebVersion "2.0.0.0-3"]
    , apt release "haskell-feed"
    , debianize "file-embed" []
    , P.Package { P.name = "haskell-formlets"
                , P.spec = Debianize (Patch
                                      (Hackage "formlets")
                                      (unlines
                                          [ "--- old/formlets.cabal\t2012-04-12 20:06:08.000000000 -0700"
                                          , "+++ new/formlets.cabal\t2012-04-12 21:29:51.670096327 -0700"
                                          , "@@ -21,12 +21,11 @@"
                                          , "     Description: Choose the even newer, even smaller, split-up base package."
                                          , " "
                                          , " Library"
                                          , "-  Build-Depends:   haskell98, "
                                          , "-                   xhtml, "
                                          , "+  Build-Depends:   xhtml, "
                                          , "                    applicative-extras >= 0.1.7, "
                                          , "                    bytestring,"
                                          , "                    blaze-html >= 0.2,"
                                          , "-                   transformers == 0.2.2.0"
                                          , "+                   transformers >= 0.2.2.0"
                                          , "   if flag(base4)"
                                          , "     Build-Depends: base >= 4 && < 5, syb"
                                          , "   else" ]))
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , P.Package { P.name = "haskell-formlets-hsp"
                , P.spec = Darcs (repo ++ "/formlets-hsp")
                , P.flags = [] }
    , patched "gd"  [ P.ExtraDevDep "libgd-dev" ]
                    (unlines
                      [ "--- gd/gd.cabal.orig\t2011-06-25 12:27:26.000000000 -0700"
                      , "+++ gd/gd.cabal\t2011-09-10 14:29:48.514415016 -0700"
                      , "@@ -21,7 +21,7 @@"
                      , "   Extensions: ForeignFunctionInterface"
                      , "   Exposed-Modules: Graphics.GD, Graphics.GD.ByteString, Graphics.GD.ByteString.Lazy"
                      , "   Ghc-options: -Wall"
                      , "-  Extra-libraries: gd, png, z, jpeg, m, fontconfig, freetype, expat"
                      , "+  Extra-libraries: gd, png, z, jpeg, fontconfig, expat"
                      , "   Includes: gd.h"
                      , "   Include-dirs:        cbits"
                      , "   Install-includes: gd-extras.h" ])
    -- , debianize "gd" [P.ExtraDep "libm-dev", P.ExtraDep "libfreetype-dev"]
    , debianize "cabal-macosx" []
    , apt release "haskell-ghc-paths" -- for leksah
    -- Unpacking haskell-gtk2hs-buildtools-utils (from .../haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb) ...
    -- dpkg: error processing /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb (--unpack):
    --  trying to overwrite '/usr/bin/gtk2hsTypeGen', which is also in package gtk2hs-buildtools 0:0.12.0-3+seereason1~lucid3
    -- dpkg-deb: subprocess paste killed by signal (Broken pipe)
    -- Errors were encountered while processing:
    --  /work/localpool/haskell-gtk2hs-buildtools-utils_0.12.1-0+seereason1~lucid2_amd64.deb
    -- E: Sub-process /usr/bin/dpkg returned an error code (1)
    , apt release "haskell-harp"
    , debianize "hashable" [P.DebVersion "1.1.2.3-1"]
    , debianize "hashed-storage" [P.DebVersion "0.5.9-2build1"]
    , P.Package { P.name = "haskell-haskeline"
                , P.spec = Debianize (Patch
                                      (Hackage "haskeline") 
                                      (unlines
                                       [ "--- old/haskeline.cabal\t2012-04-17 05:38:27.000000000 -0700"	
                                       , "+++ new/haskeline.cabal\t2012-04-17 05:58:03.514585245 -0700"	
                                       , "@@ -58,7 +58,7 @@"		       
                                       , "                            bytestring==0.9.*"	
                                       , "         }"		       
                                       , "     }"  
                                       , "-    Build-depends:  filepath >= 1.1 && < 1.4, mtl >= 1.1 && < 2.1,"	
                                       , "+    Build-depends:  filepath >= 1.1 && < 1.4, mtl >= 1.1,"	
                                       , "                     utf8-string==0.3.* && >=0.3.6,"	 
                                       , "                     extensible-exceptions==0.1.* && >=0.1.1.0"	
                                       , "     Extensions:     ForeignFunctionInterface, Rank2Types, FlexibleInstances," ]))
                , P.flags = [P.DebVersion "0.6.4.6-1"] }
    , debianize "haskell-src-meta" []
    -- Because we specify an exact debian version here, this package
    -- needs to be forced to rebuilt when its build dependencies (such
    -- as ghc) change.  Autobuilder bug I suppose.  Wait, this doesn't
    -- sound right...
    , debianize "HaXml" [P.DebVersion "1:1.22.5-2"]
    , debianize "heap" [P.DebVersion "1.0.0-1~hackage1"]
    , P.Package { P.name = "haskell-heist"
                , P.spec = Debianize (Patch 
                                      (Hackage "heist") 
                                      (unlines
                                       [ "--- old/heist.cabal\t2012-04-17 05:18:05.000000000 -0700"	
                                       , "+++ new/heist.cabal\t2012-04-17 05:35:49.742656458 -0700"	
                                       , "@@ -94,8 +94,8 @@"		   
                                       , "     directory,"		   
                                       , "     directory-tree,"	   
                                       , "     filepath,"		   
                                       , "-    MonadCatchIO-transformers >= 0.2.1 && < 0.3,"	
                                       , "-    mtl                       >= 2.0   && < 2.1,"	
                                       , "+    MonadCatchIO-transformers >= 0.2.1,"  
                                       , "+    mtl                       >= 2.0,"    
                                       , "     process,"		  
                                       , "     random,"		  
                                       , "     text                      >= 0.10  && < 0.12," ]))
                , P.flags = [] }
    , debianize "xmlhtml" []
    , debianize "directory-tree" [P.DebVersion "0.10.0-2"]
    , debianize "MonadCatchIO-transformers" []
    , debianize "hinotify" [P.DebVersion "0.3.2-1"]
    , P.Package { P.name = "haskell-hjavascript"
                , P.spec = Quilt (Apt "sid" "haskell-hjavascript") (Darcs (repo ++ "/hjavascript-quilt"))
                , P.flags = [] }
    , patched "HJScript"
                    [ P.DebVersion "0.5.0-3", P.CabalPin "0.5.0" ]
                    (unlines
                      [ "--- old-HJScript-0.5.0/src/HJScript/XMLGenerator.hs\t2012-01-01 14:25:05.000000000 -0800"
                      , "+++ new-HJScript-0.5.0/src/HJScript/XMLGenerator.hs\t2012-01-02 09:18:41.226482198 -0800"
                      , "@@ -6,8 +6,7 @@"
                      , "         ) where"
                      , " "
                      , " import qualified HSX.XMLGenerator as HSX (XMLGen(..))"
                      , "-import HSX.XMLGenerator hiding (XMLGen(..))"
                      , "-import HSX.XMLGenerator (genElement, genEElement)"
                      , "+import HSX.XMLGenerator"
                      , " "
                      , " import HJScript.Monad"
                      , " import HJScript.Lang"
                      , "@@ -18,14 +18,14 @@"
                      , " import HJScript.DOM.TextNode"
                      , " import HJScript.DOM.Document"
                      , " "
                      , "-type XML   = Exp ElementNode"
                      , "-type Child = Exp Node"
                      , "-type Attribute = Exp AttributeNode"
                      , "+type XML'   = Exp ElementNode"
                      , "+type Child' = Exp Node"
                      , "+type Attribute' = Exp AttributeNode"
                      , " "
                      , " instance HSX.XMLGen HJScript' where"
                      , "- type HSX.XML          HJScript' = XML"
                      , "- newtype HSX.Child     HJScript' = HJSChild Child"
                      , "- newtype HSX.Attribute HJScript' = HJSAttr Attribute"
                      , "+ type XML          HJScript' = XML'"
                      , "+ newtype Child     HJScript' = HJSChild Child'"
                      , "+ newtype Attribute HJScript' = HJSAttr Attribute'"
                      , "  genElement = element"
                      , "  genEElement = eElement"
                      , "  xmlToChild = HJSChild . castToNode"
                      , "@@ -33,7 +33,7 @@"
                      , " "
                      , " element :: (EmbedAsChild HJScript' c, "
                      , "             EmbedAsAttr HJScript' a) "
                      , "-            => Name -> [a] -> [c] -> HJScript XML"
                      , "+            => Name -> [a] -> [c] -> HJScript XML'"
                      , " element (ns, ln) atts xmls = do"
                      , "   let name = (maybe id (\\x y -> y ++ ':':x) ns) ln"
                      , "   elem <- fmap val $ varWith $ document # createElement (string name)"
                      , "@@ -43,8 +43,8 @@"
                      , "   mapM (\\child -> elem # appendChild child) $ map stripChild cxml"
                      , "   return elem"
                      , " "
                      , "-eElement :: EmbedAsAttr HJScript' a => Name -> [a] -> HJScript XML"
                      , "-eElement n attrs = element n attrs ([] :: [Child])"
                      , "+eElement :: EmbedAsAttr HJScript' a => Name -> [a] -> HJScript XML'"
                      , "+eElement n attrs = element n attrs ([] :: [Child'])"
                      , " "
                      , " "
                      , " instance XMLGenerator HJScript'"
                      , "@@ -52,7 +52,7 @@"
                      , " --------------------------------------------"
                      , " -- EmbedAsChild and EmbedAsAttr"
                      , " "
                      , "-instance EmbedAsChild HJScript' Child where"
                      , "+instance EmbedAsChild HJScript' Child' where"
                      , "  asChild = asChild . HJSChild"
                      , " "
                      , " instance EmbedAsChild HJScript' JString where"
                      , "@@ -66,10 +66,10 @@"
                      , " "
                      , " -- This instance should already be there, probably doesn't work due"
                      , " -- to type families not being fully supported yet."
                      , "-instance EmbedAsChild HJScript' XML where"
                      , "+instance EmbedAsChild HJScript' XML' where"
                      , "  asChild = return . return . HSX.xmlToChild"
                      , " "
                      , "-instance EmbedAsAttr HJScript' Attribute where"
                      , "+instance EmbedAsAttr HJScript' Attribute' where"
                      , "  asAttr = asAttr . HJSAttr"
                      , " "
                      , " instance (IsName n, IsAttrNodeValue a) => EmbedAsAttr HJScript' (Attr n a) where"
                      , "@@ -94,14 +94,14 @@"
                      , " -----------------------------------"
                      , " -- SetAttr and AppendChild."
                      , " "
                      , "-instance SetAttr HJScript' XML where"
                      , "+instance SetAttr HJScript' XML' where"
                      , "  setAll en ats = do"
                      , "         ev <- inVar en"
                      , "         as <- ats"
                      , "         mapM (\\attr -> ev # setAttributeNode attr) (map stripAttr as)"
                      , "         return ev"
                      , " "
                      , "-instance AppendChild HJScript' XML where"
                      , "+instance AppendChild HJScript' XML' where"
                      , "  appAll en cns = do"
                      , "         ev <- inVar en"
                      , "         cs <- cns" ])
    , debianize "hoauth" []
    , debianize "hostname" [P.DebVersion "1.0-4build1"]
    -- The Sid package has no profiling libraries, so dependent packages
    -- won't build.  Use our debianization instead.  This means keeping
    -- up with sid's version.
    , debianize "HPDF" []
    , debianize "hs-bibutils" [P.DebVersion "4.12-5"]
    , apt release "haskell-hsemail"
    , patched "HsOpenSSL"
                    [ P.ExtraDevDep "libssl-dev"
                    , P.ExtraDevDep "libcrypto++-dev" ]
                    (unlines
                      [ "--- old/HsOpenSSL.cabal\t2011-09-10 15:02:20.000000000 -0700"
                      , "+++ new/HsOpenSSL.cabal\t2011-09-10 15:24:16.735325250 -0700"
                      , "@@ -50,7 +50,7 @@"
                      , "       CC-Options:         -D MINGW32"
                      , "       CPP-Options:        -DCALLCONV=stdcall"
                      , "   else"
                      , "-      Extra-Libraries: crypto ssl"
                      , "+      Extra-Libraries: crypto++ ssl"
                      , "       C-Sources:          cbits/mutex-pthread.c"
                      , "       CC-Options:         -D PTHREAD"
                      , "       CPP-Options:        -DCALLCONV=ccall"
                      , "@@ -108,6 +108,6 @@"
                      , "   C-Sources:"
                      , "           cbits/HsOpenSSL.c"
                      , "   Include-Dirs:"
                      , "-          cbits"
                      , "+          cbits, dist/build/autogen, dist-ghc/build/autogen"
                      , "   Install-Includes:"
                      , "           HsOpenSSL.h"
                      , "--- old/cbits/HsOpenSSL.h\t2012-04-06 18:54:18.000000000 -0700"
                      , "+++ new/cbits/HsOpenSSL.h\t2012-04-06 20:37:47.827713118 -0700"
                      , "@@ -27,7 +27,7 @@"
                      , "  * hsc2hs so we can reach the cabal_macros.h from cbits."
                      , "  */"
                      , " #if !defined(MIN_VERSION_base)"
                      , "-#  include \"../dist/build/autogen/cabal_macros.h\""
                      , "+#  include <cabal_macros.h>"
                      , " #endif"
                      , " "
                      , " /* OpenSSL ********************************************************************/" ])
    , patched "hsp"
                    [ P.ExtraDep "haskell-hsx-utils"
                    , P.DebVersion "0.6.1-2", P.CabalPin"0.6.1" ]
                    (unlines
                      [ "--- old/src/HSP/XMLGenerator.hs\t2012-01-04 09:58:57.966483200 -0800"
                      , "+++ new/src/HSP/XMLGenerator.hs\t2012-01-04 10:00:35.316482037 -0800"
                      , "@@ -31,9 +31,15 @@"
                      , " \r"
                      , " -- | We can use literal XML syntax to generate values of type XML in the HSP monad.\r"
                      , " instance Monad m => HSX.XMLGen (HSPT' m) where\r"
                      , "+#if __GLASGOW_HASKELL__ < 702\r"
                      , "  type HSX.XML (HSPT' m) = XML\r"
                      , "  newtype HSX.Attribute (HSPT' m) = HSPAttr Attribute \r"
                      , "  newtype HSX.Child     (HSPT' m) = HSPChild XML\r"
                      , "+#else\r"
                      , "+ type XML (HSPT' m) = XML\r"
                      , "+ newtype Attribute (HSPT' m) = HSPAttr Attribute \r"
                      , "+ newtype Child     (HSPT' m) = HSPChild XML\r"
                      , "+#endif\r"
                      , "  xmlToChild = HSPChild\r"
                      , "  pcdataToChild = HSX.xmlToChild . pcdata\r"
                      , "  genElement = element\r" ])
    , debianize "HsSyck" [P.DebVersion "0.50-2"]
    , debianize "HStringTemplate" []
    -- This target puts the trhsx binary in its own package, while the
    -- sid version puts it in libghc-hsx-dev.  This makes it inconvenient to
    -- use debianize for natty and apt:sid for lucid.
    , debianize "hsx" [P.DebVersion "0.9.1-2", P.CabalPin "0.9.1"]
    , P.Package { P.name = "haskell-html-entities"
                , P.spec = Darcs "http://src.seereason.com/html-entities"
                , P.flags = [] }
    , debianize "http-types" []
    , debianize "i18n" [P.DebVersion "0.3-1~hackage1"]
    , debianize "iconv" [P.DebVersion "0.4.1.0-2"]
    , P.Package { P.name = "haskell-incremental-sat-solver"
                , P.spec = DebDir (Hackage "incremental-sat-solver") (Darcs "http://src.seereason.com/haskell-incremental-sat-solver-debian")
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    , debianize "instant-generics" []
    , debianize "irc" [P.DebVersion "0.5.0.0-1"]
    , debianize "ixset" [P.DebVersion "1.0.2-1~hackage1"]
    , P.Package { P.name = "haskell-json"
                , P.spec = Darcs (repo ++ "/haskell-json")
                , P.flags = [] }
    , debianize "language-css" [P.DebVersion "0.0.4.1-1~hackage1"]
    , debianize "largeword" [P.DebVersion "1.0.1-3"]
{-  , apt release "haskell-leksah"
    , apt release "haskell-leksah-server" -- for leksah -}
    , P.Package { P.name = "haskell-logic-classes"
                , P.spec = Darcs (repo ++ "/haskell-logic")
                , P.flags = [] }
    , patched "logic-TPTP" [ P.ExtraDep "alex"
                           , P.ExtraDep "happy"
                           , P.DebVersion "0.3.0.1-1~hackage1" ]
                           (unlines
                               [ "--- old/logic-TPTP.cabal\t2011-09-15 16:31:03.000000000 -0700"
                               , "+++ new/logic-TPTP.cabal\t2011-09-16 13:40:26.458725487 -0700"
                               , "@@ -51,7 +51,7 @@"
                               , "  "
                               , " "
                               , " Library"
                               , "- ghc-options: -Wall -O2"
                               , "+ ghc-options: -Wall -O2 -XBangPatterns"
                               , "  "
                               , "  build-depends:      base >=4 && < 5"
                               , "                    , array"
                               , "--- old/Parser.y\t2012-01-20 11:54:31.000000000 -0800"
                               , "+++ new/Parser.y\t2012-01-20 13:07:07.427160933 -0800"
                               , "@@ -9,7 +9,7 @@"
                               , " import Control.Monad"
                               , " import Data.List as L"
                               , " import Lexer"
                               , "-import Data.Set as S"
                               , "+import qualified Data.Set as S"
                               , " import Codec.TPTP.Base"
                               , " import System.IO"
                               , " import System.IO.Unsafe"
                               , "--- old/ParserC.y\t2012-01-20 13:28:19.000000000 -0800"
                               , "+++ new/ParserC.y\t2012-01-20 13:35:49.917165510 -0800"
                               , "@@ -7,7 +7,7 @@"
                               , " import Control.Monad"
                               , " import Data.List as L"
                               , " import Lexer"
                               , "-import Data.Set as S"
                               , "+import qualified Data.Set as S"
                               , " import Codec.TPTP.Base"
                               , " import System.IO"
                               , " import System.IO.Unsafe" ])
    , apt release "haskell-maybet"
    , P.Package { P.name = "haskell-mime"
                , P.spec = Darcs "http://src.seereason.com/haskell-mime"
                , P.flags = [] }
    , apt release "haskell-mmap"
    -- "0.2.0.3" is the version to get from hackage,
    -- "0.2.0.3-1~hackage1" is the version to base our debian version
    -- on.  Both flags should be removed when we move to 0.3, but we
    -- need base 4.4 for that.
    , debianize "monad-control" []
    , debianize "monad-par" [P.DebVersion "0.1.0.3-2"]
    , apt release "haskell-monadcatchio-mtl"
    , debianize "monadLib" [P.DebVersion "3.6.2-1~hackage1"]
    -- , debianize "monads-tf" [P.DebVersion "0.1.0.0-1~hackage1"]
    , apt release "haskell-monoid-transformer"
    , debianize "murmur-hash" [P.DebVersion "0.1.0.5-2"]
    , apt release "haskell-mwc-random"
    , patched "nano-hmac" [ P.DebVersion "0.2.0ubuntu1" ]
                            (unlines
                             [ "--- nano-hmac/nano-hmac.cabal.orig\t2011-08-14 09:25:43.000000000 -0700"
                             , "+++ nano-hmac/nano-hmac.cabal\t2011-09-10 14:24:25.234226579 -0700"
                             , "@@ -20,8 +20,8 @@"
                             , "   else"
                             , "     build-depends:     base < 3"
                             , "   exposed-modules:     Data.Digest.OpenSSL.HMAC"
                             , "-  ghc-options:         -Wall -Werror -O2 -fvia-C"
                             , "+  ghc-options:         -Wall -Werror -O2"
                             , "   extensions:          ForeignFunctionInterface, BangPatterns, CPP"
                             , "   includes:            openssl/hmac.h"
                             , "-  extra-libraries:     crypto ssl"
                             , "+  extra-libraries:     crypto++ ssl"
                             , " "
                             , "--- old/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 16:39:39.603631778 -0700"
                             , "+++ new/Data/Digest/OpenSSL/HMAC.hsc\t2011-09-16 13:57:55.000000000 -0700"
                             , "@@ -35,8 +35,11 @@"
                             , " "
                             , " import qualified Data.ByteString as B"
                             , " import qualified Data.ByteString.Unsafe as BU"
                             , "-import Foreign"
                             , "+import System.IO.Unsafe"
                             , " import Foreign.C.Types"
                             , "+import Foreign.Ptr"
                             , "+import Foreign.Storable"
                             , "+import Data.Word"
                             , " import Numeric (showHex)"
                             , " "
                             , " #include \"openssl/hmac.h\""
                             , "--- old/Data/Digest/OpenSSL/HMAC.hsc\t2012-01-20 06:44:45.000000000 -0800"
                             , "+++ new/Data/Digest/OpenSSL/HMAC.hsc\t2012-01-20 10:35:41.337161457 -0800"
                             , "@@ -103,13 +103,13 @@"
                             , "        what else was I going to do?"
                             , "     -}"
                             , "     where"
                             , "-      go :: (Storable a, Integral a) => Ptr a -> Int -> [String] -> IO String"
                             , "+      go :: (Storable a, Integral a, Show a) => Ptr a -> Int -> [String] -> IO String"
                             , "       go !q !n acc"
                             , "           | n >= len  = return $ concat (reverse acc)"
                             , "           | otherwise = do w <- peekElemOff q n"
                             , "                            go q (n+1) (draw w : acc)"
                             , " "
                             , "-      draw :: (Integral a) => a -> String"
                             , "+      draw :: (Integral a, Show a) => a -> String"
                             , "       draw w = case showHex w [] of"
                             , "                  [x] -> ['0', x]"
                             , "                  x   -> x" ])
    , patched "openid" []
                    (unlines
                      [ "--- old/openid.cabal\t2012-01-23 15:04:29.547162493 -0800"
                      , "+++ new/openid.cabal\t2012-01-23 15:04:38.637160245 -0800"
                      , "@@ -28,11 +28,11 @@"
                      , " library"
                      , "   build-depends:   base       >= 4.0.0.0  && < 5.0.0.0,"
                      , "                    bytestring >= 0.9.1.0  && < 0.10.0.0,"
                      , "-                   containers >= 0.2.0.0  && < 0.4.1.0,"
                      , "-                   HTTP       >= 4000.0.9 && < 4000.2,"
                      , "+                   containers >= 0.2.0.0,"
                      , "+                   HTTP       >= 4000.0.9,"
                      , "                    monadLib   >= 3.6.0.0  && < 3.7.0.0,"
                      , "                    network    >= 2.2.0.0  && < 2.4.0.0,"
                      , "-                   time       >= 1.1.0.0  && < 1.3.0.0,"
                      , "+                   time       >= 1.1.0.0,"
                      , "                    xml        >= 1.3.0.0  && < 1.4.0.0,"
                      , "                    HsOpenSSL  >= 0.9.0.0  && < 0.11.0.0"
                      , "   hs-source-dirs:  src"
                      , "--- old/src/Data/Digest/OpenSSL/AlternativeHMAC.hsc\t2012-01-23 15:26:46.027160840 -0800"
                      , "+++ new/src/Data/Digest/OpenSSL/AlternativeHMAC.hsc\t2012-01-20 11:40:25.566842934 -0800"
                      , "@@ -59,7 +59,7 @@"
                      , " showHMAC bs ="
                      , "     concatMap draw $ BS.unpack bs"
                      , "     where"
                      , "-      draw :: (Integral a) => a -> String"
                      , "+      draw :: (Integral a, Show a) => a -> String"
                      , "       draw w = case showHex w [] of"
                      , "                  [x] -> ['0', x]"
                      , "                  x   -> x" ])
    , debianize "operational" [P.DebVersion "0.2.0.3-1~hackage1", P.OmitLTDeps]
    , debianize "ordered" []
    , P.Package { P.name = "haskell-pandoc"
                , P.spec = Debianize (Patch
                                      (Hackage "pandoc")
                                      (unlines
                                             [ "--- old/pandoc.cabal\t2012-04-13 20:31:20.000000000 -0700"
                                             , "+++ new/pandoc.cabal\t2012-04-13 23:36:47.139174491 -0700"
                                             , "@@ -193,7 +193,7 @@"
                                             , "   Build-Depends: containers >= 0.1 && < 0.5,"
                                             , "                  parsec >= 3.1 && < 3.2,"
                                             , "                  blaze-html >= 0.4.3.0 && < 0.5,"
                                             , "-                 mtl >= 1.1 && < 2.1,"
                                             , "+                 mtl >= 1.1,"
                                             , "                  network >= 2 && < 2.4,"
                                             , "                  filepath >= 1.1 && < 1.4,"
                                             , "                  process >= 1 && < 1.2,"
                                             , "@@ -291,7 +291,7 @@"
                                             , "   Build-Depends: containers >= 0.1 && < 0.5,"
                                             , "                  parsec >= 3.1 && < 3.2,"
                                             , "                  blaze-html >= 0.4.3.0 && < 0.5,"
                                             , "-                 mtl >= 1.1 && < 2.1,"
                                             , "+                 mtl >= 1.1,"
                                             , "                  network >= 2 && < 2.4,"
                                             , "                  filepath >= 1.1 && < 1.4,"
                                             , "                  process >= 1 && < 1.2,"
                                             , "@@ -349,7 +349,7 @@"
                                             , "   Build-Depends: containers >= 0.1 && < 0.5,"
                                             , "                  parsec >= 3.1 && < 3.2,"
                                             , "                  blaze-html >= 0.4.3.0 && < 0.5,"
                                             , "-                 mtl >= 1.1 && < 2.1,"
                                             , "+                 mtl >= 1.1,"
                                             , "                  network >= 2 && < 2.4,"
                                             , "                  filepath >= 1.1 && < 1.4,"
                                             , "                  process >= 1 && < 1.2," ]))
                , P.flags = [] }
    , debianize "texmath" []
    , debianize "temporary" [P.DebVersion "1.1.2.3-1build1"]
    , debianize "pandoc-types" [P.DebVersion "1.9.1-1"]
    , debianize "parse-dimacs" [P.DebVersion "1.2-1~hackage1"]
    , debianize "parseargs" [P.DebVersion "0.1.3.2-2"]
    , apt release "haskell-parsec2"
    , P.Package { P.name = "haskell-pbkdf2",
                  P.spec = DebDir (Hackage "PBKDF2") (Darcs "http://src.seereason.com/pbkdf2-debian"),
                  P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , apt release "haskell-pcre-light"
    , debianize "permutation" [P.DebVersion "0.4.1-1~hackage1"]
    , debianize "polyparse" []
    , apt release "haskell-primitive"
    , debianize "PropLogic" []
{-  , P.Package { P.name = "haskell-proplogic"
                , P.spec = DebDir (Uri "http://www.bucephalus.org/PropLogic/PropLogic-0.9.tar.gz" "e2fb3445dd16d435e81d7630d7f78c01") (Darcs (repo ++ "/haskell-proplogic-debian"))
                , P.flags = [] } -}
    {- , P.Package { P.name = "haskell-propositional-classes"
                , P.spec = Darcs (repo ++ "/propositional-classes")
                , P.flags = [] } -}
    , debianize "PSQueue" [P.DebVersion "1.1-2"]
    , debianize "pwstore-purehaskell" [P.DebVersion "2.1-1~hackage1"]
    -- In Sid, source package haskell-quickcheck generates libghc-quickcheck2-*,
    -- but our debianize target becomes haskell-quickcheck2.  So we need to fiddle
    -- with the order here relative to haskell-quickcheck1. 
    -- lucidNatty [apt "haskell-quickcheck"] [] ++
    , apt release "haskell-quickcheck1"
    -- lucidNatty [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]] [debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"] ] ++
    -- , apt release "haskell-regex-tdfa"
    , debianize "regex-tdfa" [P.DebVersion "1.1.8-1"]
    , P.Package { P.name = "haskell-revision"
                , P.spec = Darcs "http://src.seereason.com/haskell-revision"
                , P.flags = [] }
    , debianize "RJson" []
    , debianize "safe" [P.DebVersion "0.3.3-2"]
    -- Depends on pandoc
    --, P.Package {P.name = "haskell-safecopy", P.spec = DebDir (Hackage "safecopy" [P.CabalPin "0.5.1"])) (Darcs "http://src.seereason.com/haskell-safecopy-debian" []), P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"]}
    , debianize "safecopy" []
{-  , P.Package { P.name = "haskell-safecopy05"
                , P.spec = Quilt (Hackage "safecopy" [P.CabalPin "0.5.1"])) (Darcs (repo ++ "/safecopy05-quilt") [])
                , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] } -}
    , patched "sat"
                    [ P.DebVersion "1.1.1-1~hackage1" ]
                    (unlines
                      [ "--- sat/sat.cabal.orig\t2011-09-10 10:16:05.000000000 -0700"
                      , "+++ sat/sat.cabal\t2011-09-10 14:14:46.784184607 -0700"
                      , "@@ -13,7 +13,7 @@"
                      , " description: CNF(Clausal Normal Form) SATisfiability Solver and Generator"
                      , " category: algorithms"
                      , " -- tested-with: ghc-6.4.2"
                      , "-build-depends: base"
                      , "+build-depends: base, random"
                      , " "
                      , " executable: SATSolve"
                      , " main-is: \"SATSolver.hs\"" ])
    , debianize "semigroups" []
    , debianize "sendfile" []
    , P.Package { P.name = "haskell-set-extra"
                , P.spec = Darcs "http://src.seereason.com/set-extra"
                , P.flags = [] }
    , apt release "haskell-sha"
    , debianize "shakespeare" []
    , debianize "shakespeare-css" []
    , P.Package { P.name = "haskell-simple-css",
                  P.spec = Patch (Debianize (Hackage "simple-css"))
                                 (unlines
                                  [ "--- old/src/SimpleCss.hs\t2012-03-25 07:53:41.000000000 -0700"
                                  , "+++ new/src/SimpleCss.hs\t2012-03-25 14:43:54.789176546 -0700"
                                  , "@@ -20,7 +20,7 @@"
                                  , " "
                                  , " --import qualified Data.Map as M"
                                  , " import Data.Hashable"
                                  , "-import qualified Data.HashMap.Lazy as H"
                                  , "+import qualified Data.HashMap.Lazy as HM"
                                  , " "
                                  , " import qualified Text.Blaze as H"
                                  , " import qualified Text.Blaze.Html5 as H"
                                  , "@@ -135,11 +135,11 @@"
                                  , " blazeSpec = HtmlSpec"
                                  , "     (blazeTag H.div)"
                                  , "     (blazeTag H.span)"
                                  , "-    (\\href -> blazeTag (H.a H.! HA.href (H.stringValue href)))"
                                  , "-    (\\s x -> x H.! HA.class_ (H.stringValue s))"
                                  , "+    (\\href -> blazeTag (H.a H.! HA.href (H.toValue href)))"
                                  , "+    (\\s x -> x H.! HA.class_ (H.toValue s))"
                                  , " "
                                  , " blazeTag tag xs"
                                  , "-    | null xs   = tag $ H.string \"\""
                                  , "+    | null xs   = tag $ H.toHtml \"\""
                                  , "     | otherwise = tag $ foldl1 (>>) xs"
                                  , " "
                                  , " -------------------------------------------------------------------------"
                                  , "@@ -149,7 +149,7 @@"
                                  , "     deriving (Show, Eq, Ord)"
                                  , " "
                                  , " type ClassId    = String"
                                  , "-type ClassTable = H.HashMap Rule ClassId"
                                  , "+type ClassTable = HM.HashMap Rule ClassId"
                                  , " "
                                  , " data CssTag a = DivTag | SpanTag | ATag Href | Prim a"
                                  , " "
                                  , "@@ -171,7 +171,7 @@"
                                  , " "
                                  , " -- class names table"
                                  , " classTable :: [Rule] -> ClassTable"
                                  , "-classTable = H.fromList . flip zip ids . nub"
                                  , "+classTable = HM.fromList . flip zip ids . nub"
                                  , "     where ids = map (('c' : ). show) [0 ..]  "
                                  , "           phi id (a, b) = (b, (id, a))"
                                  , "  "
                                  , "@@ -245,7 +245,7 @@"
                                  , " -- print ruleSets"
                                  , " ppRuleSets :: ClassTable -> [RuleSet]"
                                  , " ppRuleSets = ((uncurry $ flip toRuleSet) =<< ) "
                                  , "-    . sortOn snd . H.toList"
                                  , "+    . sortOn snd . HM.toList"
                                  , " "
                                  , " "
                                  , " toRuleSet :: String -> Rule -> [RuleSet]"
                                  , "@@ -260,7 +260,7 @@"
                                  , " ppHtml :: HtmlSpec a -> ClassTable -> TagTree a -> a"
                                  , " ppHtml spec table (TagTree (CssNode tag rules) xs) = "
                                  , "     setAttrs spec attrs next"
                                  , "-    where attrs = map (maybe [] id . flip H.lookup table) rules"
                                  , "+    where attrs = map (maybe [] id . flip HM.lookup table) rules"
                                  , "           next  = case tag of"
                                  , "                     Prim a    -> a"
                                  , "                     DivTag    -> divTag  spec next'" ])
                , P.flags = [P.DebVersion "0.0.4-1~hackage1"] }
    , debianize "SMTPClient" [P.DebVersion "1.0.4-3"]
    , debianize "socks" [P.DebVersion "0.4.1-1"]
    , debianize "split" [P.DebVersion "0.1.4.2-1"]
    -- This package becomes the debian package "haskell-haskell-src-exts".
    -- Unfortunately, debian gave it the name "haskell-src-exts" dropping
    -- the extra haskell from source and binary names.  This means other
    -- packages (such as haskell-hsx) which reference it get the name wrong
    -- when we generate the debianization.
    -- , lucidNatty (hackage release "haskell-src-exts" [NP]) (debianize "haskell-src-exts" [])
    , debianize "haskell-src-exts" [P.ExtraDep "happy"]
    , debianize "stb-image" []
    , apt release "haskell-strict" -- for leksah
    , debianize "strict-concurrency" [P.DebVersion "0.2.4.1-3"]
    , debianize "strict-io" [] -- for GenI
    , debianize "smallcheck" []
    -- Because 0.3.3-1 is both in sid and hackage, we need to keep the debianize
    -- code from using version 0.3.3-1~hackage1 which looks older.
    , debianize "syb-with-class" [P.DebVersion "0.6.1.3-1build1"]
    , apt release "haskell-syb-with-class-instances-text"
    , debianize "tagged" []
    , debianize "tagsoup" [P.DebVersion "0.12.6-1"]
    , debianize "tar" []
    , debianize "terminfo" [P.DebVersion "0.3.2.3-2"]
    , debianize "test-framework"
                    [ {- P.Patch . B.pack. unlines $
                      [ "--- old/Test/Framework/Runners/Console/Run.hs\t2012-01-20 11:09:22.000000000 -0800"
                      , "+++ new/Test/Framework/Runners/Console/Run.hs\t2012-01-20 11:34:42.187163011 -0800"
                      , "@@ -18,7 +18,7 @@"
                      , " "
                      , " import Text.PrettyPrint.ANSI.Leijen"
                      , " "
                      , "-import Data.Monoid"
                      , "+import Data.Monoid (Monoid(..))"
                      , " "
                      , " import Control.Arrow (second, (&&&))"
                      , " import Control.Monad (unless)" ]
                    , P.ExtraDep "libghc-random-prof" -} ]
    , debianize "test-framework-hunit" [P.DebVersion "0.2.7-1"]
    , patched "test-framework-quickcheck" []
                    (unlines
                      [ "--- old/test-framework-quickcheck.cabal\t2012-02-02 16:33:53.000000000 -0800"
                      , "+++ new/test-framework-quickcheck.cabal\t2012-02-02 18:10:11.000000000 -0800"
                      , "@@ -26,8 +26,7 @@"
                      , "         if flag(base3)"
                      , "                 Build-Depends:          base >= 3 && < 4, random >= 1, deepseq >= 1.1 && < 1.3"
                      , "         else"
                      , "-                if flag(base4)"
                      , "-                        Build-Depends:          base >= 4 && < 5, random >= 1, deepseq >= 1.1 && < 1.3"
                      , "+                Build-Depends:          base >= 4 && < 5, random >= 1, deepseq >= 1.1"
                      , " "
                      , "         Extensions:             TypeSynonymInstances"
                      , "                                 TypeOperators" ])
    , debianize "test-framework-quickcheck2" []
    , debianize "testpack" []
    , debianize "th-expand-syns" []
    , debianize "th-lift" []
    , debianize "transformers-base" [P.DebVersion "0.4.1-2"]
    , debianize "unicode-names" [P.DebVersion "3.2.0.0-1~hackage1"]
    , patched "unicode-properties"
                    [ P.DebVersion "3.2.0.0-1~hackage1" ]
                    (unlines
                          [ "--- haskell-unicode-properties-3.2.0.0/Data/Char/Properties/MiscData.hs~\t2011-12-04 10:25:17.000000000 -0800"
                          , "+++ haskell-unicode-properties-3.2.0.0/Data/Char/Properties/MiscData.hs\t2011-12-04 11:25:53.000000000 -0800"
                          , "@@ -1,4 +1,3 @@"
                          , "-{-# OPTIONS -fvia-C #-}"
                          , " module Data.Char.Properties.MiscData where"
                          , " {"
                          , " \timport Data.Char.Properties.PrivateData;"
                          , "--- haskell-unicode-properties-3.2.0.0/Data/Char/Properties/CaseData.hs~\t2011-12-04 10:25:17.000000000 -0800"
                          , "+++ haskell-unicode-properties-3.2.0.0/Data/Char/Properties/CaseData.hs\t2011-12-04 11:24:00.000000000 -0800"
                          , "@@ -1,4 +1,3 @@"
                          , "-{-# OPTIONS -fvia-C #-}"
                          , " module Data.Char.Properties.CaseData where"
                          , " {"
                          , " \timport Data.Map;" ])
    , debianize "uniplate" []
    -- , apt release "haskell-unix-compat"
    , debianize "unix-compat" [P.DebVersion "0.3.0.1-1build1"]
    , debianize "Unixutils-shadow" []
    , debianize "unordered-containers" []
    , debianize "utf8-prelude" [P.DebVersion "0.1.6-1~hackage1"]
    , P.Package { P.name = "haskell-utf8-string"
                , P.spec = Apt "sid" "haskell-utf8-string"
                , P.flags = [P.RelaxDep "hscolour", P.RelaxDep "cpphs"] }
    , debianize "unification-fd" []
    , P.Package { P.name = "haskell-logict"
                , P.spec = Debianize (Patch 
                                      (Hackage "logict") 
                                      (unlines
                                       [ "--- old/logict.cabal\t2012-04-17 05:38:33.000000000 -0700"	
                                       , "+++ new/logict.cabal\t2012-04-17 05:55:22.242593856 -0700"	
                                       , "@@ -17,7 +17,7 @@"		    
                                       , " " 
                                       , " Stability:              Experimental"	
                                       , " Tested-With:            GHC"		
                                       , "-Build-Depends:          base >=2 && < 5, mtl>=1.0.1 && <2.1"	
                                       , "+Build-Depends:          base >=2 && < 5, mtl>=1.0.1"   
                                       , " Build-Type:             Simple"  
                                       , " "			    
                                       , " Exposed-Modules:        Control.Monad.Logic," ]))
                , P.flags = [P.DebVersion "0.5.0-2"] }
    , apt release "haskell-utility-ht"
    , debianize "vacuum" [P.DebVersion "1.0.0.2-1~hackage1"]
    -- Requires devscripts 0.8.9, restore when that gets built
    -- apt release "haskell-vector"
    -- Version 0.9-1+seereason1~lucid1 is uploaded to lucid already,
    -- remove this pin when a new hackage version comes out to trump it.
    , debianize "vector" [P.DebVersion "0.9.1-2"]
    , apt release "haskell-vector-algorithms"
    , patched "virthualenv" [ P.DebVersion "0.2-1~hackage1" ]
                    (unlines
                     [ "--- old/virthualenv.cabal\t2012-04-16 11:19:52.000000000 -0700"
                     , "+++ new/virthualenv.cabal\t2012-04-16 12:27:37.987322472 -0700"
                     , "@@ -98,12 +98,12 @@"
                     , " "
                     , "   Ghc-options: -threaded -Wall"
                     , " "
                     , "-  Build-depends: base >= 4.2.0.0 && < 4.5"
                     , "+  Build-depends: base >= 4.2.0.0"
                     , "                , process >= 1.0.1.2 && < 1.2"
                     , "-               , filepath >= 1.1.0.3 && < 1.3"
                     , "+               , filepath >= 1.1.0.3"
                     , "                , directory >= 1.0.1.0 && < 1.2"
                     , "-               , Cabal >= 1.8.0.6 && < 1.13"
                     , "-               , mtl >= 1.1.0.2 && < 2.1"
                     , "+               , Cabal >= 1.8.0.6"
                     , "+               , mtl >= 1.1.0.2"
                     , "                , bytestring >= 0.9.1.7 && < 0.10"
                     , "                , file-embed >= 0.0.4.1 && < 0.1"
                     , "                , split >= 0.1.4 && < 0.2" ])
    , debianize "vault" [P.DebVersion "0.1.0.0-2build1"]
    , patched "web-encodings" []
                    (unlines
                      [ "--- old/web-encodings.cabal\t2012-01-20 06:47:07.000000000 -0800"
                      , "+++ new/web-encodings.cabal\t2012-01-20 08:55:12.256222810 -0800"
                      , "@@ -22,7 +22,7 @@"
                      , "                      old-locale >= 1.0.0.1 && < 1.1,"
                      , "                      bytestring >= 0.9.1.4 && < 0.10,"
                      , "                      text >= 0.11 && < 0.12,"
                      , "-                     failure >= 0.0.0 && < 0.2,"
                      , "+                     failure >= 0.0.0 && < 0.3,"
                      , "                      directory >= 1 && < 1.2"
                      , "     exposed-modules: Web.Encodings"
                      , "                      Web.Encodings.MimeHeader," ])
    , debianize "boomerang" []
    , P.Package { P.name = "haskell-web-routes"
                , P.spec = Cd "web-routes" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-boomerang"
                , P.spec = Cd "web-routes-boomerang" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-happstack"
                , P.spec = Cd "web-routes-happstack" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-hsp"
                , P.spec = Cd "web-routes-hsp" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , P.Package { P.name = "haskell-web-routes-mtl"
                , P.spec = Cd "web-routes-mtl" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }      
    , P.Package { P.name = "haskell-web-routes-th"
                , P.spec = Cd "web-routes-th" (Darcs (repo ++ "/web-routes"))
                , P.flags = [] }
    , apt release "haskell-xml"
    , debianize "cookie" [P.DebVersion "0.4.0-1"]
    , debianize "lifted-base" [P.DebVersion "0.1.0.3-1"]
    , debianize "system-filepath" [P.DebVersion "0.4.6-1"]
    , patched "xml-enumerator" []
                 (unlines
                   [ "--- old/xml-enumerator.cabal\t2012-01-20 06:47:15.000000000 -0800"
                   , "+++ new/xml-enumerator.cabal\t2012-01-20 09:54:58.246541244 -0800"
                   , "@@ -42,9 +42,9 @@"
                   , "                    , attoparsec                >= 0.10"
                   , "                    , blaze-builder             >= 0.2      && < 0.4"
                   , "                    , blaze-builder-enumerator  >= 0.2      && < 0.3"
                   , "-                   , transformers              >= 0.2      && < 0.3"
                   , "-                   , failure                   >= 0.1      && < 0.2"
                   , "-                   , data-default              >= 0.2      && < 0.4"
                   , "+                   , transformers              >= 0.2"
                   , "+                   , failure                   >= 0.1"
                   , "+                   , data-default              >= 0.2"
                   , "     exposed-modules: Text.XML.Stream.Parse"
                   , "                      Text.XML.Stream.Render"
                   , "                      Text.XML.Unresolved"
                   , "@@ -61,7 +61,7 @@"
                   , "     build-depends:          base                         >= 4               && < 5"
                   , "                           , containers                   >= 0.2             && < 0.5"
                   , "                           , text                                               < 1"
                   , "-                          , transformers                 >= 0.2             && < 0.3"
                   , "+                          , transformers                 >= 0.2"
                   , "                           , enumerator                   >= 0.4.14          && < 0.5"
                   , "                           , bytestring                   >= 0.9             && < 0.10"
                   , "                           , xml-enumerator               >= 0.4             && < 0.5" ])
    , debianize "xml-types" [P.DebVersion "0.3.1-2"]
    , debianize "xss-sanitize" []
    , debianize "yaml-light" [P.DebVersion "0.1.4-2"]
    , apt release "haskell-zip-archive"

    , P.Package { P.name = "haskell-highlighting-kate"
                , P.spec = Debianize (Hackage "highlighting-kate")
                , P.flags = [P.DebVersion "0.5.0.5-1"] }
    , debianize "regex-pcre-builtin" []
    , P.Package { P.name = "hscolour"
                , P.spec = Apt "sid" "hscolour"
                , P.flags = [P.RelaxDep "hscolour"] }
    , apt release "hslogger"
    , apt release "html-xml-utils"
    , P.Package { P.name = "jquery"
                , P.spec = Proc (Apt "sid" "jquery")
                , P.flags = [] }
    , P.Package { P.name = "jquery-goodies"
                , P.spec = Proc (Apt "sid" "jquery-goodies")
                , P.flags = [] }
    , P.Package { P.name = "jqueryui"
                , P.spec = Proc (Apt "sid" "jqueryui")
                , P.flags = [] }
    , P.Package { P.name = "jcrop"
                , P.spec = DebDir (Uri "http://src.seereason.com/jcrop/Jcrop.tar.gz" "6bf28a79e2fa1856a9d8eef6c038060e") (Darcs (repo ++ "/jcrop-debian"))
                , P.flags = [] }
    , P.Package { P.name = "magic-haskell"
                , P.spec = Quilt (Apt "sid" "magic-haskell") (Darcs (repo ++ "/magic-quilt"))
                , P.flags = [] }
    , debianize "MissingH" [P.DebVersion "1.1.1.0-1~hackage1"]
    , P.Package { P.name = "seereason-keyring"
                , P.spec = Darcs "http://src.seereason.com/seereason-keyring"
                , P.flags = [P.UDeb "seereason-keyring-udeb"] }
    , apt release "tinymce"
    , P.Package { P.name = "vc-darcs"
                , P.spec = Darcs "http://src.seereason.com/vc-darcs"
                , P.flags = [] }
    -- , debianize "hlatex" []
    ]

ghc release =
    if elem release ["squeeze-seereason"]
    then
        P.Package { P.name = "ghc"
                  , P.spec = Patch (Apt "sid" "ghc")
                             (unlines
                              [ "--- old/ghc-7.4.1/debian/control\t2012-03-10 10:38:09.000000000 -0800"
                              , "+++ new/ghc-7.4.1/debian/control\t2012-03-22 17:18:51.548720165 -0700"
                              , "@@ -6,7 +6,7 @@"
                              , " Standards-Version: 3.9.2"
                              , " Build-Depends:"
                              , "   debhelper (>= 7),"
                              , "-  libgmp-dev,"
                              , "+  libgmp-dev | libgmp3-dev,"
                              , "   devscripts,"
                              , "   ghc,"
                              , "   grep-dctrl,"
                              , "@@ -31,7 +31,7 @@"
                              , " "
                              , " Package: ghc"
                              , " Architecture: any"
                              , "-Depends: gcc (>= 4:4.2), llvm-3.0 [armel armhf], libgmp-dev, libffi-dev, libbsd-dev, libc6-dev, ${shlibs:Depends}, ${misc:Depends}"
                              , "+Depends: gcc (>= 4:4.2), llvm-3.0 [armel armhf], libgmp-dev | libgmp3-dev, libffi-dev, libbsd-dev, libc6-dev, ${shlibs:Depends}, ${misc:Depends}"
                              , " Provides: haskell-compiler, ${provided-devs}, ${haskell:Provides}, ${ghci}"
                              , " Replaces: ghc6 (<< 7)"
                              , " Conflicts: ghc6 (<< 7), ${provided-devs}" ])
                  , P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt","python-minimal","libgmp-dev"] }
    else if elem release ["lucid-seereason", "natty-seereason"]
         then P.NoPackage
         else (apt "sid" "ghc") {P.flags = map P.RelaxDep ["ghc","happy","alex","xsltproc","debhelper","quilt","python-minimal","libgmp-dev"]}

platform release =
    P.Packages (singleton "platform") $
    [ let dist = (case release of 
                     "precise-seereason" -> "precise"
                     _ -> "sid") in
      P.Package { P.name = "haskell-devscripts"
                , P.spec = Apt dist "haskell-devscripts"
                , P.flags = [P.RelaxDep "python-minimal"] }
    , -- Our automatic debianization code produces a package which is
      -- missing the template files required for happy to work properly,
      -- so I have imported debian's debianization and patched it to
      -- work with ghc 7.4.1.  Note that this is also the first target
      -- to require the new "install orig.tar.gz file" code in the
      -- autobuilder.
      P.Package { P.name = "happy",
                  P.spec = DebDir (Hackage "happy") (Darcs "http://src.seereason.com/happy-debian"),
                  P.flags = [P.RelaxDep "happy", 
                             P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] }
    {- , debianize "happy" [] -}
    , debianize "stm" []
    , apt release "haskell-zlib"
    -- , apt "haskell-deepseq"
    , case release of
        "natty-seereason" -> P.NoPackage -- a version newer than the latest in hackage is bundled with ghc
        _ -> P.NoPackage -- debianize "deepseq" []

    , case release of
        "precise-seereason" -> debianize "mtl" []
        _ -> P.Package { P.name = "haskell-mtl"
                       , P.spec = Apt "sid" "haskell-mtl"
                       , P.flags = [{-P.AptPin "2.0.1.0-3"-}] }
    , case release of
        "precise-seereason" -> debianize "transformers" []
        _ -> P.Package { P.name = "haskell-transformers"
                       , P.spec = Apt "sid" "haskell-transformers"
                       , P.flags = [{-P.AptPin "0.2.2.0-3"-}] }
    , debianize "parallel" [P.DebVersion "3.2.0.2-2build1"]
    , debianize "syb" [P.DebVersion "0.3.6-1"]
    , debianize "fgl" [P.DebVersion "5.4.2.4-2"]
    , debianize "text" []
    , P.Package { P.name = "alex"
                , P.spec = Apt "sid" "alex"
                , P.flags = [P.RelaxDep "alex"] }
    , opengl release
    -- , haddock release
    , debianize "haskell-src" [ P.ExtraDep "happy", P.DebVersion "1.0.1.5-1" ]
    , debianize "network" []
    , debianize "HTTP" [P.DebVersion "1:4000.2.3-1~hackage1"]
    , P.Package { P.name = "haskell-cgi"
                , P.spec = Debianize (Hackage "cgi")
                -- , P.spec = DebDir (Uri "http://hackage.haskell.org/packages/archive/cgi/3001.1.8.2/cgi-3001.1.8.2.tar.gz" "4092efaf00ac329b9771879f57a95323") (Darcs "http://src.seereason.com/haskell-cgi-debian")
                , P.flags = [P.DebVersion "3001.1.8.2-2"] }
    -- This is bundled with the compiler
    -- , debianize "process" []
    -- Random is built into 7.0, but not into 7.2, and the version
    -- in hackage is incompatible with the version shipped with 7.0.
    , debianize "random" [P.DebVersion "1.0.1.1-1"]
    , debianize "HUnit" [P.DebVersion "1.2.4.2-3"]
    , debianize "QuickCheck" [P.ExtraDep "libghc-random-prof"]
    , apt release "haskell-parsec"
    , apt release "haskell-html"
    , apt release "haskell-regex-compat"
    , apt release "haskell-regex-base"
    , apt release "haskell-regex-posix"
    , debianize "xhtml" [P.DebVersion "3000.2.0.5-1"]
    ]

-- | Packages pinned pending update of happstack-authenticate (in one possible build order.)
authenticate _home release = 
  P.Packages (singleton "authenticate") $
    [ apt release "haskell-puremd5"
    , debianize "monadcryptorandom" []
    , debianize "RSA" []
    , debianize "resourcet" []
    , debianize "conduit" []
    , debianize "void" []
    , debianize "certificate" []
    , debianize "pem" []
    , debianize "zlib-bindings" []
    , debianize "tls" []
    , debianize "tls-extra" []
    , debianize "attoparsec-conduit" []
    , debianize "blaze-builder-conduit" []
    , debianize "zlib-conduit" []
    , P.Package { P.name = "haskell-http-conduit"
                , P.spec = Debianize (Hackage "http-conduit")
                , P.flags = [] }
    , P.Package { P.name = "haskell-xml-conduit"
                , P.spec = Debianize (Hackage "xml-conduit")
                , P.flags = [] }
    -- , debianize "authenticate" []
    , P.Package { P.name = "haskell-authenticate"
                , P.spec = Debianize (Hackage "authenticate")
                , P.flags = [] }

    , P.Package { P.name = "haskell-zlib-enum"
                , P.spec = Debianize (Patch 
                                      (Hackage "zlib-enum") 
                                      (unlines
                                       [ "--- old/zlib-enum.cabal\t2012-04-17 05:18:19.000000000 -0700"
                                       , "+++ new/zlib-enum.cabal\t2012-04-17 05:30:32.558673393 -0700"
                                       , "@@ -24,7 +24,7 @@"
                                       , "          Buildable: False"
                                       , "     build-depends: base                  >= 4 && < 5"
                                       , "                  , bytestring"
                                       , "-                 , transformers          == 0.2.*"
                                       , "+                 , transformers          >= 0.2"
                                       , "                  , enumerator            == 0.4.*"
                                       , "                  , zlib-bindings         == 0.1.*"
                                       , "     exposed-modules: Codec.Zlib.Enum"
                                       , "@@ -38,7 +38,7 @@"
                                       , "         build-depends:"
                                       , "               base                       >= 4.0   &&   < 5.0"
                                       , "             , bytestring                 >= 0.9   &&   < 0.10"
                                       , "-            , transformers               == 0.2.*"
                                       , "+            , transformers               >= 0.2"
                                       , "             , enumerator                 >= 0.4   &&   < 0.5"
                                       , "             , zlib-bindings              >= 0.1"
                                       , "             , test-framework             >= 0.6   &&   < 0.7" ]))
                , P.flags = [] }
    , debianize "wai" []
    , P.Package { P.name = "haskell-http-enumerator"
                , P.spec = Debianize (Patch
                                      (Hackage "http-enumerator")
                                      (unlines
                                       [ "--- old/http-enumerator.cabal\t2012-04-18 06:24:08.000000000 -0700"
                                       , "+++ new/http-enumerator.cabal\t2012-04-18 06:46:46.365816097 -0700"
                                       , "@@ -37,7 +37,7 @@"
                                       , "                  , tls-extra             >= 0.4.3   && < 0.5"
                                       , "                  , monad-control         >= 0.2     && < 0.4"
                                       , "                  , containers            >= 0.2"
                                       , "-                 , certificate           >= 1.1     && < 1.2"
                                       , "+                 , certificate           >= 1.1"
                                       , "                  , case-insensitive      >= 0.2"
                                       , "                  , base64-bytestring     >= 0.1     && < 0.2"
                                       , "                  , asn1-data             >= 0.5.1   && < 0.7" ]))
                , P.flags = [] }
    , P.Package { P.name = "haskell-happstack-authenticate"
                , P.spec = Debianize (Patch
                                      (Darcs (repo ++ "/happstack-authenticate"))
                                      (unlines
                                       [ "--- old/happstack-authenticate.cabal\t2012-04-17 16:46:28.000000000 -0700"
                                       , "+++ new/happstack-authenticate.cabal\t2012-04-17 16:51:02.576493389 -0700"
                                       , "@@ -31,16 +31,16 @@"
                                       , "                        blaze-html                   == 0.4.*,"
                                       , "                        bytestring                   == 0.9.*,"
                                       , "                        containers                   == 0.4.*,"
                                       , "-                       digestive-functors           == 0.2.*,"
                                       , "+                       digestive-functors           >= 0.2,"
                                       , "                        digestive-functors-happstack >= 0.1,"
                                       , "-                       digestive-functors-blaze     == 0.2.*,"
                                       , "+                       digestive-functors-blaze     >= 0.2,"
                                       , "                        ixset                        >= 1.0 && < 1.1,"
                                       , "                        happstack-server             >= 6.0 && < 7.1,"
                                       , "                        http-conduit                 == 1.4.*,"
                                       , "                        http-types                   == 0.6.*,"
                                       , "                        fb                           == 0.8.*,"
                                       , "                        safecopy                     == 0.6.*,"
                                       , "-                       mtl                          == 2.0.*,"
                                       , "+                       mtl                          >= 2.0,"
                                       , "                        pwstore-purehaskell          == 2.1.*,"
                                       , "                        QuickCheck                   >= 2,"
                                       , "                        text                         == 0.11.*," ]))
                , P.flags = [] }
    ]

clckwrks _home =
    let repo = "http://src.clckwrks.com/clckwrks" {- localRepo _home ++ "/clckwrks" -} in
    P.Packages (singleton "clckwrks") $
        [ P.Package { P.name = "haskell-clckwrks"
                    , P.spec = Debianize (Patch
                                          (DataFiles
                                           (DataFiles
                                            (Cd "clckwrks" (Darcs repo))
                                            (Uri "http://cloud.github.com/downloads/vakata/jstree/jstree_pre1.0_fix_1.zip"
                                                 "e211065e573ea0239d6449882c9d860d")
                                            "jstree")
                                           (Uri "https://raw.github.com/douglascrockford/JSON-js/master/json2.js"
                                                "95def87b93d11289cd2eee1cc3ca7948")
                                           "json2")
                                          (unlines
                                           [ "--- old/clckwrks.cabal.orig\t2012-03-06 11:34:28.000000000 -0800"
                                           , "+++ new/clckwrks.cabal\t2012-03-07 20:17:25.465527760 -0800"
                                           , "@@ -15,6 +15,79 @@"
                                           , " Cabal-version:       >=1.2"
                                           , " Data-Files:"
                                           , "     static/admin.css"
                                           , "+    jstree/_lib/jquery.hotkeys.js"
                                           , "+    jstree/_lib/jquery.js"
                                           , "+    jstree/_lib/jquery.cookie.js"
                                           , "+    jstree/jquery.jstree.js"
                                           , "+    jstree/_demo/_inc/class._database.php"
                                           , "+    jstree/_demo/_inc/class.tree.php"
                                           , "+    jstree/_demo/_inc/class._database_i.php"
                                           , "+    jstree/_demo/_inc/__mysql_errors.log"
                                           , "+    jstree/_demo/_install.txt"
                                           , "+    jstree/_demo/file.png"
                                           , "+    jstree/_demo/index.html"
                                           , "+    jstree/_demo/_dump.sql"
                                           , "+    jstree/_demo/config.php"
                                           , "+    jstree/_demo/root.png"
                                           , "+    jstree/_demo/server.php"
                                           , "+    jstree/_demo/folder.png"
                                           , "+    jstree/_docs/cookies.html"
                                           , "+    jstree/_docs/unique.html"
                                           , "+    jstree/_docs/_json_data.json"
                                           , "+    jstree/_docs/json_data.html"
                                           , "+    jstree/_docs/_html_data.html"
                                           , "+    jstree/_docs/sort.html"
                                           , "+    jstree/_docs/html_data.html"
                                           , "+    jstree/_docs/themeroller.html"
                                           , "+    jstree/_docs/_drive.png"
                                           , "+    jstree/_docs/xml_data.html"
                                           , "+    jstree/_docs/index.html"
                                           , "+    jstree/_docs/hotkeys.html"
                                           , "+    jstree/_docs/languages.html"
                                           , "+    jstree/_docs/ui.html"
                                           , "+    jstree/_docs/!style.css"
                                           , "+    jstree/_docs/checkbox.html"
                                           , "+    jstree/_docs/_search_data.json"
                                           , "+    jstree/_docs/syntax/page_white_code.png"
                                           , "+    jstree/_docs/syntax/wrapping.png"
                                           , "+    jstree/_docs/syntax/page_white_copy.png"
                                           , "+    jstree/_docs/syntax/!script.js"
                                           , "+    jstree/_docs/syntax/printer.png"
                                           , "+    jstree/_docs/syntax/!style.css"
                                           , "+    jstree/_docs/syntax/magnifier.png"
                                           , "+    jstree/_docs/syntax/clipboard.swf"
                                           , "+    jstree/_docs/syntax/help.png"
                                           , "+    jstree/_docs/_search_result.json"
                                           , "+    jstree/_docs/crrm.html"
                                           , "+    jstree/_docs/types.html"
                                           , "+    jstree/_docs/core.html"
                                           , "+    jstree/_docs/dnd.html"
                                           , "+    jstree/_docs/logo.png"
                                           , "+    jstree/_docs/search.html"
                                           , "+    jstree/_docs/contextmenu.html"
                                           , "+    jstree/_docs/themes.html"
                                           , "+    jstree/_docs/_xml_nest.xml"
                                           , "+    jstree/_docs/_xml_flat.xml"
                                           , "+    jstree/themes/default/d.gif"
                                           , "+    jstree/themes/default/d.png"
                                           , "+    jstree/themes/default/style.css"
                                           , "+    jstree/themes/default/throbber.gif"
                                           , "+    jstree/themes/apple/d.png"
                                           , "+    jstree/themes/apple/style.css"
                                           , "+    jstree/themes/apple/dot_for_ie.gif"
                                           , "+    jstree/themes/apple/bg.jpg"
                                           , "+    jstree/themes/apple/throbber.gif"
                                           , "+    jstree/themes/default-rtl/d.gif"
                                           , "+    jstree/themes/default-rtl/d.png"
                                           , "+    jstree/themes/default-rtl/dots.gif"
                                           , "+    jstree/themes/default-rtl/style.css"
                                           , "+    jstree/themes/default-rtl/throbber.gif"
                                           , "+    jstree/themes/classic/d.gif"
                                           , "+    jstree/themes/classic/d.png"
                                           , "+    jstree/themes/classic/style.css"
                                           , "+    jstree/themes/classic/dot_for_ie.gif"
                                           , "+    jstree/themes/classic/throbber.gif"
                                           , "+    json2/json2.js"
                                           , " "
                                           , " Library"
                                           , "   Exposed-modules: Clckwrks"
                                           , "@@ -83,7 +83,7 @@"
                                           , "      process                      >= 1.0 && < 1.2,"
                                           , " --     plugins-auto == 0.0.1.1,"
                                           , "      safecopy                     == 0.6.*,"
                                           , "-     stm                          == 2.2.*,"
                                           , "+     stm                          >= 2.2,"
                                           , "      tagsoup                      == 0.12.*,"
                                           , "      text                         == 0.11.*,"
                                           , "      time                         >= 1.2 && <1.5," ]))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-cli"
                    , P.spec = Debianize (Patch
                                          (Cd "clckwrks-cli" (Darcs repo))
                                          (unlines
                                           [ "--- old/clckwrks-cli.cabal\t2012-04-18 21:22:22.000000000 -0700"
                                           , "+++ new/clckwrks-cli.cabal\t2012-04-23 21:02:30.932816903 -0700"
                                           , "@@ -19,5 +19,5 @@"
                                           , "   Build-depends:"
                                           , "      acid-state == 0.6.*,"
                                           , "      base        < 5,"
                                           , "-     clckwrks   == 0.5.0,"
                                           , "+     clckwrks   >= 0.5.0,"
                                           , "      network    == 2.3.*" ]))
                    , P.flags = [] }
        , P.Package { P.name = "haskell-clckwrks-plugin-media"
                    , P.spec = Debianize (Cd "clckwrks-plugin-media" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-plugin-ircbot"
                    , P.spec = Debianize (Cd "clckwrks-plugin-ircbot" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , P.Package { P.name = "haskell-clckwrks-theme-basic"
                    , P.spec = Debianize (Cd "clckwrks-theme-basic" (Darcs repo))
                    , P.flags = [P.ExtraDep "haskell-hsx-utils"] }
        , debianize "jmacro" []
        , debianize "hsx-jmacro" []
        ]

-- Broken targets:
--
-- Text/JSONb/Decode.hs:48:3:
--     Not in scope: data constructor `Done'
--     Perhaps you meant `Attoparsec.Done' (imported from Data.Attoparsec.Char8)
-- 
-- Text/JSONb/Decode.hs:49:3:
--     Not in scope: data constructor `Fail'
--     Perhaps you meant `Attoparsec.Fail' (imported from Data.Attoparsec.Char8)
-- 
-- Text/JSONb/Decode.hs:50:3:
--     Not in scope: data constructor `Partial'
--     Perhaps you meant `Attoparsec.Partial' (imported from Data.Attoparsec.Char8)
jsonb = P.Packages (singleton "jsonb") $
    [ debianize "JSONb" [P.DebVersion "1.0.7-1~hackage1"]
    , debianize "data-object-json" [] ]

-- May work with these added dependencies (statevar thru openglraw)
opengl _release = P.Packages (singleton "opengl") $
    [ debianize "OpenGL" []
{-  , P.Package { P.name = "haskell-vacuum-opengl"
                , P.spec = Patch (Debianize (Hackage "vacuum-opengl"))
                                 (unlines
                                  [ "--- old/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:26:14.000000000 -0700"
                                  , "+++ new/System/Vacuum/OpenGL/Server.hs\t2012-03-25 14:32:17.027953252 -0700"
                                  , "@@ -34,7 +34,7 @@"
                                  , " "
                                  , " import Network"
                                  , " "
                                  , "-import Foreign"
                                  , "+import Foreign (shiftR)"
                                  , " import Foreign.C"
                                  , " "
                                  , " --------------------------------------------------------------------------------" ])
                , P.flags = [ P.DebVersion "0.0.3-1~hackage2" ] } -}
    , P.Package { P.name = "haskell-bitmap-opengl"
                , P.spec = Patch (Debianize (Hackage "bitmap-opengl"))
                                 (unlines
                                  [ "--- old/Data/Bitmap/OpenGL.hs\t2012-03-25 07:48:39.000000000 -0700"
                                  , "+++ new/Data/Bitmap/OpenGL.hs\t2012-03-25 09:07:54.849175710 -0700"
                                  , "@@ -11,6 +11,7 @@"
                                  , " --------------------------------------------------------------------------------"
                                  , " "
                                  , " import Data.Bitmap.IO"
                                  , "+import Data.Bitmap.Simple (withBitmap)"
                                  , " "
                                  , " import Graphics.Rendering.OpenGL"
                                  , " " ])
                , P.flags = [P.ExtraDep "libglu1-mesa-dev", P.DebVersion "0.0.0-1~hackage1"] }
    , debianize "GLUT" []
    , debianize "StateVar" [P.DebVersion "1.0.0.0-2build1"]
    , debianize "Tensor" []
    , debianize "GLURaw" []
    , debianize "ObjectName" []
    , debianize "OpenGLRaw" [ P.ExtraDep "libgl1-mesa-dev" ]
    ]

-- Problem compiling C code in glib:
--  System/Glib/hsgclosure.c:110:8:
--       error: void value not ignored as it ought to be
glib release = P.Packages (singleton "glib") $
    [ debianize "glib" [ P.ExtraDep "haskell-gtk2hs-buildtools-utils"
                       , P.ExtraDep "libglib2.0-dev"]
    , apt release "haskell-criterion"
    , apt release "haskell-ltk"
    , apt release "haskell-chart"
    , apt release "haskell-gio"
    , apt release "haskell-gtk"
    , apt release "haskell-gtksourceview2"
    , apt release "haskell-pango" ]

--  Using pkg-config version 0.25 found on system at: /usr/bin/ 2> 
--  <interactive>:2:1:
--      Failed to load interface for `Directory'
--      It is a member of the hidden package `haskell98-2.0.0.1'.
--      Use -v to see a list of the files searched for.
--  
--  src/System/Plugins/Utils.hs:21:8:
--      Warning: In the use of `catch'
--               (imported from Prelude, but defined in System.IO.Error):
--               Deprecated: "Please use the new exceptions variant, Control.Exception.catch"
--  
--  src/System/Plugins/Load.hs:91:35:
--      Module `GHC.Exts' does not export `addrToHValue#'
--  make: *** [build-ghc-stamp] Error 1
plugins = P.Packages (singleton "plugins") $
    [ patched "plugins"
                    [ P.DebVersion "1.5.1.4-1~hackage1" ]
                    (unlines
                      [ "--- haskell-plugins-1.5.1.4.orig/src/System/Plugins/Load.hs\t2011-12-07 07:13:54.000000000 -0800"
                      , "+++ haskell-plugins-1.5.1.4/src/System/Plugins/Load.hs\t2012-01-02 10:16:25.766481952 -0800"
                      , "@@ -84,7 +84,9 @@"
                      , " import System.Directory         ( doesFileExist, removeFile )"
                      , " import Foreign.C.String         ( CString, withCString, peekCString )"
                      , " "
                      , "+#if !MIN_VERSION_ghc(7,2,0)"
                      , " import GHC                      ( defaultCallbacks )"
                      , "+#endif"
                      , " import GHC.Ptr                  ( Ptr(..), nullPtr )"
                      , " import GHC.Exts                 ( addrToHValue# )"
                      , " import GHC.Prim                 ( unsafeCoerce# )"
                      , "@@ -99,7 +101,11 @@"
                      , " readBinIface' :: FilePath -> IO ModIface"
                      , " readBinIface' hi_path = do"
                      , "     -- kludgy as hell"
                      , "+#if MIN_VERSION_ghc(7,2,0)"
                      , "+    e <- newHscEnv undefined"
                      , "+#else"
                      , "     e <- newHscEnv defaultCallbacks undefined"
                      , "+#endif"
                      , "     initTcRnIf 'r' e undefined undefined (readBinIface IgnoreHiWay QuietBinIFaceReading hi_path)"
                      , " "
                      , " -- TODO need a loadPackage p package.conf :: IO () primitive"
                      , "@@ -679,7 +685,11 @@"
                      , " "
                      , "                 -- and find some packages to load, as well."
                      , "                 let ps = dep_pkgs ds"
                      , "+#if MIN_VERSION_ghc(7,2,0)"
                      , "+                ps' <- filterM loaded . map packageIdString . nub $ map fst ps"
                      , "+#else"
                      , "                 ps' <- filterM loaded . map packageIdString . nub $ ps"
                      , "+#endif"
                      , " "
                      , " #if DEBUG"
                      , "                 when (not (null ps')) $" ])
    , debianize "happstack-plugins" []
    , debianize "plugins-auto" [] ]

-- Control/Monad/Unpack.hs:33:3:
--      Illegal repeated type variable `a_a4L6'
higgsset = P.Packages (singleton "higgsset") $
    [ debianize "unpack-funcs" []
    , debianize "HiggsSet" []
    , debianize "TrieMap" [P.DebVersion "4.0.1-1~hackage1"] ]

-- ircbot needs a dependency on containers
happstackdotcom _home =
    P.Packages (singleton "happstackdotcom") $
    [ P.Package { P.name = "haskell-ircbot"
                , P.spec = Debianize (Patch
                                      (Darcs "http://patch-tag.com/r/stepcut/ircbot")
                                      (unlines
                                       [ "--- old/ircbot.cabal\t2012-04-17 05:18:09.000000000 -0700"	
                                       , "+++ new/ircbot.cabal\t2012-04-17 05:34:20.122661243 -0700"  
                                       , "@@ -35,7 +35,7 @@"	
                                       , "                   directory   < 1.2,"	
                                       , "                   filepath   >= 1.2 && < 1.4,"	
                                       , "                   irc        == 0.5.*,"  
                                       , "-                  mtl        == 2.0.*,"  
                                       , "+                  mtl        >= 2.0,"    
                                       , "                   network    == 2.3.*,"  
                                       , "                   old-locale == 1.0.*,"  
                                       , "                   parsec     == 3.1.*," ]))
                , P.flags = [] }
{-  , P.Package { P.name = "haskell-happstackdotcom"
                , P.spec = Darcs ("http://src.seereason.com/happstackDotCom")
                , P.flags = [] } -}
    , P.Package { P.name = "haskell-happstackdotcom-doc"
                , P.spec = Darcs "http://src.seereason.com/happstackDotCom-doc"
                , P.flags = [] } ]

frisby = P.Packages (singleton "frisby")
    [ P.Package { P.name = "haskell-frisby"
                , P.spec = DebDir (Cd "frisby" (Darcs "http://src.seereason.com/frisby")) (Darcs "http://src.seereason.com/frisby-debian")
                , P.flags = [] }
    , P.Package { P.name = "haskell-decimal"
                , P.spec = Darcs "http://src.seereason.com/decimal"
                , P.flags = [] } ]

haddock release =
    -- For leksah.  Version 2.9.2 specifies ghc < 7.2 and base ==
    -- 4.3.* so we can't use "debianize "haddock" []".  I don't think
    -- we really need this, or the hackage version.  Version 2.10.0 is
    -- included with ghc 7.4.0.
    [ apt release "haskell-haddock" ]

-- These have been failing for some time, and I don't think we've missed them.
failing release =
    [ debianize "funsat" []
    , apt release "haskell-statistics" ]

algebra =
    [ debianize "adjunctions" []
    , debianize "algebra" []
    , debianize "bifunctors" []
    , debianize "categories" []
    , debianize "comonad" []
    , debianize "comonads-fd" []
    , debianize "comonad-transformers" []
    , debianize "contravariant" []
    , debianize "data-lens" []
    , debianize "distributive" []
    , debianize "free" []
    , debianize "keys" []
    , debianize "representable-functors" []
    , debianize "representable-tries" []
    , debianize "semigroupoids" [] ]

-- Debian package has versioned dependencies on binary, but the
-- virtual binary package provided with ghc 7.4 (0.5.1.0) is
-- newer than the version of binary in hackage (0.5.0.2.)  This
-- means we try to pull in bogus debs for libghc-binary-* and
-- dependency problems ensue.
agda release =
    [ apt release "agda"
    , apt release "agda-bin"
    , apt release "agda-stdlib" ]

other release =
    [ apt release "darcs"
    , patched "aeson-native" []
                    (unlines
                      [ "--- old-aeson-native/aeson-native.cabal\t2011-12-03 08:17:32.000000000 -0800"
                      , "+++ new-aeson-native/aeson-native.cabal\t2012-01-02 12:33:12.776486492 -0800"
                      , "@@ -119,7 +119,7 @@"
                      , "     blaze-textual-native >= 0.2.0.2,"
                      , "     bytestring,"
                      , "     containers,"
                      , "-    deepseq < 1.2,"
                      , "+    deepseq < 1.3,"
                      , "     hashable >= 1.1.2.0,"
                      , "     mtl,"
                      , "     old-locale," ])
    , apt release "haskell-binary-shared" -- for leksah
    , debianize "cairo" [P.ExtraDep "haskell-gtk2hs-buildtools-utils"] -- for leksah
    , debianize "gnuplot" [P.DebVersion "0.4.2-1~hackage1"]
    , apt release "bash-completion"
    ]

apt :: String -> String -> P.Packages
apt release name =
          let dist =
                  case release of
                    -- Several packages in oneiric are newer looking than the ones in sid
                    "oneiric-seereason" ->
                        case name of
                          "haskell-opengl" -> "oneiric"
                          "haskell-mtl" -> "oneiric"
                          "haskell-utility-ht" -> "oneiric"
                          "haskell-utf8-string" -> "oneiric"
                          "haskell-deepseq" -> "oneiric"
                          "haskell-dlist" -> "oneiric"
                          "haskell-sha" -> "oneiric"
                          _ -> "sid"
                    _ -> "sid"
              version =
                  case release of
                    "oneiric-seereason" ->
                        case name of
                          "haskell-deepseq" -> Just "1.1.0.2-2"
                          _ -> Nothing
                    _ ->
                        case name of
                          "haskell-deepseq" -> Just "1.1.0.2-2"
                          _ -> Nothing in
          P.Package
               { P.name = name
               , P.spec = Apt dist name
               , P.flags = (maybe [] (\ v -> [P.AptPin v]) version) }

{-
-- |Here is a program to generate a list of all the packages in sid that have ghc for a build dependency.

#!/usr/bin/env runghc

import Data.Maybe (catMaybes)
import Debian.Control (Control'(unControl), parseControlFromFile, fieldValue)
import Debian.Relation (Relation(Rel), parseRelations)

main =
    parseControlFromFile "/home/dsf/.autobuilder/dists/sid/aptEnv/var/lib/apt/lists/mirrors.usc.edu_pub_linux_distributions_debian_dists_sid_main_source_Sources" >>=
    either (error "parse") (mapM_ putStrLn . catMaybes . map checkPackage . unControl)
    where
      checkPackage p =
          if any (\ (Rel name _ _) -> name == "ghc") rels then fieldValue "Package" p else Nothing
          where
            rels = either (const []) concat $
                       maybe (Left undefined) parseRelations $
                           fieldValue "Build-Depends" p
-}

-- | Build a target that pulls the source from hackage and then
-- generates a debianization using cabal-debian.  Note that this will
-- affect the debian source package names for a given cabal package,
-- but it does not affect the dependency names generated by cabal
-- debian when it debianizes a package.
debianize :: String -> [P.PackageFlag] -> P.Packages
debianize s flags =
    P.Package { P.name = debianName s
              , P.spec = Debianize (Hackage s)
              , P.flags = [P.Maintainer "SeeReason Autobuilder <partners@seereason.com>"] ++ flags ++ [P.Revision ""]}
    where
      -- This is a quick hack, but what we should do is have
      -- cabal-debian compute and return the source package name.
      debianName "QuickCheck" = "haskell-quickcheck2"
      debianName "parsec" = "haskell-parsec3"
      debianName "gtk2hs-buildtools" = "gtk2hs-buildtools"
      -- The correct name would be haskell-haskell-src-exts, but the package
      -- in sid has the name "haskell-src-exts".  (Update: we no longer use
      -- the haskell-src-exts package from sid.  Now packages from sid that
      -- depend on this package will fail, so we will have to remove those too.
      -- debianName "haskell-src-exts" = "haskell-src-exts"
      debianName "MissingH" = "haskell-missingh"
      debianName _ = "haskell-" ++ map toLower s

patched :: String -> [P.PackageFlag] -> String -> P.Packages
patched s flags patch =
    let p = debianize s flags in
    let (Debianize (Hackage s)) = P.spec p in
    p {P.spec = Debianize (Patch (Hackage s) patch)}
