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

-- |Make sure the targets are in alphabetical order by debian source package name.
checkOrder ts =
    check ts
    where
      check (a : b : more) =
          case (compare `on` P.name) a b of
            GT -> error $ "Misordered targets: " ++ P.name a ++ " precedes " ++ P.name b
            _ -> check (b : more)
      check _ = ts

checkUnique :: P.Packages -> P.Packages
checkUnique ts =
    case P.foldPackages (\ name spec flags (mp, errs) ->
                              case Map.lookup name mp of
                                Nothing ->
                                    (Map.insert name (spec, flags) mp, errs)
                                Just (spec', flags') ->
                                    (mp, ("Duplicate targets for package " ++ name ++
                                          ":\n " ++ show spec ++ ", flags=" ++ show flags ++ 
                                          "\n " ++ show spec' ++ ", flags=" ++ show flags') : errs)) (Map.empty, []) ts of
      (_, []) -> ts
      (_, errs) -> error (unlines errs)
{-    
    check $ sortBy (compare `on` P.name) $ ts
    where
      check (a : b : more) =
          case (compare `on` P.name) a b of
            EQ -> error $ "Duplicate targets:\n " ++ show a ++ "\n " ++ show b
            _ -> check (b : more)
      check _ = ts
-}