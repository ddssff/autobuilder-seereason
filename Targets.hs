-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Targets 
    ( public
    , private
    ) where

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import qualified Targets.Hackage as Hackage
import qualified Targets.Private as Private
import qualified Targets.SeeReason as SeeReason
import qualified Targets.Sid as Sid

-- |Each of theses lists can be built on their own as a group,
-- and any sequence of groups can be built together as long as
-- no intermediate group is omitted.  Comment out the ones you
-- don't wish to build.
public home release = {- Sid.ring0 home release ++ -} Sid.ring1 home release ++ Hackage.targets home release ++ SeeReason.targets home

private home = Private.libraries home ++ Private.applications home
