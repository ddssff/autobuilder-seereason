-- |This module defines how we obtain and assemble the source code for
-- the packages we want to build.
module Targets 
    ( public
    , private
    ) where

import Data.List (isPrefixOf)
import qualified Data.Set as Set
import Debian.AutoBuilder.ParamClass (Target(..))
import qualified Targets.Hackage as Hackage
import qualified Targets.Private as Private
import qualified Targets.SeeReason as SeeReason
import qualified Targets.Sid as Sid

public home release =
    -- Each of theses lists can be built on their own as a group,
    -- and any sequence of groups can be built together as long as
    -- no intermediate group is omitted.  Comment out the ones you
    -- don't wish to build.
    -- Sid.ring0 home ++
    Sid.ring1 home ++
    Hackage.targets home ++
    SeeReason.targets home

private home = Private.libraries home ++ Private.applications home

--    , Target { sourcePackageName = "eclipse-clp"
--             , sourceSpec = "deb-dir:(uri:http://eclipseclp.org/Distribution/6.0_160/src/eclipse_src.tgz:75d074bf0ee66948e6afd3b69e51e81e):(darcs:http://src.seereason.com/eclipse-clp-debian)"
--             , relaxInfo = [] }
--  , Target { sourcePackageName = "haskell-binary"
--             , sourceSpec = "quilt:(apt:sid:haskell-binary):(darcs:http://src.seereason.com/haskell-binary-quilt)"
--             , relaxInfo = [] } -}
--    , Target { sourcePackageName = "haskell-formlets"
--             , sourceSpec = "darcs:http://src.seereason.com/formlets"
--             , relaxInfo = [] }
--    , Target { sourcePackageName="haskell-logict"
--             , sourceSpec="deb-dir:(uri:http://hackage.haskell.org/packages/archive/logict/0.4/logict-0.4.tar.gz:39eeb4aa1d7a67b1c4865f01ca417b7d):(darcs:http://src.seereason.com/debian/haskell-logict-debian)"
--             , relaxInfo = [] }
--    , Target { sourcePackageName = "haskell-special-functors"
--      , sourceSpec =
--      "deb-dir:(uri:http://hackage.haskell.org/packages/archive/special-functors/1.0/special-functors-1.0.tar.gz:4547f0a1b4146d3621bcc95b11148939):(darcs:http://src.seereason.com/haskell-special-functors-debian)"
--      , relaxInfo = [] }
--    , Target { sourcePackageName = "tptp"
--             , sourceSpec = "deb-dir:(uri:http://www.cs.miami.edu/~tptp/TPTP/Distribution/TPTP-v4.1.0.tgz:3cffa92b1def9b8b9865f65d0b775b86):(darcs:http://src.seereason.com/tptp-debian)"
--             , relaxInfo = [] }
