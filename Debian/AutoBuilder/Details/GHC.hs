-- | Collect all the cruft that has grown up around the different
-- versions of GHC.
module Debian.AutoBuilder.Details.GHC
    ( myCompilerVersion
    , ghc
    ) where

import Data.Maybe (fromMaybe)
import Debian.AutoBuilder.Details.Distros (Release, baseRelease, BaseRelease(..))

-- | Unfortunately, we need to tell the autobuilder what version of ghc
-- is going to be in our release so that cabal-debian knows what packages
-- are bundled with the compiler.  If a compiler version is assigned here
-- it must be known to the cabal-debian library installed on the machine
-- on which the autobuilder is running.  If the result is Nothing it
-- assumes the same compiler is used in the build environment as in the
-- parent environment.
myCompilerVersion :: Release -> Maybe String
myCompilerVersion release =
    case ghc release of
      704 -> Just "7.4.1"
      706 -> Just "7.6.1"
      708 -> Just "7.8.1"
      _ -> Nothing

ghc :: Release -> Int
ghc release =
    fromMaybe 706 (lookup (baseRelease release) alist)
    where
      alist
          = [ (Quantal, 706)
            , (Natty, 704)
            , (Natty, 704)
            , (Lucid, 704)
            , (Lenny, 704)
            , (Wheezy, 708)
            , (Jessie, 708)
            , (Precise, 708)
            , (Trusty, 708) ]
