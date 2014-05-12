-- | Collect all the cruft that has grown up around the different
-- versions of GHC.
module Debian.AutoBuilder.Details.GHC
    ( myCompilerVersion
    , ghc
    ) where

import Data.Maybe (fromMaybe)

-- | Unfortunately, we need to tell the autobuilder what version of ghc
-- is going to be in our release so that cabal-debian knows what packages
-- are bundled with the compiler.  If a compiler version is assigned here
-- it must be known to the cabal-debian library installed on the machine
-- on which the autobuilder is running.  If the result is Nothing it
-- assumes the same compiler is used in the build environment as in the
-- parent environment.
myCompilerVersion release =
    case ghc release of
      704 -> Just "7.4.1"
      706 -> Just "7.6.1"
      708 -> Just "7.8.1"
      _ -> Nothing

ghc :: String -> Int
ghc release =
    fromMaybe 706 (lookup release alist)
    where
      alist = alistPublic ++ alistPrivate
      alistPublic
          = [ ("quantal-seereason", 706)
            , ("natty-seereason", 704)
            , ("natty-seereason", 704)
            , ("lucid-seereason", 704)
            , ("lenny-seereason", 704)
            , ("wheezy-seereason", 708)
            , ("jessie-seereason", 708)
            , ("precise-seereason", 708)
            , ("trusty-seereason", 708) ]
      alistPrivate = map (\ (s, n) -> (s ++ "-private", n)) alistPublic
