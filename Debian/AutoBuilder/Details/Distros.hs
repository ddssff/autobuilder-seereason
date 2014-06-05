-- | Specific information about support distributions and releases
-- thereof.
module Debian.AutoBuilder.Details.Distros
    ( BaseRelease(..)
    , baseReleaseDistro
    , baseReleaseString
    , allReleases
    , Release(..)
    , Distro(..)
    , releaseName
    , releaseString
    , baseRelease
    , distroString
    , parseReleaseName
    , isPrivateRelease
    ) where

import Data.Char (toLower)
-- The ReleaseName type from the debian library - just a newtyped string.
import Debian.Release (ReleaseName(ReleaseName))

data BaseRelease =
    -- Debian releases
      Sarge -- 2005/6
    | Etch -- 2007/4
    | Lenny -- 2009/2
    | Squeeze -- 2011/2
    | Wheezy -- 2013/5
    | Jessie -- unreleased as of 2014/6
    | Sid -- always current
    | Experimental -- always current
    -- Ubuntu releases
    | Dapper -- 2006/10
    | Edgy -- 2007/4
    | Feisty -- 2007/10
    | Hardy -- 2008/4
    | Intrepid -- 2008/10
    | Jaunty -- 2009/4
    | Karmic -- 2009/10
    | Lucid -- 2010/4
    | Maverick -- 2010/10
    | Natty -- 2011/4
    | Oneiric -- 2011/10
    | Precise -- 2012/4
    | Quantal -- 2012/10
    | Raring -- 2013/4
    | Saucy -- 2013/10
    | Trusty -- 2014/4
    | Utopic -- 2014/10
    deriving (Eq, Show, Enum, Bounded)

allReleases :: [BaseRelease]
allReleases = [minBound .. maxBound]

debianReleases = [Sarge, Etch, Lenny, Squeeze, Wheezy, Jessie, Sid, Experimental]
ubuntuReleases = [Dapper, Edgy, Feisty, Hardy, Intrepid, Jaunty, Karmic, Lucid,
                  Maverick, Natty, Oneiric, Precise, Quantal, Raring, Saucy, Trusty, Utopic]

-- | A Distro is any organization that provides packages.
data Distro = Ubuntu | Debian | Kanotix | SeeReason deriving (Eq, Show)

data Release
    = Release BaseRelease
    | ExtendedRelease Release Distro
    -- ^ A release which is based on another release, known as the
    -- base release.  Thus, this release contains packages which can
    -- be installed on a machine running the base release.
    | PrivateRelease Release
    -- ^ A private release based on another release.
    deriving (Eq, Show)

releaseDistro :: Release -> Distro
releaseDistro (ExtendedRelease _ distro) = distro
releaseDistro (PrivateRelease release) = releaseDistro release
releaseDistro (Release release) = baseReleaseDistro release

baseReleaseDistro :: BaseRelease -> Distro
baseReleaseDistro Dapper = Ubuntu
baseReleaseDistro Edgy = Ubuntu
baseReleaseDistro Feisty = Ubuntu
baseReleaseDistro Hardy = Ubuntu
baseReleaseDistro Intrepid = Ubuntu
baseReleaseDistro Jaunty = Ubuntu
baseReleaseDistro Karmic = Ubuntu
baseReleaseDistro Lucid = Ubuntu
baseReleaseDistro Maverick = Ubuntu
baseReleaseDistro Natty = Ubuntu
baseReleaseDistro Oneiric = Ubuntu
baseReleaseDistro Precise = Ubuntu
baseReleaseDistro Quantal = Ubuntu
baseReleaseDistro Raring = Ubuntu
baseReleaseDistro Saucy = Ubuntu
baseReleaseDistro Trusty = Ubuntu
baseReleaseDistro Utopic = Ubuntu
baseReleaseDistro Sarge = Debian
baseReleaseDistro Etch = Debian
baseReleaseDistro Lenny = Debian
baseReleaseDistro Squeeze = Debian
baseReleaseDistro Wheezy = Debian
baseReleaseDistro Jessie = Debian
baseReleaseDistro Sid = Debian
baseReleaseDistro Experimental = Debian

releaseName :: Release -> ReleaseName
releaseName = ReleaseName . releaseString

releaseString :: Release -> String
releaseString (PrivateRelease r) = releaseString r ++ "-private"
releaseString (ExtendedRelease r vendor) = releaseString r ++ "-" ++ distroString vendor
releaseString (Release name) = baseReleaseString name

baseReleaseString :: BaseRelease -> String
baseReleaseString name = map toLower (show name)

distroString :: Distro -> String
distroString = map toLower . show

baseRelease :: Release -> BaseRelease
baseRelease (PrivateRelease release) = baseRelease release
baseRelease (ExtendedRelease release _) = baseRelease release
baseRelease (Release release) = release

parseReleaseName :: ReleaseName -> Release
parseReleaseName (ReleaseName s) =
    parse s
    where
      parse "sarge" = Release Sarge
      parse "etch" = Release Etch
      parse "lenny" = Release Lenny
      parse "squeeze" = Release Squeeze
      parse "wheezy" = Release Wheezy
      parse "jessie" = Release Jessie
      parse "sid" = Release Sid
      parse "experimental" = Release Experimental

      parse "dapper" = Release Dapper
      parse "edgy" = Release Edgy
      parse "feisty" = Release Feisty
      parse "hardy" = Release Hardy
      parse "intrepid" = Release Intrepid
      parse "jaunty" = Release Jaunty
      parse "karmic" = Release Karmic
      parse "lucid" = Release Lucid
      parse "maverick" = Release Maverick
      parse "natty" = Release Natty
      parse "oneiric" = Release Oneiric
      parse "precise" = Release Precise
      parse "quantal" = Release Quantal
      parse "raring" = Release Raring
      parse "saucy" = Release Saucy
      parse "trusty" = Release Trusty
      parse "utopic" = Release Utopic

      parse s =
          case viewSuffix "-private" s of
            Just prefix -> PrivateRelease (parse prefix)
            Nothing ->
                case viewSuffix "-seereason" s of
                  Just prefix -> ExtendedRelease (parse prefix) SeeReason
                  Nothing -> error $ "Unexpected release name: " ++ show s

isPrivateRelease (PrivateRelease _) = True
isPrivateRelease _ = False

viewSuffix :: String -> String -> Maybe String
viewSuffix suffix string =
    case splitAt (length string - length suffix) string of
      (pre, suf) | suf == suffix -> Just pre
      _ -> Nothing
