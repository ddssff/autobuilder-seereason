import System.Directory (getHomeDirectory)
import System.Environment
import System.FilePath
import Extra.Misc (md5sum)
import Data.Maybe (listToMaybe)
import Data.Either
import Data.List (elemIndices, splitAt)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import System.Unix.Process (lazyCommand, Output(..))
import Data.Char (toLower)
import Data.ByteString.UTF8 (toString)
import Language.Haskell.Exts.Parser (parseExp, fromParseResult)
import Language.Haskell.Exts.Syntax (Exp(..))
import Language.Haskell.Exts.Pretty (prettyPrint)

type SourcePackageName = String
type Version = String
type Checksum = String
type Distro = String

data URI = URI
data Target = Target { sourcePackageName :: SourcePackageName
                     , sourceSpec :: String
                     , relaxInfo :: [String]
                     } deriving Show

data SourceSpec = DebDir SourceSpec SourceSpec |
                  URISpec URI Checksum |
                  DarcsSpec URI |
                  AptSpec Distro SourcePackageName (Maybe Version) |
                  Quilt SourceSpec SourceSpec

type Config = (SourcePackageName, Version, Checksum, FilePath, FilePath)

processArgs :: [String] -> Either String (SourcePackageName, Maybe Version)
processArgs [] = Left usage
processArgs (s:[]) = Right (s, Nothing)
processArgs (s:v:[]) = Right (s, Just v)
processArgs _ = Left $ "error: too many arguments\n" ++ usage

usage = "usage: packageName [version]\n   for some package on hackage.haskell.org"

main =
  getArgs >>= return . processArgs >>= either error run
      where run (p, mv) = do
              v <- maybe (latestVersion p) return mv
              buildconfig p v >>= putStrLn . pp . target
              -- map (buildconfig home) . lines >>= mapM checksum >>= return . (map config) >>= mapM_ print

pp :: (Show a) => a -> String
pp = prettyPrint . fromParseResult . parseExp . show

buildconfig :: SourcePackageName -> Version -> IO Config
buildconfig packageName packageVersion =
    do
      home <- getHomeDirectory
      csum <- checksum (tp home)
      return (packageName, packageVersion, csum, (tp home), hURI)
    where
      hURI = hackageURI packageName packageVersion
      tp home = tarpath home packageName packageVersion

targetType "deb-dir" = Just DebDir
targetType "patchsys" = Just DebDir
targetType _ = Nothing

checksum :: FilePath -> IO Checksum
checksum fp =
    do
       eSum <- md5sum fp
       return $ either (error $ "checksum failed on filepath: " ++ fp) id eSum

latestVersion :: SourcePackageName -> IO Version
latestVersion package = do
    outputs <- lazyCommand (versionCmd package) (L.pack [])
    return (dropNL . toString . out . head $ outputs)
        where
          versionCmd p = "cabal info " ++ p ++ " | grep 'Latest version available' | sed -e 's/^.*: //'"
          out (Stdout s) = s
          out _ = error "latestVersion failed."
          dropNL = filter (/= '\n')

tarpath home packageName packageVersion = home </> ".cabal" </> "packages" </> "hackage.haskell.org" </> packageName </> packageVersion </> tarname packageName packageVersion


target :: Config -> Target
target (packageName, packageVersion, checksum, tarpath, hackageURL) = 
    Target { sourcePackageName = "haskell-" ++ debname
           , sourceSpec = "deb-dir:" ++ (parens $ uri (hackageURI packageName packageVersion) checksum) ++ ":" ++ (parens $ "darcs:" ++ "http://src.seereason.com/haskell-" ++ debname ++ "-debian")
           , relaxInfo = []
           }
        where debname = map toLower packageName

parens s = "(" ++ s ++ ")"
uri u csum = "uri:" ++ u  ++ ":" ++ csum

hackageURI p v =
    "http://hackage.haskell.org/" ++ ("packages" </> "archive" </> p </> v </>  (tarname p v))

tarname :: String -> String -> String
tarname p v = p ++ "-" ++ v ++ targz
    where
      targz = ".tar.gz"

splitVersion :: String -> (String, String)
splitVersion s =
    case lastdash s of
      Nothing -> error $ "Expected a dash in packagename-version string: " ++ s
      Just i -> (\(u,v) -> (u, tail v)) $ splitAt i s

lastdash :: String -> Maybe Int
lastdash s =
    listToMaybe $ reverse dashes -- Get the last match, if it exists.
        where
          dashes = elemIndices dash s
          dash = '-'

