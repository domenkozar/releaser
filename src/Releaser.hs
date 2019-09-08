module Releaser
  ( readCabalVersion
  , writeCabalVersion
  ) where

import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Data.Version (parseVersion)
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity (silent)
import Distribution.Types.PackageId (pkgVersion)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.GenericPackageDescription (packageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Types.Version (versionNumbers, mkVersion')
import Distribution.Simple.Utils (tryFindPackageDesc)


-- | Given a folder, find a Cabal file and read the package version
readCabalVersion :: FilePath -> IO String
readCabalVersion filepath = do
  cabalFile <- tryFindPackageDesc filepath
  genericPackageDescription <- readGenericPackageDescription silent cabalFile
  let version = pkgVersion $ package $ packageDescription genericPackageDescription
  return $ intercalate "." $ show <$> versionNumbers version

-- | Given a folder, find a Cabal file and update the package version
writeCabalVersion :: FilePath -> String -> IO ()
writeCabalVersion filepath versionStr = do
  cabalFile <- tryFindPackageDesc filepath
  genericPackageDescription <- readGenericPackageDescription silent cabalFile
  let
    pd = packageDescription genericPackageDescription
    p = package $ packageDescription genericPackageDescription
    gpd = genericPackageDescription { packageDescription = pd { package = p { pkgVersion = version } } }
  writeGenericPackageDescription cabalFile gpd
  where
    -- TODO: handle the read failure nicely
    version = mkVersion' $ case parseMaybe parseVersion versionStr of
      Nothing -> error "parsing the version"
      Just a -> a

    parseMaybe :: ReadP a -> String -> Maybe a
    parseMaybe parser input =
      case readP_to_S parser input of
          [] -> Nothing
          xs -> Just $ fst (last xs)


--
-- git checkout version
-- git commit
-- git tag
