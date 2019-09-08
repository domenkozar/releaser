module Releaser.Primitives (
  -- cabal utilities
    CabalInfo(..)
  , cabalRead
  , cabalWriteVersion
  , cabalBumpVersion
  , cabalSdist
  , cabalUpload
  -- git primitives
  , gitCheckout
  , gitGetTags
  , gitTag
  , gitCommit
  , gitPush
  , gitAssertEmptyStaging
  -- utilities
  , prompt
  , logStep
  , changelogPrepare
  ) where

import System.IO 
import System.Process
import System.Console.Pretty (Color(..), color)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import Text.Regex.PCRE
import Data.Functor (void)
import Data.List (intercalate)
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Data.Version (parseVersion)
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity (silent)
import Distribution.Types.PackageId (pkgVersion, pkgName)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.GenericPackageDescription (packageDescription)
import Distribution.PackageDescription.PrettyPrint (writeGenericPackageDescription)
import Distribution.Types.Version (versionNumbers, mkVersion')
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.Types.PackageName (unPackageName)

logStep :: String -> IO ()
logStep txt = 
  putStrLn $ color Yellow ">> " <> txt

prompt :: String -> IO String
prompt str = do
    putStr str
    hFlush stdout
    getLine  

data CabalInfo = CabalInfo 
  { name :: String
  , version :: String
  }

-- | Given a folder, find a Cabal file and read the package version
cabalRead :: FilePath -> IO CabalInfo
cabalRead dir = do
  logStep $ "Looking for a cabal file in " <> dir
  cabalFile <- tryFindPackageDesc dir
  genericPackageDescription <- readGenericPackageDescription silent cabalFile
  let pkgversion = pkgVersion $ package $ packageDescription genericPackageDescription
      pkgname = pkgName $ package $ packageDescription genericPackageDescription
      cabalinfo = CabalInfo 
        { version = intercalate "." $ show <$> versionNumbers pkgversion
        , name = unPackageName pkgname
        }
  logStep $ "Found " <> name cabalinfo <> "-" <> version cabalinfo
  return cabalinfo

-- | Given a folder, find a Cabal file and update the package version
cabalWriteVersion :: FilePath -> String -> IO ()
cabalWriteVersion dir versionStr = do
  cabalFile <- tryFindPackageDesc dir
  genericPackageDescription <- readGenericPackageDescription silent cabalFile
  let
    pd = packageDescription genericPackageDescription
    p = package $ packageDescription genericPackageDescription
    gpd = genericPackageDescription { packageDescription = pd { package = p { pkgVersion = version } } }
  writeGenericPackageDescription cabalFile gpd
  logStep $ "Bumped " <> unPackageName (pkgName p) <> " to " <> versionStr
  where
    -- TODO: handle the read failure nicely
    version = mkVersion' $ case parseMaybe parseVersion versionStr of
      Nothing -> error "parsing the cabal version failed"
      Just ver -> ver

    parseMaybe :: ReadP a -> String -> Maybe a
    parseMaybe parser input =
      case readP_to_S parser input of
          [] -> Nothing
          xs -> Just $ fst (last xs)

validCabalVersion :: String -> Bool
validCabalVersion version =
  version =~ "^[0-9]+([.][0-9]+)*$"

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

cabalBumpVersion :: FilePath -> IO String
cabalBumpVersion dir = do
  cabalinfo <- cabalRead dir
  version <- prompt $ "Bump cabal version from " <> version cabalinfo <> " to: "
  if validCabalVersion version
  then do 
    cabalWriteVersion dir version
    return version
  else do
    putStrLnErr "Cabal version does not match /^[0-9]+([.][0-9]+)*$/. Try again."
    cabalBumpVersion dir

cabalSdist :: FilePath -> IO FilePath
cabalSdist dir = do
  logStep "Running $ cabal dist"
  cabalinfo <- cabalRead dir
  void $ readProcess "cabal" ["sdist"] mempty
  let sdistTarball = "dist/" <> name cabalinfo <> "-" <> version cabalinfo <> ".tar.gz"
  logStep $ "Created " <> sdistTarball
  return sdistTarball

cabalUpload :: FilePath -> IO ()
cabalUpload sdistTarball = do
  logStep "Running $ cabal upload"
  -- TODO: assert that credentials are configured via netrc
  void $ readProcess "cabal" ["upload", sdistTarball] mempty
    
gitGetTags :: IO [String]
gitGetTags = do
  lines <$> readProcess "git" ["tag"] mempty

-- TODO: what can we do if previous release process terminated and branch exists?
gitCheckout :: String -> IO ()
gitCheckout tag = do
  logStep $ "Running $ git checkout -b " <> tag
  tags <- gitGetTags
  if elem tag tags
  then error "git branch already exists, please delete it to start over"
  else void $ readProcess "git" ["checkout", "-b", tag] mempty

gitTag :: String -> IO ()
gitTag tag = do
  logStep $ "Running $ git tag --annotate --sign" <> tag
  tags <- gitGetTags
  if elem tag tags
  then error "git tag already exists, please delete it and start over"
  else void $ readProcess "git" ["tag", "--annotate", "--sign", tag] mempty

gitCommit :: String -> IO ()
gitCommit message = do
  logStep $ "Running $ git commit "
  void $ readProcess "git" ["commit", "-m", message] mempty

gitPush :: String -> IO ()
gitPush remote = do
  logStep $ "Pushing git to " <> remote
  void $ readProcess "git" ["push", remote, "HEAD"] mempty

gitAssertEmptyStaging :: IO ()
gitAssertEmptyStaging = do
  logStep "Assserting there are no uncommitted files"
  output <- readProcess "git" ["status", "--untracked-files=no", "--porcelain"] mempty
  if output == ""
  then return ()
  else error "git status is not clean"

changelogPrepare :: String -> IO ()
changelogPrepare file = do
  logStep "Assserting there are no uncommitted files"
  editorEnv <- lookupEnv "EDITOR"
  case editorEnv of
    Nothing -> error "please make sure $EDITOR is set"
    Just editor -> do
      -- TODO: prepare the changelog
      (_, _, _, ph) <- createProcess (proc editor ["CHANGELOG.md"])
      exitcode <- waitForProcess ph
      case exitcode of
        ExitSuccess -> return ()
        ExitFailure i -> do
          logStep $ editor <> " failed with " <> show i <> ", retrying"
          changelogPrepare file