module Releaser.Primitives (
  -- cabal utilities
    CabalInfo(..)
  , cabalRead
  , cabalWriteVersion
  , cabalBumpVersion
  , cabalSdist
  , cabalUpload
  , cabalMakeHaddocks
  , cabalUploadDocs
  -- git primitives
  , gitCheckout
  , gitGetTags
  , gitTag
  , gitCommit
  , gitPush
  , gitPushTags
  , gitAssertEmptyStaging
  -- utilities
  , prompt
  , abort
  , logStep
  , changelogPrepare
  ) where

import System.IO 
import System.Process
import System.Console.Pretty (Color(..), color)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..), exitFailure)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text
import Data.Functor (void)
import Data.List (intercalate)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.ParserCombinators.ReadP (ReadP, readP_to_S)
import Data.Version (parseVersion)
import Distribution.PackageDescription.Parsec
import Distribution.Verbosity (silent)
import Distribution.Types.PackageId (pkgVersion, pkgName)
import Distribution.Types.PackageDescription (package)
import Distribution.Types.GenericPackageDescription (packageDescription)
import Distribution.Types.Version (versionNumbers, mkVersion')
import Distribution.Simple.Utils (tryFindPackageDesc)
import Distribution.Types.PackageName (unPackageName)

logStep :: String -> IO ()
logStep str = 
  putStrLn $ color Green ">> " <> str

prompt :: String -> IO String
prompt str = do
  putStr $ color Blue ">> " <> str
  hFlush stdout
  getLine  

promptRetry :: String -> IO ()
promptRetry str =
  void $ prompt $ str <> ". Retry? (press enter) "

abort :: String -> IO a
abort str = do
  putStrLnErr $ color Red ">> " <> str
  exitFailure

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
  if validCabalVersion versionStr
  then do
    cabalFile <- tryFindPackageDesc dir
    cabalinfo <- cabalRead dir
    cabal <- T.readFile cabalFile
    let versionPrev :: T.Text
        versionPrev = cabal =~ ("version:[ \t]*" ++ version cabalinfo)
    if versionPrev == ""
    then abort $ "Failed to replace version in " <> cabalFile <> ", please open an issue at https://github.com/domenkozar/releaser/issues"
    else do
      T.writeFile cabalFile $ T.replace versionPrev ("version: " <> T.pack versionStr) cabal
      logStep $ "Bumped " <> name cabalinfo <> " to " <> versionStr
  else do
    promptRetry "Cabal version does not match /^[0-9]+([.][0-9]+)*$/"
    void $ cabalBumpVersion dir
    

validCabalVersion :: String -> Bool
validCabalVersion version =
  version =~ ("^[0-9]+([.][0-9]+)*$" :: String)

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

cabalBumpVersion :: FilePath -> IO String
cabalBumpVersion dir = do
  cabalinfo <- cabalRead dir
  version <- prompt $ "Bump cabal version from " <> version cabalinfo <> " to: "
  cabalWriteVersion dir version
  return version

cabalSdist :: FilePath -> IO FilePath
cabalSdist dir = do
  logStep "Running $ cabal dist"
  cabalinfo <- cabalRead dir
  void $ readProcess "cabal" ["v2-sdist"] mempty
  let sdistTarball = "dist-newstyle/sdist/" <> name cabalinfo <> "-" <> version cabalinfo <> ".tar.gz"
  logStep $ "Created " <> sdistTarball
  return sdistTarball

cabalMakeHaddocks :: FilePath -> IO FilePath
cabalMakeHaddocks dir = do
  logStep "Running $ cabal haddock"
  cabalinfo <- cabalRead dir
  void $ readProcess "cabal" ["v2-haddock", "--haddock-for-hackage"] mempty
  let docsTarball = "dist-newstyle/" <> name cabalinfo <> "-" <> version cabalinfo <> "-docs.tar.gz"
  logStep $ "Created " <> docsTarball
  return docsTarball

cabalUploadDocs :: FilePath -> IO ()
cabalUploadDocs docsTarball = do
  logStep "Running $ cabal upload -d"
  -- TODO: recommend that credentials are configured via ~/cabal/config
  interactiveProcess (proc "cabal" ["upload", "-d", "--publish", docsTarball]) $ \_ -> do
    promptRetry "cabal upload -d"
    cabalUploadDocs docsTarball

cabalUpload :: FilePath -> IO ()
cabalUpload sdistTarball = do
  logStep "Running $ cabal upload"
  -- TODO: recommend that credentials are configured via ~/cabal/config
  interactiveProcess (proc "cabal" ["upload", "--publish", sdistTarball]) $ \_ -> do
    promptRetry "cabal upload"
    cabalUpload sdistTarball
    
gitGetTags :: IO [String]
gitGetTags = do
  lines <$> readProcess "git" ["tag"] mempty

-- TODO: what can we do if previous release process terminated and branch exists?
gitCheckout :: String -> IO ()
gitCheckout tag = do
  logStep $ "Running $ git checkout -b " <> tag
  -- TODO: check for existing branch
  interactiveProcess (proc "git" ["checkout", "-b", tag]) $ \i -> do 
    promptRetry "git checkout failed"
    gitCheckout tag

gitTag :: String -> IO ()
gitTag tag = do
  logStep $ "Running $ git tag --annotate --sign " <> tag
  tags <- gitGetTags
  if elem tag tags
  then abort "git tag already exists, please delete it and start over"
  else interactiveProcess (proc "git" ["tag", "--annotate", "--sign", tag]) $ \i -> do 
    promptRetry "git tag failed"
    gitTag tag

gitCommit :: String -> IO ()
gitCommit message = do
  logStep $ "Running $ git commit "
  interactiveProcess (proc "git" ["commit", "-a", "-m", message]) $ \i -> do 
    promptRetry "git commit failed"
    gitCommit message


gitPush :: String -> IO ()
gitPush remote = do
  logStep $ "Pushing git to " <> remote
  interactiveProcess (proc "git" ["push", remote, "HEAD"]) $ \i -> do 
    promptRetry "git push"
    gitPush remote

gitPushTags :: String -> IO ()
gitPushTags remote = do
  logStep $ "Pushing git tags to " <> remote
  void $ readProcess "git" ["push", remote, "--tags"] mempty

gitAssertEmptyStaging :: IO ()
gitAssertEmptyStaging = do
  logStep "Assserting there are no uncommitted files"
  output <- readProcess "git" ["status", "--untracked-files=no", "--porcelain"] mempty
  if output == ""
  then return ()
  else abort "git status is not clean"

changelogPrepare :: IO ()
changelogPrepare = do
  logStep "Assserting there are no uncommitted files"
  editorEnv <- lookupEnv "EDITOR"
  case editorEnv of
    Nothing -> abort "please make sure $EDITOR is set"
    Just editor -> do
      -- TODO: prepare the changelog
      interactiveProcess (proc editor ["CHANGELOG.md"]) $ \i -> do
        logStep $ editor <> " failed with " <> show i <> ", retrying"
        changelogPrepare

-- internal

interactiveProcess :: CreateProcess -> (Int -> IO ()) -> IO ()
interactiveProcess cmd bad = do
  (_, _, _, ph) <- createProcess cmd
  exitcode <- waitForProcess ph
  case exitcode of
    ExitSuccess -> return ()
    ExitFailure i -> bad i