module Main (main) where

import Releaser.Primitives
import System.IO (hSetBuffering, stdout, stderr, BufferMode(..))

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering

  -- prepare release
  -- TODO: pass a list directories
  gitAssertEmptyStaging
  version <- cabalBumpVersion "."
  let release = "v" <> version
  changelogPrepare

  -- make release
  tarball <- cabalSdist "."
  gitCommit $ "Bump to " <> release
  gitTag release

  -- syncing github/hackage
  gitPush "origin"
  gitPushTags "origin"
  cabalUpload tarball

  -- make haddocks and upload them
  tarball <- cabalMakeHaddocks "."
  cabalUploadDocs tarball
