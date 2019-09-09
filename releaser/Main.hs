module Main (main) where

import Releaser.Primitives


main :: IO ()
main = do
  gitAssertEmptyStaging

  -- prepare release
  -- TODO: pass a list directories
  version <- cabalBumpVersion "."
  let release = "v" <> version
  gitCheckout release
  changelogPrepare

  -- make release
  tarball <- cabalSdist "."
  gitCommit $ "Bump to " <> release
  gitTag release

  -- syncing github/hackage
  gitPush "origin"
  gitPushTags "origin"
  cabalUpload tarball