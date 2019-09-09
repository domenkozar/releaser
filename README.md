# Releaser

Automation of Haskell package release process.

## Example usage

```
$ releaser
>> Assserting there are no uncommitted files
>> Looking for a cabal file in .
>> Found releaser-0.1.0.0
>> Bump cabal version from 0.1.0.0 to: 0.1.0.0
>> Bumped releaser to 0.1.0.0
>> Running $ git checkout -b v0.1.0.0
Switched to a new branch 'v0.1.0.0'
>> Assserting there are no uncommitted files
>> Running $ cabal dist
>> Looking for a cabal file in .
>> Found releaser-0.1.0.0
Warning: The sdist command is a part of the legacy v1 style of cabal usage.

Please switch to using either the new project style and the new-sdist command
or the legacy v1-sdist alias as new-style projects will become the default in
the next version of cabal-install. Please file a bug if you cannot replicate a
working v1- use case with the new-style commands.

For more information, see: https://wiki.haskell.org/Cabal/NewBuild

>> Created dist/releaser-0.1.0.0.tar.gz
>> Running $ git commit 
>> Running $ git tag --annotate --sign v0.1.0.0
>> Pushing git to origin
Enumerating objects: 25, done.
Counting objects: 100% (25/25), done.
Delta compression using up to 8 threads
Compressing objects: 100% (13/13), done.
Writing objects: 100% (19/19), 3.37 KiB | 3.37 MiB/s, done.
Total 19 (delta 8), reused 0 (delta 0)
remote: Resolving deltas: 100% (8/8), completed with 3 local objects.
remote: 
remote: Create a pull request for 'v0.1.0.0' on GitHub by visiting:
remote:      https://github.com/domenkozar/releaser/pull/new/v0.1.0.0
remote: 
To https://github.com/domenkozar/releaser.git
 * [new branch]      HEAD -> v0.1.0.0
>> Pushing git to origin
Enumerating objects: 1, done.
Counting objects: 100% (1/1), done.
Writing objects: 100% (1/1), 540 bytes | 540.00 KiB/s, done.
Total 1 (delta 0), reused 0 (delta 0)
To https://github.com/domenkozar/releaser.git
 * [new tag]         v0.1.0.0 -> v0.1.0.0
>> Running $ cabal upload
ploading dist/releaser-0.1.0.0.tar.gz...
Package successfully published. You can now view it at
'https://hackage.haskell.org/package/releaser-0.1.0.0'.
```

## Features

- `Releaser.Primitives` module provides a bunch of functions
  to help automating release process.

- `releaser` executable provides interactive automation
  for releasing package to Hackage and git.

## Design decisions

- When there's a choice for configuration, prefer a convention and avoid configuration.

- Log each primitive action

## Prior art

- [bgamari/hs-maintainer-tools](https://github.com/bgamari/hs-maintainer-tools)
