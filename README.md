# Releaser

Automation of Haskell package release process.

## Usage

- `Releaser.Primitives` provides a bunch of functions
  to help automating release process.

- `releaser` executable provides interactive automation
  for releasing package to Hackage and git.

## Design decisions

- When there's a choice for configuration, prefer a convention and avoid configuration.

- Log each primitive action

## Prior art

- [bgamari/hs-maintainer-tools](https://github.com/bgamari/hs-maintainer-tools)
