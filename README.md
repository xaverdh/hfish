# hfish

## What it is

This is the main repository of hfish, a reimplementation of the [fish shell][fish-shell] in haskell.
This is not, however, an exact replica of fish (that wouldn't be possible anyway, since to my knowledge there is no formal specification of the fish language).
Refer to the [Syntax](#syntax) section for details on the differences.
  [fish-shell]: https://github.com/fish-shell/fish-shell/

## Prerequisites and Dependencies
  * A unix / posix system (linux for example)
  * A recent cabal / ghc
  * The icu (International Components for Unicode) library
  * Tons of haskell library dependencies (cabal install should take care of them), they are listed in the [cabal file][hfish-cabal-file].

 [hfish-cabal-file]: https://gitlab.com/hfish-shell/hfish/blob/master/hfish.cabal

## Building

For now I recommend doing the following in a fresh directory:

```sh
git clone https://gitlab.com/hfish-shell/posix-fd-io
git clone https://gitlab.com/hfish-shell/nfc-text
git clone https://gitlab.com/hfish-shell/hfish
git clone https://gitlab.com/hfish-shell/hfish-parser
git clone https://gitlab.com/hfish-shell/fish-parser
git clone https://gitlab.com/hfish-shell/fish-lang
cd hfish
cabal sandbox init
cabal sandbox add-source ../posix-fd-io ../nfc-text ../fish-lang ../fish-parser ../hfish-parser
cabal install --ghc-option=-threaded
```

There is a build script containing theses lines, so equivalently (assuming you have wget) you could do:
```sh
wget -O - https://gitlab.com/hfish-shell/hfish/blob/master/build | sh
```

Build time from scratch is ~10min on a i5-4200U with -j4 so I suggest that you get yourself a cup of tea.

## Syntax

TODO
