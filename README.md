# hfish

## What it is

This is the main repository of hfish, a reimplementation of the [fish shell][fish-shell] in haskell.
It is not, however, an exact copy of fish (that wouldn't be possible anyway, since to my knowledge there is no formal specification of the fish language).
Refer to the Syntax section for details on the differences.
  [fish-shell]: https://github.com/fish-shell/fish-shell/

## Prerequisites and Dependencies
  * A unix / posix system (linux for example)
  * A recent cabal / ghc
  * The icu (International Components for Unicode) library
  * Tons of haskell library dependencies (cabal install should take care of them), they are listed in the [cabal file][hfish-cabal-file].

 [hfish-cabal-file]: https://github.com/xaverdh/hfish/blob/master/hfish.cabal

## Building

For now I recommend doing the following in a fresh directory:

```sh
git clone git://github.com/xaverdh/hfish
git clone git://github.com/xaverdh/hfish-parser
git clone git://github.com/xaverdh/fish-parser
git clone git://github.com/xaverdh/fish-lang
cd hfish
cabal sandbox init
cabal sandbox add-source ../fish-lang ../fish-parser ../hfish-parser
cabal install
```

Build time from scratch is ~10min on a i5-4200U with -j4 so I suggest that you get yourself a cup of tea.

## Syntax

TODO
