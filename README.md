# hfish

## Dependencies
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

Build time from scratch is ~10min so I suggest that you get yourself a cup of tea.
