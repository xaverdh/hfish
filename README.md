# hfish

## Dependencies 

There are tons of dependencies, they are listed in the [cabal file][hfish-cabal-file].

 [hfish-cabal-file]: https://github.com/xaverdh/hfish/blob/master/hfish.cabal

## Building

For now I recommend doing this in a fresh directory (this assumes that you have all dependencies installed):

```sh
git clone git://github.com/xaverdh/fish-parser
git clone git://github.com/xaverdh/hfish
cabal sandbox init
cabal sandbox add-source fish-parser
cd hfish
make
```
