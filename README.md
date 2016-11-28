# hfish

## Dependencies 

There are tons of dependencies, they are listed in the [cabal file][hfish-cabal-file].

 [hfish-cabal-file]: https://github.com/xaverdh/hfish/blob/master/hfish.cabal

## Building

For now I recommend doing the following in a fresh directory (this assumes that you have all dependencies installed):

```sh
git clone git://github.com/xaverdh/hfish-parser
git clone git://github.com/xaverdh/hfish
cabal sandbox init
cabal sandbox add-source hfish-parser
cd hfish
make
```
