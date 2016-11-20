# hfish

## Building

For now I recommend doing this in a fresh directory:

```sh
git clone git://github.com/xaverdh/fish-parser
git clone git://github.com/xaverdh/hfish
cabal sandbox init
cabal sandbox add-source fish-parser
cd hfish
make
```
