# hfish

## What it is

This is the main repository of hfish, a reimplementation of the [fish shell][fish-shell] in haskell.
This is not, however, an exact replica of fish (that wouldn't be possible anyway, since to my knowledge there is no formal specification of the fish language).
Refer to the [Syntax](#syntax) section for details on the differences.

## Prerequisites and Dependencies
  * A unix / posix system (linux for example)
  * A recent cabal / ghc
  * Tons of haskell library dependencies (cabal install should take care of them), they are listed in the [cabal file][hfish-cabal-file].


## Building

For now I recommend doing the following in a fresh directory:

```sh
git clone https://gitlab.com/hfish-shell/hfish
cd hfish
git clone https://gitlab.com/hfish-shell/posix-fd-io
git clone https://gitlab.com/hfish-shell/nfc-text
git clone https://gitlab.com/hfish-shell/hfish-interpreter
git clone https://gitlab.com/hfish-shell/hfish-parser
git clone https://gitlab.com/hfish-shell/fish-parser
git clone https://gitlab.com/hfish-shell/fish-lang
cabal sandbox init
cabal sandbox add-source posix-fd-io nfc-text fish-lang fish-parser hfish-parser hfish-interpreter
cabal install --ghc-option=-threaded
```

There is a [build script][hfish-build-script] containing theses lines, so equivalently (assuming you have wget) you could do:
```sh
wget -O - https://gitlab.com/hfish-shell/hfish/raw/master/build | sh
```

Build time from scratch is ~10min on a i5-4200U with -j4 so I suggest that you get yourself a cup of tea.

## Building on Arch Linux with [pristine ghc][ghc-pristine]

For Arch Linux users that want a statically linked executable, there is the [ghc-pristine build script][hfish-build-static-script].
This assumes you have ghc-static and ghc-pristine (AUR) installed.

## Syntax

TODO

## Bugs

Many. A non-exhaustive list can be found in the bug tracker.

## Legal
    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    any later version.
    
See [LICENSE](LICENSE) for more details.


[hfish-cabal-file]: https://gitlab.com/hfish-shell/hfish/blob/master/hfish.cabal

[hfish-build-script]: https://gitlab.com/hfish-shell/hfish/blob/master/build

[hfish-build-static-script]: https://gitlab.com/hfish-shell/hfish/blob/master/build-static

[fish-shell]: https://github.com/fish-shell/fish-shell/

[ghc-pristine]: https://aur.archlinux.org/packages/ghc-pristine/


