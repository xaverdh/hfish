# Collection of hfish packages
{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, haskellPackages ? pkgs.haskellPackages
, doProfiling ? false
, doHaddock ? false
, lpath ? null
, local ? false
}:
let
  fetchGitL = { url ? null, lpath ? null, rev, sha256 ? null, local ? false }:
    if local then builtins.fetchGit { url = lpath; inherit rev; }
    else pkgs.fetchgit { inherit url rev sha256; };

  mkPackage = { name, remote, forceLocal ? false }:
    let src = fetchGitL {
        inherit (remote) url rev sha256;
        inherit lpath;
        local = forceLocal || local;
      };
    in args: with pkgs.haskell.lib; lib.pipe
      ( haskellPackages.callCabal2nix name src args )
      (
        lib.optionals (!doProfiling)
          [
            disableLibraryProfiling
            disableExecutableProfiling
          ]
        ++ lib.optional (!doHaddock) dontHaddock
      );

  src = import ./hfish-collection-sources.nix;

  hfish = mkPackage {
    name = "hfish";
    remote = src.hfish;
  } {
    inherit nfc-text posix-fd-io fish-lang
      fish-parser hfish-parser hfish-interpreter
      ;
  };

  hfish-interpreter = mkPackage {
    name = "hfish-interpreter";
    remote = src.hfish-interpreter;
  } {
    inherit nfc-text posix-fd-io fish-lang
      fish-parser hfish-parser
      ;
  };

  hfish-parser = mkPackage {
    name = "hfish-parser";
    remote = src.hfish-parser;
  } { inherit nfc-text fish-lang; };

  fish-parser = mkPackage {
    name = "fish-parser";
    remote = src.fish-parser;
  } { inherit nfc-text fish-lang; };

  fish-lang = mkPackage {
    name = "fish-lang";
    remote = src.fish-lang;
  } { inherit nfc-text; };

  nfc-text = mkPackage {
    name = "nfc-text";
    remote = src.nfc-text;
  } {};

  posix-fd-io = mkPackage {
    name = "posix-fd-io";
    remote = src.posix-fd-io;
  } {};

in
[
  posix-fd-io
  nfc-text
  fish-lang
  fish-parser
  hfish-parser
  hfish-interpreter
  hfish
]
