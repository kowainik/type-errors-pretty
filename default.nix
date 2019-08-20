{ # The git revision here corresponds to the nixpkgs-unstable channel, which at
  # the time of this writing has GHC 8.6.5 as the default compiler (matching the
  # one used by stack.yaml). Use https://howoldis.herokuapp.com/ to determine
  # the current rev.
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/002b853782e.tar.gz") {}
  # Which GHC compiler to use.
  # To determine the list of compilers available run:
  #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
, compiler ? "default"
}:
let
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
in
haskellPackages.developPackage {
  # The path to our cabal project's root directory
  root = ./.;
}
