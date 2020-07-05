{ compiler ? null
, pkgs ? import <nixpkgs> {}
}:

let
  haskellPackages =
    if builtins.isNull compiler
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
in
  haskellPackages.callCabal2nix "habitica-party-bot" ./. {}
