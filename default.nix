{ compiler ? null
, pkgs ? import <nixpkgs> {}
}:

let
  haskellPackages =
    if builtins.isNull compiler
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  overriddenPackages = haskellPackages.override {
    overrides = self: super: {
      habitica-hs = self.callPackage ./lib/habitica-hs {};
    }; 
  };
in
  overriddenPackages.callCabal2nix "habitica-party-bot" ./. {}
