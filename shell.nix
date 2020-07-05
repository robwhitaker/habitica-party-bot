let
  config = {
    # because calamity and several of its dependencies are marked as broken
    allowBroken = true;
  };
  overlays = [
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = hself: hsuper: {
          habitica-hs = hself.callPackage ./lib/habitica-hs {};

          # Overridden with `doCheck = false;`

          # the tests on these two rely on HTF which fails to compile
          # on GHC 8.8.3
          stm-containers = hself.callPackage ./nix/stm-containers.nix {};
          list-t = hself.callPackage ./nix/list-t.nix {};

          # the tests on this just plain fail
          wreq-patchable = hself.callPackage ./nix/wreq-patchable.nix {};
        };
      };
    })
  ];
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/a5e211dd7f95167ab42066e82bfaa5a65971e67c.tar.gz;
    sha256 = "0f9279l5if8jc8gnyyb722sqssa5h0dszm3fhmj0ndg9d071wl8g";
  }) { inherit config overlays; };
  drv = import ./. { inherit pkgs; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      stylish-haskell
    ];
  }
