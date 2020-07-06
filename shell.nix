let
  config = {
    # because haskell-discord is marked as broken
    allowBroken = true;
  };
  overlays = [
    (import ./nix/overlays.nix)
  ];
  pkgs = import ./nix/pinned-package-set.nix { inherit config overlays; };
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
