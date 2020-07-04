let
  pkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/20.03.tar.gz;
    sha256 = "0182ys095dfx02vl2a20j1hz92dx3mfgz2a6fhn31bqlp1wa8hlq";
  }) {};
  oldPkgs = import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/19.09.tar.gz;
    sha256 = "0mhqhq21y5vrr1f30qd2bvydv4bbbslvyzclhw0kdxmkgg3z4c92";
  }) {};
  drv = import ./. { inherit pkgs; };
in
  pkgs.haskellPackages.shellFor {
    packages = p: [drv];
    buildInputs = with pkgs.haskellPackages; [
      cabal-install
      ghcid
      hlint
      oldPkgs.stylish-haskell # stylish-haskell is marked "broken" in newer versions of nixpkgs
    ];
  }
